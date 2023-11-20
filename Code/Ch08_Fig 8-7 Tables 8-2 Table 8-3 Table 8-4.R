
########################
# Chap 9 deep learning: Code to create Figure 9.7 and Tables 9.2 and 9.3
########################
## Deep Learning for Forecasting
## Series with Seasonality (No Trend)


Amtrak.data <- read.csv("Data/Amtrak data.csv")

### Prepare training data for forecasting a series with LSTM ### 

# create time-series objects (from stats package)
ridership.ts <- ts(Amtrak.data$Ridership, start=c(1991, 1), freq=12)

# separate to training and testing datasets
nTest <- 36
nTrain <- length(ridership.ts) - nTest
train.ts <- window(ridership.ts, start=c(1991, 1), end=c(1991, nTrain))
test.ts <- window(ridership.ts, start=c(1991, nTrain + 1),
                  end=c(1991, nTrain + nTest))

# define function for normalization of training set and it's inverse
minmax <- range(train.ts)
normalize_ts <- function(x) (x - minmax[1]) / (minmax[2] - minmax[1])
inv_normalize_ts <- function(x) (minmax[2] - minmax[1]) * x + minmax[1]
norm_train.ts <- normalize_ts(train.ts)

# convert timeseries into sequence of subseries of length (ninput + noutput)
ninput <- 12
noutput <- 1
nSubsequences <- length(norm_train.ts) - (ninput + noutput) + 1
getSubsequence <- function(i) norm_train.ts[i:(i - 1 + ninput+noutput)]
subsequences <- t(sapply(1:nSubsequences, getSubsequence))

# split subsequences into input (x) and output (y) and convert both to arrays
x.train <- subsequences[, 1:ninput]
y.train <- subsequences[, (ninput+1):(ninput+noutput)]
x.train <- array(data=x.train, dim=c(nrow(x.train), ninput, 1))
y.train <- array(data=y.train, dim=c(nrow(x.train), noutput, 1))
dim(x.train); dim(y.train)


### Define LSTM model for forecasting ### 

# load required packages
# Keras and TensorFlow require a Python conda environment with these packages installed

library(reticulate)
library(devtools)
library(keras)

#You can confirm that the installation succeeded with:
install_keras(envname = "r-reticulate")

library(tensorflow)
tf$constant("Hello Tensorflow!")

# Seed Random Numbers with the TensorFlow Backend
set.seed(123)      # R
py_set_seed(1234)  # Python and numpy
set_random_seed(2) # Tensorflow

### Fit the NNT

lstm_model <- keras_model_sequential() %>%
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, ninput, 1), # batch size, timesteps, features
             dropout = 0.01,
             recurrent_dropout = 0.01,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_lstm(units = 50,
             dropout = 0.01,
             recurrent_dropout = 0.01,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_flatten() %>%
  layer_dense(units = 1)
summary(lstm_model)

lstm_model %>%
  compile(loss = 'mae', optimizer = 'adam', metrics = 'mse')


lstm_model %>% fit(
  x = x.train,
  y = y.train,
  batch_size = 1,
  epochs = 400,
  verbose = 1,
  shuffle = TRUE
)


lstm_model %>% save_model_weights_tf("lstm-model.ckpt")

lstm_model %>% load_model_weights_tf("lstm-model.ckpt")



# code for forecasting one month ahead with sliding window

forecast.ts <- c()
window <- norm_train.ts[(nTrain-12):nTrain]
for (i in 1:36) {
  x <- array(data=window, dim=c(1, ninput, 1))
  pred <- predict(lstm_model, x, batch_size=1)
  window <- c(window[2:length(window)], pred[1])
  forecast.ts <- c(forecast.ts, pred[1])
}

forecast.ts <- inv_normalize_ts(forecast.ts)
forecast.ts <- ts(forecast.ts, start=c(1991, 1 + nTrain), freq=12)

fitted <- predict(lstm_model, x.train, batch_size=1)
fitted.ts <- ts(inv_normalize_ts(fitted), start=c(1991, 1 + ninput), freq=12)

# to compute performance measures for ts object, install and load forecast library
library(forecast)
forecast::accuracy(fitted.ts, ridership.ts)
forecast::accuracy(forecast.ts, ridership.ts)
detach("package:forecast", unload = TRUE)

delta <- 1/12
date_t <- time(train.ts)[1]
date_th <- time(test.ts)[1] - delta
date_hf <- tail(time(test.ts), 1) + delta

# time plot of series with LSTM fitted values and forecasts

pdf("Plots/AmtrakFig_9_7_3e.pdf",height=4,width=6)
autoplot(train.ts, xlab="Time", ylab="Ridership", color= "black") +
  autolayer(test.ts, color="black") +
  autolayer(fitted.ts, color= "blue1", size=0.75) +
  autolayer(forecast.ts, color= "blue1", size=0.75 , linetype= "dashed") +
  scale_x_continuous(n.breaks=10) +
  geom_text(aes(x=(date_t+date_th)/2, y= 2250), label='Training', color="grey37")+  
  geom_text(aes(x=(date_th+date_hf)/2, y=2250), label='Validation', color="grey37") +
  geom_segment(aes(x=date_t, xend=date_th-delta, y= 2225, yend= 2225), color="darkgrey") +
  geom_segment(aes(x=date_th+delta, xend=date_hf-delta, y= 2225, yend= 2225), color="darkgrey")+
  geom_vline(xintercept=date_th, color="darkgrey")
dev.off() 
=======
########################
# Chap 9 deep learning: Code to create Figure 9.7 and Tables 9.2 and 9.3
########################
## Deep Learning for Forecasting
## Series with Seasonality (No Trend)


Amtrak.data <- read.csv("Data/Amtrak data.csv")

### Prepare training data for forecasting a series with LSTM ### 

# create time-series objects (from stats package)
ridership.ts <- ts(Amtrak.data$Ridership, start=c(1991, 1), freq=12)

# separate to training and testing datasets
nTest <- 36
nTrain <- length(ridership.ts) - nTest
train.ts <- window(ridership.ts, start=c(1991, 1), end=c(1991, nTrain))
test.ts <- window(ridership.ts, start=c(1991, nTrain + 1),
                  end=c(1991, nTrain + nTest))

# define function for normalization of training set and it's inverse
minmax <- range(train.ts)
normalize_ts <- function(x) (x - minmax[1]) / (minmax[2] - minmax[1])
inv_normalize_ts <- function(x) (minmax[2] - minmax[1]) * x + minmax[1]
norm_train.ts <- normalize_ts(train.ts)

# convert timeseries into sequence of subseries of length (ninput + noutput)
ninput <- 12
noutput <- 1
nSubsequences <- length(norm_train.ts) - (ninput + noutput) + 1
getSubsequence <- function(i) norm_train.ts[i:(i - 1 + ninput+noutput)]
subsequences <- t(sapply(1:nSubsequences, getSubsequence))

# split subsequences into input (x) and output (y) and convert both to arrays
x.train <- subsequences[, 1:ninput]
y.train <- subsequences[, (ninput+1):(ninput+noutput)]
x.train <- array(data=x.train, dim=c(nrow(x.train), ninput, 1))
y.train <- array(data=y.train, dim=c(nrow(x.train), noutput, 1))
dim(x.train); dim(y.train)


### Define LSTM model for forecasting ### 

# load required packages
# Keras and TensorFlow require a Python conda environment with these packages installed

library(reticulate)
library(devtools)
library(keras)

#You can confirm that the installation succeeded with:
install_keras(envname = "r-reticulate")

library(tensorflow)
tf$constant("Hello Tensorflow!")

# Seed Random Numbers with the TensorFlow Backend
set.seed(123)      # R
py_set_seed(1234)  # Python and numpy
set_random_seed(2) # Tensorflow

### Fit the NNT

lstm_model <- keras_model_sequential() %>%
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, ninput, 1), # batch size, timesteps, features
             dropout = 0.01,
             recurrent_dropout = 0.01,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_lstm(units = 50,
             dropout = 0.01,
             recurrent_dropout = 0.01,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_flatten() %>%
  layer_dense(units = 1)
summary(lstm_model)

lstm_model %>%
  compile(loss = 'mae', optimizer = 'adam', metrics = 'mse')


lstm_model %>% fit(
  x = x.train,
  y = y.train,
  batch_size = 1,
  epochs = 400,
  verbose = 1,
  shuffle = TRUE
)


lstm_model %>% save_model_weights_tf("lstm-model.ckpt")

lstm_model %>% load_model_weights_tf("lstm-model.ckpt")



# code for forecasting one month ahead with sliding window

forecast.ts <- c()
window <- norm_train.ts[(nTrain-12):nTrain]
for (i in 1:36) {
  x <- array(data=window, dim=c(1, ninput, 1))
  pred <- predict(lstm_model, x, batch_size=1)
  window <- c(window[2:length(window)], pred[1])
  forecast.ts <- c(forecast.ts, pred[1])
}

forecast.ts <- inv_normalize_ts(forecast.ts)
forecast.ts <- ts(forecast.ts, start=c(1991, 1 + nTrain), freq=12)

fitted <- predict(lstm_model, x.train, batch_size=1)
fitted.ts <- ts(inv_normalize_ts(fitted), start=c(1991, 1 + ninput), freq=12)

# to compute performance measures for ts object, install and load forecast library
library(forecast)
forecast::accuracy(fitted.ts, ridership.ts)
forecast::accuracy(forecast.ts, ridership.ts)
detach("package:forecast", unload = TRUE)

delta <- 1/12
date_t <- time(train.ts)[1]
date_th <- time(test.ts)[1] - delta
date_hf <- tail(time(test.ts), 1) + delta

# time plot of series with LSTM fitted values and forecasts

pdf("Plots/AmtrakFig_9_7_3e.pdf",height=4,width=6)
autoplot(train.ts, xlab="Time", ylab="Ridership", color= "black") +
  autolayer(test.ts, color="black") +
  autolayer(fitted.ts, color= "blue1", size=0.75) +
  autolayer(forecast.ts, color= "blue1", size=0.75 , linetype= "dashed") +
  scale_x_continuous(n.breaks=10) +
  geom_text(aes(x=(date_t+date_th)/2, y= 2250), label='Training', color="grey37")+  
  geom_text(aes(x=(date_th+date_hf)/2, y=2250), label='Validation', color="grey37") +
  geom_segment(aes(x=date_t, xend=date_th-delta, y= 2225, yend= 2225), color="darkgrey") +
  geom_segment(aes(x=date_th+delta, xend=date_hf-delta, y= 2225, yend= 2225), color="darkgrey")+
  geom_vline(xintercept=date_th, color="darkgrey")
dev.off() 
>>>>>>> .merge_file_dqp69d
