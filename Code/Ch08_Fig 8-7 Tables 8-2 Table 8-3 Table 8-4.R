
########################
# Chap 9 deep learning: Code for creating Figure 8.7 and Tables 8.2, 8.3, 8.4, 8.5
########################

# load required packages for deep learning
library(reticulate)
library(keras)
# You can confirm that the installation succeeded with:
install_keras(envname = "r-reticulate")
library(tensorflow)
tf$constant("Hello Tensorflow!")


Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

nValid <- 36
nTrain <- dim(ridership)[1] - nValid
train <- ridership |> filter(row_number() <= nTrain)
valid <- ridership |> filter(row_number() > nTrain)

# Normalization
minmax <- range(train$Ridership, na.rm = TRUE)
train <- train |>
  mutate(Normalized_Ridership = (Ridership - minmax[1]) / (minmax[2] - minmax[1]))

# Function to inverse the normalization
inv_normalize <- function(x) (minmax[2] - minmax[1]) * x + minmax[1]

# Creating sequences for LSTM
ninput <- 12
noutput <- 1

nSubsequences <- nrow(train) - (ninput + noutput) + 1
getSubsequence <- function(i) train$Normalized_Ridership[i:(i - 1 + ninput+noutput)]
subsequences <- t(sapply(1:nSubsequences, getSubsequence))

# split subsequences into input (x) and output (y) and convert both to arrays
x.train <- subsequences[, 1:ninput]
y.train <- subsequences[, (ninput+1):(ninput+noutput)]
x.train <- array(data=x.train, dim=c(nrow(x.train), ninput, 1))
y.train <- array(data=y.train, dim=c(nrow(x.train), noutput, 1))
dim(x.train); dim(y.train)



# Now start using deep learning

# Random seed numbers with the TensorFlow Backend
set.seed(123)      # R
py_set_seed(1234)  # Python and numpy
set_random_seed(2) # Tensorflow

lstm_model <- keras_model_sequential() |>
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, ninput, 1), # batch size, timesteps, features
             dropout = 0.01,
             recurrent_dropout = 0.01,
             return_sequences = TRUE,
             stateful = TRUE) |>
  layer_lstm(units = 50,
             dropout = 0.01,
             recurrent_dropout = 0.01,
             return_sequences = TRUE,
             stateful = TRUE) |>
  layer_flatten() |>
  layer_dense(units = 1)

summary(lstm_model)

lstm_model |>
  compile(loss = 'mae', optimizer = 'adam', metrics = 'mse')

lstm_model |> fit(
  x = x.train,
  y = y.train,
  batch_size = 1,
  epochs = 400,
  verbose = 1,
  shuffle = TRUE
)

# Forecasting one month ahead with sliding window 
window <- as.numeric(train$Normalized_Ridership[(nTrain-12):nTrain])
forecast <- numeric(nValid)

for (i in 1:nValid) {
  x <- array(data = window, dim = c(1, ninput, 1))
  pred <- predict(lstm_model, x, batch_size = 1)
  window <- c(window[-1], pred[1])  # Move the window forward by discarding the first element and appending the prediction
  forecast[i] <- pred[1]
}

# Inverse the normalization
forecast <- inv_normalize(forecast)

# Convert to tsibble
forecast_tbl <- tsibble(
  Forecast = forecast,
  Month = yearmonth(seq(as.Date("1991/01/01") + months(nTrain), by = "1 month", length.out = length(forecast))),
  index = Month
)
# For fitted values
fitted <- predict(lstm_model, x.train, batch_size = 1)
fitted_values <- inv_normalize(as.vector(fitted))
fitted_tbl <- tsibble(
  Fitted = fitted_values,
  Month = yearmonth(seq(as.Date("1991/01/01") + months(ninput), by = "1 month", length.out=length(fitted_values))),
  index = Month
)

# Plot 
pdf("AmtrakFig_9_7_3e.pdf",height=4,width=6)
  autoplot(ridership, Ridership) +
    autolayer(fitted_tbl, Fitted, color = "blue") +
    autolayer(forecast_tbl, Forecast, linetype = "dashed", color = "blue") +
    scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
    geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6)+
    geom_segment(aes(x = yearmonth("2001-May"), y = 2250, xend = yearmonth("2004-Mar"), yend = 2250),
                 arrow = arrow(length = unit(0.25, "cm"), ends = "both") , size = 0.3, color = "grey55")+ 
    annotate(geom = "text", x = yearmonth("2002-Aug"), y = 2280, label = "Validation", color = "grey37") +
    geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, xend = yearmonth("2001-Mar"), yend = 2250),
                 arrow = arrow(length = unit(0.25, "cm"), ends = "both"), size = 0.3, color = "grey55")+ 
    annotate(geom="text", x = yearmonth("1996-Aug"), y = 2280, label = "Training", color = "grey37")
dev.off() 
  

# Accuracy measures (cannot use fable because do not have a fable model)
train_all <- train |>
  left_join(fitted_tbl, by = "Month") |>
  mutate(error = Ridership - Fitted)

valid_all <- valid |>
  left_join(forecast_tbl, by = "Month") |>
  mutate(forecast_error = Ridership - Forecast)


library(forecast)
forecast::accuracy(train_all$Fitted, train_all$Ridership)
forecast::accuracy(forecast_tbl$Forecast, valid$Ridership)
detach("package:forecast", unload = TRUE) # to remove forecast package
