################
# This code calculates the naive and seasonal naive forecasts (Table 3.5) and their predictive measures (Table 3.6)


Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character( Amtrak.data$Month)) ) |>
  as_tsibble(index = Month)

# Table 3.5

train.ridership <- ridership |> filter_index( ~ "2001 Mar") 
valid.ridership <- ridership |> filter_index( "2001 Apr" ~ .)

# Naive model
ridership.naive <- train.ridership |>
  model(naive_model = NAIVE(Ridership))

ridership.naive.pred <- ridership.naive |> forecast(h = dim(valid.ridership)[1])

accuracy(ridership.naive.pred, ridership) |> select(MAE, RMSE, MAPE)

# SNAIVE
ridership.snaive <- train.ridership |>
  model(snaive_model = SNAIVE(Ridership))

ridership.snaive.pred <- ridership.snaive |> forecast(h = dim(valid.ridership)[1])


accuracy(ridership.snaive.pred, ridership) |> select(MAE, RMSE, MAPE)



#############
# Table 3.6 (clever way of calculating values for 1-35 steps ahead)

lengthTrainPeriod <-dim(train.ridership)[1]
rollingWindowSize <- 1

ridership_tr <- ridership |> 
  slice(1:(n()-rollingWindowSize)) |>
  stretch_tsibble(.init=lengthTrainPeriod ,.step= 1)

# Naive
fc <- ridership_tr |>
  model(naive_model = NAIVE(Ridership)) |>
  forecast(h=1) 

fc |> accuracy(ridership) |> select(MAE, RMSE, MAPE ) 

# sNaive
fc <- ridership_tr |>
  model(naive_model = SNAIVE(Ridership)) |>
  forecast(h=1) 

fc |> accuracy(ridership) |> select(MAE, RMSE, MAPE ) 

