################
# This code calculates the naive and seasonal naive forecasts (Table 3.4) and their predictive measures (Table 3.5)


Amtrak.data <- read.csv("Data/Amtrak.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month)) ) |>
  as_tsibble(index = Month)

fit <- train.ridership |>
  model(
    naive_model = NAIVE(Ridership),
    snaive_model = SNAIVE(Ridership)
  )

# fixed validation forecasts:
fc <- fit |> forecast(h = valid.ridership)

# validation accuracy
accuracy(fc, valid.ridership) |>   
     select(MAE, RMSE, MAPE)

# to get training accuracy: 
accuracy(fit)

#############
# Table 3.6 (clever way of calculating values for 1-35 steps ahead)

lengthTrainPeriod <- nrow(train.ridership)
rollingWindowSize <- 1

ridership_tr <- ridership |> 
  slice(1:(n()-rollingWindowSize)) |>
  stretch_tsibble(.init = lengthTrainPeriod, .step= 1)

# Naive
fc <- ridership_tr |>
  model(naive_model = NAIVE(Ridership)) |>
  forecast(h=1) 

fc |> accuracy(ridership) |> select(MAE, RMSE, MAPE) 

# sNaive
fc <- ridership_tr |>
  model(naive_model = SNAIVE(Ridership)) |>
  forecast(h=1) 

fc |> accuracy(ridership) |> select(MAE, RMSE, MAPE) 
