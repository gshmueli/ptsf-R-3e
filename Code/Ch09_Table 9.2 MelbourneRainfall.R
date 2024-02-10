### Table 9.2

# create columns for trend and lag
rain <- rain |>
  mutate(
    Rainy = (rain$RainfallAmount_millimetres > 0),  # create new binary column Rainy
    Lag1 = lag(Rainy),        # create Lag1 with previous day Rainy/not
    t = row_number(),   # create t with time index 1 to nPeriods
    Seasonal_sine = sin(2 * pi * t / 365.25),   # create Seasonal_sine with sine term
    Seasonal_cosine = cos(2 * pi * t / 365.25)  # create Seasonal_cosine with cosine term
  )

# Partition data
train.rain <- rain |> filter_index(~ "2009-12-31")
valid.rain <- rain |> filter_index("2010-01-01" ~ .)

library(fable.binary) # for fitting logistic regression to tsibble object

fit <- train.rain |>
  model(
    logistic = LOGISTIC(Rainy ~ Lag1 + Seasonal_sine + Seasonal_cosine)
  )
# Or, equivalently, use the simpler code: LOGISTIC(Rainy ~ Lag1 + fourier(K = 1, period = "year"))

report(fit)
fitted(fit) # gives predicted probabilities for training period
fc <- forecast(fit, valid.rain) # predicted probabilities for validation period (in .mean)

# training performance
library(caret)
confusionMatrix(data = as.factor(fitted(fit)$.fitted > 0.5),
                reference = as.factor(train.rain$Rainy),
                positive = "TRUE")

# validation performance
confusionMatrix(data = as.factor(fc$.mean > 0.5),
                reference = as.factor(valid.rain$Rainy),
                positive = "TRUE")
