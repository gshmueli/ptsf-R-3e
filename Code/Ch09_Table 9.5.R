# Neural net for Melbourne rainfall (binary series)
set.seed(201)
fit <- train.rain |>
  model(
    nn = BINNET(Rainy ~ Lag1 + Seasonal_sine + Seasonal_cosine) 
  )

report(fit)
fitted(fit) # gives predicted probabilities (between 0 and 1)

fc <- forecast(fit, valid.rain) # predicted probabilities in .mean

# training performance
library(caret)
confusionMatrix(data = as.factor(fitted(fit)$.fitted > 0.5),
                reference = as.factor(train.rain$Rainy),
                positive = "TRUE")

# validation performance
confusionMatrix(data = as.factor(fc$.mean > 0.5),
                reference = as.factor(valid.rain$Rainy),
                positive = "TRUE")
