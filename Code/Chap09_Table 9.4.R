# training performance
library(caret)
confusionMatrix(data = as.factor(fitted(fit)$.fitted > 0.5),
                reference = as.factor(train.rain$Rainy),
                positive = "TRUE")

# validation performance
confusionMatrix(data = as.factor(fc$.mean > 0.5),
                reference = as.factor(valid.rain$Rainy),
                positive = "TRUE")
