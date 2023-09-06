library(caret)  # loading the package caret that contains functions to streamline the model training process for complex regression.
library(InformationValue) # 

setwd("C:/Users/jpolak/GitHub/PTSF_GalitShmueli_fable")

rain.df <- read.csv("Data/MelbourneRainfall.csv") # read the data. The data frame contain two columns, the Date column and the RainfallAmount_millimetres column.
rain.df$Date <- as.Date(rain.df$Date, format="%d/%m/%Y") # change the data format in the Date column.
rain.df$Rainy <- ifelse(rain.df$Rainfall > 0, 1, 0) # create a new column, names Rainy, to be an indicator if a rainy day.
nPeriods <- length(rain.df$Rainy) # number of rainy days 
rain.df$Lag1 <- c(NA,rain.df$Rainfall[1:(nPeriods-1)]) # create a new column, Lag1, with a 

rain.df$t <- seq(1, nPeriods, 1)
rain.df$Seasonal_sine = sin(2 * pi * rain.df$t / 365.25)
rain.df$Seasonal_cosine = cos(2 * pi * rain.df$t / 365.25)
train.df <- rain.df[rain.df$Date <= as.Date("31/12/2009", format="%d/%m/%Y"), ]
train.df <- train.df[-1,]
valid.df <- rain.df[rain.df$Date > as.Date("31/12/2009", format="%d/%m/%Y"), ]
xvalid <- valid.df[, c(4,6,7)]

rainy.lr <- glm(Rainy ~ Lag1 + Seasonal_sine + Seasonal_cosine, data = train.df, family = "binomial")
summary(rainy.lr)
rainy.lr.pred <- predict(rainy.lr, xvalid, type = "response") 
confusionMatrix(ifelse(rainy.lr$fitted > 0.5, 1, 0), train.df$Rainy)
confusionMatrix(ifelse(rainy.lr.pred > 0.5, 1, 0)  , valid.df$Rainy)

str(ifelse(rainy.lr$fitted > 0.5, 1, 0))
str(train.df$Rainy)

confusionMatrix(as.vector(ifelse(rainy.lr$fitted > 0.5, 1, 0)), train.df$Rainy, positive = "1")
