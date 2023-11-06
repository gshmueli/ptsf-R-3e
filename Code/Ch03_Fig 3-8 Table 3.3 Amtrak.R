#################
# Code to create Figure 3.8 and Table 3.3

Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

# Naive model
ridership.naive <- train.ridership |>
  model(naive_model = NAIVE(Ridership))

ridership.naive.pred <- ridership.naive |> forecast(valid.ridership)

# Monthly roll-foreword validation period
ridership.naive <- train.ridership |>
  model(roll_model = NAIVE(Ridership))

stepAhead <- 1
lengthTrainPeriod <- dim(train.ridership)[1]
lengthValidPeriod <- dim(valid.ridership)[1]
error <- rep(NA, lengthValidPeriod -1 + stepAhead)

for(i in 1:lengthValidPeriod ){
  train.ridership.l <- ridership |> slice(., 1:(lengthTrainPeriod-1+i)) 

  ridership.naive.l <- train.ridership.l |>
                           model(naive_model = NAIVE(Ridership))
  
  ridership.naive.pred.l <- ridership.naive.l |> 
                          forecast(h = stepAhead)
  error[i] <- valid.ridership$Ridership[i] - ridership.naive.pred.l$.mean
}
error <- unlist(error) 


ridership.naive.pred.l <- rep(NA, dim(valid.ridership)[1])
for(i in 1:lengthValidPeriod ){
  train.ridership.l <- ridership |> slice(1:(lengthTrainPeriod-1+i)) 
 
  ridership.naive.l <- train.ridership.l |>
    model(naive_model = NAIVE(Ridership))
  
  temp <- ridership.naive.l |> 
    forecast(h = stepAhead) 
  ridership.naive.pred.l[i] <- temp$.mean
}

predict.df <- bind_cols(valid.ridership, ridership.naive.pred.l)
names(predict.df)[3] <- "predict"

# Figure 3.8

p <- ridership.naive.pred  |>
  autoplot(ridership, level=NULL, linetype = "dashed", size=1.25) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  geom_line(aes(Month, predict), predict.df, linetype = "dashed", size=0.8, color = "coral1")+
  xlab("Time") + ylab("Ridership")  +
  theme(axis.text.y = element_text(angle = 90, hjust = 1)) + # AXIS TICKS: SET AND ROTATE
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6)+
  geom_segment(aes(x = yearmonth("2001-May"), y = 2500, 
                   xend = yearmonth("2004-Mar"), yend = 2500),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color="grey55")+  # arrow Validation
  annotate(geom="text", x=yearmonth("2002-Aug"), y=2540, label="Validation", color="grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2500, 
                   xend = yearmonth("2001-Mar"), yend = 2500),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color="grey55")+  # arrow Training
  annotate(geom="text", x=yearmonth("1996-Aug"), y=2540, label="Training", color="grey37")

p


#############
# Table 3.3 

lengthTrainPeriod <-dim(train.ridership)[1]
rollingWindowSize <- 1
ridership_tr <- ridership |>
  slice(1:(n()-rollingWindowSize)) |>
  stretch_tsibble(.init=lengthTrainPeriod ,.step= 1)

fc <- ridership_tr |>
  model(naive_model = NAIVE(Ridership)) |>
  forecast(h=1)

fc |> accuracy(ridership) |> select(MAE, RMSE, MAPE)

###################

# 1 step ahead

MAE <- mean(abs(error))
RMSE <- sqrt(mean(abs(error^2)))
MAPE <- mean(abs(error/predict.df$Ridership))
c(MAE,RMSE, MAPE)

# >=1 steps ahead
step.ahead <- 1
roll.forward <-1
lengthTrainPeriod <- dim(train.ridership)[1]
lengthValidPeriod <- dim(valid.ridership)[1]
error       <- rep(NA, lengthValidPeriod - roll.forward)
prec.actual <- rep(NA, lengthValidPeriod  - roll.forward)


for(i in 1:(lengthValidPeriod - roll.forward + step.ahead)){
  train.ridership.l <- ridership |> slice( 1:(lengthTrainPeriod-1+i) ) 
  
  ridership.naive.l <- train.ridership.l |>
    model(naive_model = NAIVE(Ridership))
  
  ridership.naive.pred.l <- ridership.naive.l |> 
                               forecast(h = roll.forward) |> 
                               slice(roll.forward:(roll.forward+step.ahead-1)) # keep forecast for 1 obs from roll.forward location
  
  error[i] <- valid.ridership$Ridership[roll.forward + i - 1] - ridership.naive.pred.l$.mean
  prec.actual[i] <- error[i] / valid.ridership$Ridership[roll.forward + i - 1]
}

# augment(ridership.naive.l)
# View(valid.ridership)

MAE <- mean(abs(error))
RMSE <- sqrt(mean(abs(error^2)))
MAPE <- mean(abs(prec.actual))
c(MAE,RMSE, MAPE)


############
# Roll-forward overall
############

lengthTrainPeriod <-dim(train.ridership)[1]
rollingWindowSize <- 1

ridership_tr <- ridership |> 
  slice(1:(n()-rollingWindowSize)) |>
  stretch_tsibble(.init=lengthTrainPeriod ,.step= 1)

# Naive
fc <- ridership_tr |>
  model(naive_model = NAIVE(Ridership)) |>
  forecast(h=1) 

fc |> accuracy(ridership) |> select(MAE, RMSE, MAPE) 



#############
# Fixed partitioning overall

train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

fit <- train.ridership |>
  model(naive_model = NAIVE(Ridership))

fc <- fit |> forecast(h = dim(valid.ridership)[1])

accuracy(fc, ridership)
#  .model      .type    ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
#  <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1 trend_model Test  -84.0  180.  134. -4.73  7.08  1.62 0.417
