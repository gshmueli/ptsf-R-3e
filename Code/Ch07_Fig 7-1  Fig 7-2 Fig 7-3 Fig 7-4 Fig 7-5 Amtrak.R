################
# Code to create Figures 7.1-7.5


Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  dplyr::mutate(Month = yearmonth(as.character( Amtrak.data$Month))) |>
  as_tsibble(index = Month)

ridership.24 <- ridership |> filter_index(~ "1992 Dec") 

ridership.24 <- ridership.24 |> 
                    mutate(lag1Ridership = lag(Ridership, n=1),
                           lag2Ridership = lag(Ridership, n=2))
# Figure 7.1
View(ridership.24)


################
# Code to create Figure 7.2

ridership.24 |> ACF(Ridership, lag_max = 18) |>
  autoplot() + 
  xlab("Lag") + 
  scale_x_continuous(breaks = seq(0, 18, by = 1)) 

###############
# Code to create Figure 7.3

train.ridership <- ridership |> filter_index(~ "2001 Mar") 

# Seasonality & quadratic trend
train.lm.trend.season <- train.ridership |> 
  model(TSLM(Ridership ~ trend() + I(trend()^2) + season()))

train.lm.trend.season |> residuals() |> ACF(.resid, lag_max = 18) |> 
  autoplot() + 
  xlab("Lag")

###############
# Code to create Figure 7.4

train.lm.trend.season.resid <- train.ridership |> 
  model(TSLM( Ridership ~ trend() + I(trend()^2) + season())) |>
  residuals() |>
  select(-.model) 

train.res.arima <- train.lm.trend.season.resid |> 
  model(ARIMA(.resid ~ 1 + pdq(1,0,0) ))

train.res.arima |> forecast(h=1)

train.res.arima.fitted <- fitted.values(train.res.arima)

p <- train.lm.trend.season.resid  |>
  autoplot() + 
  geom_line(aes(y = .fitted), data = train.res.arima.fitted,  colour = "blue1", size = 1) +
  xlab("Time") + ylab("Residuals")  +
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size = 0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 200, 
                   xend = yearmonth("2004-May"), yend = 200),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color="grey55")+  
  annotate(geom="text", x=yearmonth("2002-Sep"), y=240, label="Validation", color="grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 200, 
                   xend = yearmonth("2001-Mar"), yend = 200),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color="grey55")+  
  annotate(geom="text", x=yearmonth("1995-Aug"), y=240, label="Training", color="grey37")   +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") 

p

report(train.res.arima)

###############
# Code to create Figure 7.5

train.res.arima |> residuals() |> ACF(.resid, lag_max = 36) |> 
  autoplot() + xlab("Lag")




