################
# Code for creating Table 6.4 & Figure 6.7

Amtrak.data <- read.csv("Data/Amtrak.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

# Model with seasonality & quadratic trend
train.lm.trend.season <- train.ridership |> 
                   model(TSLM(Ridership ~ trend() + I(trend()^2) + season()))

### Table 6.4
report(train.lm.trend.season)


### Figure 6.7
pred.values.train.trend.season <- fitted.values(train.lm.trend.season) # forecasts in training period
fc.lm.trend.season <- train.lm.trend.season |> forecast(h = 36)    # forecasts in validation period
fc.resid.trend.season <-  valid.ridership$Ridership - fc.lm.trend.season$.mean # errors in validation period

fc.resid.trend.season <- data.frame(valid.ridership$Month, fc.resid.trend.season) 
colnames(fc.resid.trend.season)[1] <- "Month"
fc.resid.trend.season <- fc.resid.trend.season |>
  as_tsibble(index = Month)


p.model <- ridership  |>
  autoplot(Ridership) + 
  geom_line(aes(y = .mean), data = fc.lm.trend.season,  colour = "blue1", linetype = "dashed", size = 1) +
  autolayer(pred.values.train.trend.season, alpha = 0.5, level = NULL, colour = "blue1", size = 1) +
  xlab("Time") + ylab("Ridership")  +
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2450, xend = yearmonth("2004-May"), yend = 2450),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color="grey55")+  
  annotate(geom = "text", x = yearmonth("2003-Jan"), y = 2500, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2450, xend = yearmonth("2001-Mar"), yend = 2450),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55")+  
  annotate(geom = "text", x = yearmonth("1995-Aug"), y = 2500, label = "Training", color = "grey37") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") 

p.resid <- train.lm.trend.season |> augment() |> 
  autoplot(.resid) +
  autolayer(fc.resid.trend.season, level = NULL) +
  xlab("Time") + ylab("Residuals")  +
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") 


pdf("Plots/AmtrakFig_6_7_3e.pdf",height=5.5,width=8)
grid.arrange(p.model, p.resid, nrow = 2)
dev.off()
