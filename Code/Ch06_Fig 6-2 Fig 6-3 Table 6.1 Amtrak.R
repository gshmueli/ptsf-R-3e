#################
# Code for creating Figures 6.2, 6.3, and Table 6.1

Amtrak.data <- read.csv("Data/Amtrak.csv")

ridership <- Amtrak.data |>
  dplyr::mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

# Final three years as validation period 
train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

# Fit linear trend model to training period
train.lm <- train.ridership |> 
  model(TSLM(Ridership ~ trend()))

# Generate forecasts for validation period
fc.lm <- train.lm |> forecast(h = 36)

### Fig 6.2 (simple code)
train.ridership |> 
  autoplot(Ridership) +
  autolayer(fitted.values(train.lm), colour = "blue1", size = 1.2) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = "Time")

# Fig 6.3 (without Training/Validation arrows and marks)
ridership |> 
  autoplot(Ridership) +
  autolayer(fitted.values(train.lm), colour = "blue1", size = 1.2) +
  geom_line(aes(y = .mean), data = fc.lm,  colour = "blue1", linetype = "dashed", size = 1.2) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = "Time")


### Fig 6.3 with Training/Validation arrows and marks

fc.lm <- train.lm |> forecast(h = 36)

pdf("Plots/AmtrakFig_6_3_3e.pdf", height=4, width=8)

ridership  |>
  autoplot(Ridership) + 
  geom_line(aes(y = .mean), data = fc.lm,  colour = "blue1", linetype = "dashed", size = 1.2) +
  autolayer(fitted.values(train.lm), colour="blue1", size = 1.2) +
  xlab("Time") + ylab("Ridership")  +
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, xend = yearmonth("2004-May"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55")+  
  annotate(geom="text", x = yearmonth("2003-Jan"), y = 2290, label = "Validation", color="grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55")+  
  annotate(geom="text", x=yearmonth("1997-Aug"), y = 2290, label = "Training", color = "grey37") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")  

dev.off()

#### Table 6.1
report(train.lm)
