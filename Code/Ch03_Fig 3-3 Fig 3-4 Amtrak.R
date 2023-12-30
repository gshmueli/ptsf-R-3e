#################
# Code to create Figure 3.3 and Figure 3.4

Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

# Fit model to training period
ridership.lm <- train.ridership |>
  model(trend_model = TSLM(Ridership ~ trend() + I(trend()^2)))
# augment(ridership.lm)

# Get forecasts for validation period
fc <- ridership.lm |> 
  forecast(h = nrow(valid.ridership))

# Compute validation errors
fc <- fc |>
  mutate(fc.error = valid.ridership$Ridership - fc$.mean) |>
  as_tsibble(index = Month)

# Plot 1: actuals and forecasts
p.model <- autoplot(ridership, Ridership) +
  autolayer(fitted(ridership.lm), .fitted, color = "blue", size = 1.25) + 
  autolayer(fc, .mean, level = NULL, linetype = "dashed", color = "blue", size = 1.25) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Actual and forecasted ridership", y = "Ridership") +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55") +  
  annotate(geom = "text", x = yearmonth("2002-Aug"), y=2290, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") +  
  annotate(geom = "text", x = yearmonth("1996-Aug"), y=2290, label = "Training", color = "grey37")

# Plot 2: errors
p.errors <- autoplot(resid(ridership.lm), .resid) + 
  autolayer(fc, fc.error, linetype = "dashed") + 
  geom_hline(yintercept = 0) + 
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Errors", y = "Error") +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6)

#### Figure 3.4 (histogram):
pdf("Plots/AmtrakFig_3_4_3e.pdf", height=4, width=6)
fc |> 
  ggplot(aes(x = fc.error)) + # errors of the validation set
  geom_histogram(bins = 7, color = "black", fill = "white") +
  xlab("Forecast Error") + ylab("Frequency")
dev.off()
