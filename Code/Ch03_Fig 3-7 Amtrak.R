#################
# Code to create Figure 3.7 (naive and seaonal naive forecasts)

Amtrak.data <- read.csv("Data/Amtrak.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

fit <- train.ridership |>
  model(
    naive_model = NAIVE(Ridership),
    snaive_model = SNAIVE(Ridership)
  )

# Get forecasts for validation period
fc <- fit |>
  forecast(h = nrow(valid.ridership)) # or simply h = "3 years"

p.model <- autoplot(ridership, Ridership) +
  # autolayer(fitted(fit), .fitted, alpha = 0.7) + # Uncomment to see training fitted values
  autolayer(fc, .mean, level = NULL, linetype = "longdash", size = 1.25) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  xlab("Time") + ylab("Ridership") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55") +  
  annotate(geom = "text", x = yearmonth("2002-Aug"), y = 2280, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") +  
  annotate(geom = "text", x = yearmonth("1996-Aug"), y = 2280, label = "Training", color = "grey37") +
  theme(legend.position = "bottom")

# Errors plot
fc2 <- fc |>
  left_join(valid.ridership, by = "Month") |>
  mutate(fc.error = valid.ridership$Ridership - fc$.mean)

p.errors <- autoplot(fc2, fc.error, linetype = "dashed") +
  autolayer(resid(fit), .resid) +
  labs(title = "Errors", x = "Time", y = "Error") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
#  theme(legend.position = "bottom") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 500, 
                   xend = yearmonth("2004-Mar"), yend = 500),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55") +  
  annotate(geom = "text", x = yearmonth("2002-Aug"), y = 530, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 500, 
                   xend = yearmonth("2001-Mar"), yend = 500),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") +  
  annotate(geom = "text", x = yearmonth("1996-Aug"), y = 530, label = "Training", color = "grey37") +
  theme(legend.position = "none")

# Plot 
pdf("Plots/AmtrakFig_3_7_3e.pdf", height=7, width=8)
grid.arrange(p.model, p.errors, nrow = 2)
dev.off() 
