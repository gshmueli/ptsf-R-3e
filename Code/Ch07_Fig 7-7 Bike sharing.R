################
# Code to create Fig.7.7_LR_Bike
# Forecasting Bike Rentals (TSLM with external predictors
###################


bike <- read.csv("Data/BikeSharingDaily.csv") |>
  mutate(
    Date = as.Date(dteday, format = "%Y-%m-%d"),
    Month = factor(month(Date, label = TRUE, abbr = TRUE), ordered = FALSE),
    WorkingDay = factor(workingday, levels = c(0, 1), labels = c("Not_Working", "Working")),
    Weather = factor(weathersit, levels = c(1, 2, 3), labels = c("Clear", "Mist", "Rain_Snow")),
    WorkingDay_Weather = interaction(WorkingDay, Weather)) |>
  as_tsibble(index = Date) |>
  select(Date, cnt, Month, WorkingDay, Weather, WorkingDay_Weather)

train.bike <- bike |> filter_index(~"2012-10-02") 
valid.bike <- bike |> filter_index("2012-10-03"~.)   # 90 days

# fit TSLM model
fit <- train.bike |> 
  model(reg.model = TSLM(cnt ~ trend() + season() + Month + WorkingDay_Weather)) 
report(fit)

# forecast validation period (valid.bike also has external predictors)
fc <- fit |> forecast(valid.bike)

# Performance measures
accuracy(fit) # training
accuracy(fc, valid.bike) # validation

# Compute validation forecast errors
fc2 <- fc |>
  left_join(valid.bike, by = "Date") |>
  mutate(fc.error = valid.bike$cnt - fc$.mean)

# Plot 1: actual and forecasts
p1 <- autoplot(bike, cnt) +
  autolayer(fitted(fit), .fitted, color = "blue") +
  autolayer(fc, .mean, level = 95) +
  labs(title = "Rentals and Forecasts", y = "Count") +
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(ymd("2012-10-02"))), linetype = "solid", color = "grey55", size = 0.6)+
  geom_segment(aes(x = ymd("2012-10-02"), y = 10000, 
                   xend = ymd("2012-12-31"), yend = 10000),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55")+ 
  annotate(geom = "text", x = ymd("2012-11-20"), y = 10500, label = "Validation", color = "grey37") +
  geom_segment(aes(x = ymd("2011-01-01"), y = 10000, 
                   xend = ymd("2012-10-01"), yend = 10000),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") + 
  annotate(geom = "text", x = ymd("2012-01-01"), y = 10500, label = "Training", color="grey37")

# Plot 2: errors
p2 <- autoplot(fc2, fc.error, linetype = "dashed") +
  autolayer(resid(fit), .resid) +
  labs(title = "Errors", y = "Error")

#pdf("Fig_7_6_3e_Bike.pdf", height=6.5,width=8)
grid.arrange(p1, p2 , nrow = 2)
#dev.off()

