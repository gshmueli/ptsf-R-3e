################
# Code to create Fig. 7.7_one_pair
# WalmArt Sales - use of automated ARIMA
## Example 4: Adding IsHoliday -- Section 7.4
###################


one.pair <- read.csv("Data/Walmart_One_Pair.csv")

one.pair <- one.pair |>
  dplyr::mutate(Week = yearweek(one.pair$Date) ) |>
  filter(!is.na(Weekly_Sales)) |> # removes rows where Weekly_Sales is NA
  as_tsibble(index = Week)


one.pair.train <- one.pair |> filter_index(~ "2012 W05")  # 2 years (105 weeks)
one.pair.valid <- one.pair |> filter_index("2012 W06" ~ "2012 W43")  # 38 weeks

# Compare stepwise ARIMA(1,0,1) with stepwise AR(2) + IsHoliday.
fit <- one.pair.train |>
  model(SAR1 = ARIMA(Weekly_Sales),   # using the default stepwise procedure
        AR2.IsHoliday   = ARIMA(Weekly_Sales ~ IsHoliday)#,  
        #     SAR1.IsHoliday = ARIMA(Weekly_Sales ~ IsHoliday + 0 + pdq(1,0,0) + PDQ(0,1,0))  # specify best stepwise model without IsHoliday
  )
fit
glance(fit)
fit |> select(SAR1) |> report()
fit |> select(AR2.IsHoliday) |> report()
#fit |> select(SAR1.IsHoliday) |> report()




# Forcasting
fc <- fit |>
  forecast(one.pair.valid)

# Performance measures
accuracy(fit) # training
accuracy(fc, one.pair.valid) # validation



##new_ext_variable_values <- one.pair.valid |> select(Date, IsHoliday) 

##fc <- train.aut.arima |>
##   forecast(h = dim(one.pair.valid)[1], new_data = new_ext_variable_values)

## Performance measures
##accuracy(train.aut.arima.ext) # training
##accuracy(fc, one.pair.valid) # validation





# Compute validation forecast errors
# add them to new fc2
fc2 <- fc |>
  left_join(one.pair.valid, by = "Week") |>
  mutate(fc.error = one.pair.valid$Weekly_Sales - fc$.mean) |>
  select(.model, Week, fc.error)

# Plot 1: actual and forecasts
p3 <- autoplot(one.pair, Weekly_Sales) +
  autolayer(fitted(fit), .fitted, alpha = 0.7) +
  autolayer(fc, .mean, linetype = "dashed", level = NULL) +
  labs(title = "Sales and Forecasts", x = "Week", y = "Sales")

# Plot 2: errors
p4 <- autoplot(fc2, fc.error, linetype = "dashed") +
  autolayer(resid(fit), .resid, alpha = 0.7) +
  labs(title = "Errors", x = "Week", y = "Error")


pdf("Plots/OnePairFig_7_7_one_pair_3e.pdf", height=6.5,width=8)
grid.arrange(p3, p4 , nrow = 2)
dev.off()