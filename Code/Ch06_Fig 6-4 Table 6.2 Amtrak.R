#################
# Code to create Figure 6.4 & Table 6.2


Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  dplyr::mutate(Month = yearmonth(as.character( Amtrak.data$Month)) ) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index( ~ "2001 Mar") 
valid.ridership <- ridership |> filter_index( "2001 Apr" ~ .)


train.lm <- train.ridership |>
  model(
    linear = TSLM(Ridership ~ trend()),
    exponential = TSLM(log(Ridership) ~ trend()),
  )
fc_trends <- train.lm |> forecast(h = 36)

pdf("Plots/AmtrakFig_6_4_3e.pdf",height=6,width=8)

ridership |>
  autoplot(Ridership) +
  geom_line(data = fitted(train.lm),
            aes(y = .fitted, colour = .model, linetype = .model)) +
  autolayer(fc_trends, level = NULL, linetype = "dashed") +
  labs(x = "Time") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  # adding training/validation arrows/marks
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-May"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color="grey55")+  
  annotate(geom="text", x=yearmonth("2003-Jan"), y=2290, label="Validation", color="grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color="grey55")+  
  annotate(geom="text", x=yearmonth("1997-Aug"), y=2290, label="Training", color="grey37")
dev.off()

### Polynomial trend - Table 6.2

train.lm.poly.trend <- train.ridership |> 
  model(TSLM(Ridership ~ trend() + I(trend()^2)))

report(train.lm.poly.trend)

######### OPTIONAL: CREATE MEDIAN FORECASTS (INSTEAD OF MEAN). THIS WILL BE IDENTICAL TO FORECASTS COMPUTED MANUALLY BY TAKING EXPONENT OF FORECASTED LOG(Y)

# Exponential Trend 
train.lm.expo.trend <- train.ridership |> 
  model(TSLM(log(Ridership) ~ trend()))

pred.values.train.lm.expo.trend <- fitted(train.lm.expo.trend) # fitted values train window
report(train.lm.expo.trend)

# Using mean forecasts
fc.lm.expo.trend <- train.lm.expo.trend |> forecast(h=36)
# Using median forecasts
fc.lm.expo.trend <- train.lm.expo.trend |> forecast(h=36, point_forecast = lst(mean, median)) 

