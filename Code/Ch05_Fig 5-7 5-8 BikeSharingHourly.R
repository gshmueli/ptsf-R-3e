############################################
# Code to create Figures 5.7 and 5.8

bike.hourly.data <- read.csv("Data/BikeSharingHourly.csv")

rides <- bike.hourly.data |>
  mutate(datetime = as.Date(dteday, format = "%Y-%m-%d") + hours(hr)) |>  # combine date and hour into a datetime
  as_tsibble(index = datetime) |> # convert to tsibble
  select(cnt, datetime)

# Set up the training and the validation sets:
rides.train <- rides |> filter_index("2012-07-01" ~ "2012-07-21") # 21 days
rides.validation <- rides |> filter_index("2012-07-22" ~ "2012-07-31") # 10 days

##Figure 5.7

# Plot time series, with zoom, to see hourly and day-of-week seasonality
p.ridership <- rides.train |>  
  autoplot(cnt) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%Y-%m-%d") +
  labs(y = "Hourly bike rentals", x = "Week", title = "Weekly Pattern (3 weeks)")

p.ridership.zoom <- rides.train |>  
  filter_index("2012-07-01" ~ "2012-07-04") |>
  autoplot(cnt) +
  scale_x_datetime(date_breaks = "1 day") +
  labs(y = "Hourly bike rentals", x = "Day", title = "Daily Pattern (4 days)")
  

pdf("Plots/BikeFig_5_7_3e.pdf",height=4,width=6)
grid.arrange(p.ridership, p.ridership.zoom, ncol = 1)
dev.off()

## Create Figure 5.8

## Create index t for STL
rides.train <- rides.train |>
  mutate(t = row_number()) |>
  update_tsibble(index = t, regular = TRUE)

## Run STL + ETS
my_dcmp_spec <- decomposition_model(
  STL(cnt ~ season(period = 24) +
        season(period = 7*24),
      robust = TRUE),
  ETS(season_adjust ~ season("N"))
)

## Generate forecasts from model
fc <- rides.train |>
  model(my_dcmp_spec) |>
  forecast(h = 240) |> 
  autoplot(rides.train, level = NULL, linetype = "dashed") +
  labs(y = "Hourly bike rentals")

pdf("Plots/BikeFig_5_8_3e.pdf", height=4, width=6)
fc
dev.off()
