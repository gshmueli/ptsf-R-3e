################
# Code to create Table 6.5 for the 3rd addition


Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  dplyr::mutate(Month = yearmonth(as.character( Amtrak.data$Month)) ) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index( ~ "2003 Mar") 
valid.ridership <- ridership |> filter_index( "2003 Apr" ~ .)

# Seasonality & quadratics trend
train.ridership |> model(TSLM( Ridership ~ trend() + I(trend()^2) + 
                I(sin(2*pi*trend()/12)) + I(cos(2*pi*trend()/12)) ))




