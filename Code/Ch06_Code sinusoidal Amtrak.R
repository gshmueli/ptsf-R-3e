################
# Code for fitting sinusoidal seasonality

Amtrak.data <- read.csv("Data/Amtrak.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character( Amtrak.data$Month))) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index(~ "2003 Mar") 
valid.ridership <- ridership |> filter_index("2003 Apr" ~ .)

# Sinusoidal seasonality & quadratic trend
# Note: fourier(K=1) is equivalent to using I(sin(2*pi*trend()/12)) + I(cos(2*pi*trend()/12)) 
train.ridership |> 
  model(TSLM(Ridership ~ trend() + I(trend()^2) + fourier(K=1))) |> report()
