#################
# Code to create Figure 3.7 (naive and seaonal naive forecasts)

Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index( ~ "2001 Mar") 
valid.ridership <- ridership |> filter_index( "2001 Apr" ~ .)

# Naive forecasts
ridership.naive <- train.ridership |>
  model(naive_model = NAIVE(Ridership)) |> 
  forecast(h = "3 years") 

# Seasonal naive forecasts
ridership.snaive <- train.ridership |>
  model(snaive_model = SNAIVE(Ridership)) |> 
  forecast(h = "3 years") 

fc <- 
train.ridership |>
  autoplot(Ridership) +
  autolayer(ridership.naive, level = NULL, linetype = "dashed", size = 1.25) +
  autolayer(ridership.snaive, level = NULL, linetype = "dashed", color = "coral1", size = 0.8) +
  xlab("Time") + ylab("Ridership") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6)+
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , size=0.3, color="grey55")+  # arrow Validation
  annotate(geom="text", x=yearmonth("2002-Aug"), y=2280, label="Validation", color="grey37") +
  
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), size=0.3, color="grey55")+  # arrow Training
  annotate(geom="text", x=yearmonth("1996-Aug"), y=2280, label="Training", color="grey37")+
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")

pdf("Plots/AmtrakFig_3_3_3e.pdf",height=4,width=6)
fc
dev.off()
