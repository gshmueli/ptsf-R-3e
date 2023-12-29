#################
# Code to create Figure 3-2

Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)


train.ridership <- ridership |> filter_index( ~ "2001 Mar") 
valid.ridership <- ridership |> filter_index( "2001 Apr" ~ .)


fit <- train.ridership |>
  model(trend_model = TSLM(Ridership ~  trend() + I(trend()^2)))

fc <- fit |> forecast(h = nrow(valid.ridership))

augment(fit) # print table with fitted values and the residuals.




pdf("Plots/AmtrakFig_3_2_3e.pdf",height=4,width=6)
ridership  |>
  autoplot(Ridership) +
  geom_line(aes(y=.fitted, colour=.model), data = fitted(fit), size = 1.25, color = "blue1") +
  autolayer(fc,  level = NULL, linetype = "dashed", color = "blue1", size = 1.25) +
  xlab("Time") + ylab("Ridership")  +
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55") +  
  annotate(geom = "text", x = yearmonth("2002-Aug"), y = 2280, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") +  
  annotate(geom = "text", x = yearmonth("1996-Aug"), y = 2280, label = "Training", color = "grey37") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")
dev.off()



#########################
# Code for computing the predictive measures, based on the values in Table 3.1
###############################

accuracy(fc, ridership)
