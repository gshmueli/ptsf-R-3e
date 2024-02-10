############################################
# Code for creating Figure 5.6 and Table 5.2
############################################

Amtrak.data <- read.csv("Data/Amtrak.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character( Amtrak.data$Month))) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

fit.ets.HW <- train.ridership |> model(ets =  ETS(Ridership ~ error("M") + trend("A") + season("A"))) 

fc.ets.HW <- fit.ets.HW |> forecast(h = nrow(valid.ridership))

pdf("Plots/AmtrakFig_5_6_3e.pdf", height=4, width=6)

ridership  |>
  autoplot(Ridership) +
  geom_line(aes(y = .mean), data = fc.ets.HW,  colour = "blue1", linetype = "dashed") +
  autolayer(fitted.values(fit.ets.HW), alpha = 0.5, level = NULL, colour = "blue1") +
  xlab("Time") + ylab("Ridership")  +
  theme(legend.position = "none") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55")+  
  annotate(geom = "text", x = yearmonth("2002-Aug"), y=2290, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55")+  
  annotate(geom = "text", x = yearmonth("1996-Aug"), y = 2290, label = "Training", color = "grey37")

dev.off()

############################################
# Table 5.2
############################################

report(fit.ets.HW)

View(fit.ets.HW |> components()) # to see the level, trend, seasonal components

# Final states:
n <- nrow(components(fit.ets.HW))
components(fit.ets.HW)[n, c("level", "slope")] # level and trend
t(components(fit.ets.HW)[(n-11):n, c("season")])  # s1 - s12

############################################
# Table 5.4
############################################

fit.ets.out <- train.ridership |> 
  model(ets = ETS(Ridership)) |>
  report()


############################################
# Table 5.5
############################################

accuracy(fit.ets.HW) # training
fc.ets.HW <- fit.ets.HW |> forecast(h = nrow(valid.ridership))
accuracy(fc.ets.HW, ridership)

accuracy(fit.ets.out)
fc.ets.out <- fit.ets.out |> forecast(h = nrow(valid.ridership))
accuracy(fc.ets.out, ridership)

# IMPORTANT comment
# In Table 5.5 we applied the accuracy() to the complete series rather than only the validation. 
# This is required for seasonal series. If we use accuracy(fc.ets.out, valid.ridership) it does not know what are the validation seasons and cannot compute seasonal MAPE and MASE. 
# In forecast package, the forecast object includes the information of the original dataset; In fable package, the forecast object does not include this info (to make the object smaller and simpler)
