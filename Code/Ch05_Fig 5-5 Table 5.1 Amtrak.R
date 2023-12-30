#################
# Code for creating Figure 5.5 and Table 5.1

Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

lag_data <- ridership |> mutate(dif1 =  Ridership - lag(Ridership, 1),
                                 dif12 =  Ridership - lag(Ridership, 12),
                                 dif12_1 =  dif12 - lag(dif12, 1))

train.ridership <- lag_data |> filter_index("1992 Feb" ~ "2001 Mar") # ETS does not support missing values
valid.ridership <- lag_data |> filter_index("2001 Apr" ~ .)

fit.ets <- train.ridership |> 
model(ets = ETS(dif12_1 ~ error("A") + trend("N", alpha = 0.2) + season("N")))

fc.ets <- fit.ets |> forecast(h = nrow(valid.ridership)) |> print()

pdf("Plots/AmtrakFig_5_5_3e.pdf",height=4,width=6)
fc.ets |> 
  autoplot(train.ridership, level = NULL, size = 1, linetype = "dashed",  colour = "blue1") +
  geom_line(aes(y = dif12_1), data = lag_data, size = 0.5) +
  geom_line(aes(y = .fitted), data = fitted.values(fit.ets), size = 1, colour = "blue2") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 250, xend = yearmonth("2004-Mar"), yend = 250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color="grey55")+  # error Validation
  annotate(geom = "text", x = yearmonth("2002-Aug"), y=290, label="Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 250, xend = yearmonth("2001-Mar"), yend = 250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55")+  # error Validation
  annotate(geom = "text", x = yearmonth("1996-Aug"), y = 290, label = "Training", color="grey37")
dev.off()
 
############################################
# Table 5.1
############################################

fit.ets.opt <- train.ridership |> 
  model(ets = ETS(dif12_1 ~ error("A") + trend("N") + season("N")))
report(fit.ets.opt)

fc.ets.opt <- fit.ets.opt |> forecast(h = nrow(valid.ridership))

accuracy(fit.ets) # training
accuracy(fc.ets, valid.ridership) # validation

accuracy(fit.ets.opt) # training
accuracy(fc.ets.opt, valid.ridership) # validation











 
