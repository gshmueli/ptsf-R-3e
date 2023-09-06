#################
# Code to create Figure 5-4


Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  dplyr::mutate(Month = yearmonth(as.character( Amtrak.data$Month)) ) |>
  as_tsibble(index = Month)

lag_data <- ridership |> mutate(dif1=  Ridership - lag(Ridership,1),
                     dif12 =  Ridership - lag(Ridership,12),
                     dif12_1 =  dif12 - lag(dif12,1))


p1 <- lag_data  |>
  autoplot(Ridership) +
  xlab("Time") + ylab("Ridership") + labs(subtitle = "Ridership")+ #+ theme(plot.title = element_text(hjust = 0.5, size=8)))
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")

p2 <- lag_data  |>
  autoplot(dif12) +
  xlab("Time") + ylab("Ridership") + labs(subtitle = "Lag-12 Difference") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")


p3 <- lag_data  |>
  autoplot(dif1) +
  xlab("Time") + ylab("Ridership") + labs(subtitle = "Lag-1 Difference") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")


p4 <- lag_data  |>
  autoplot(dif12_1) +
  xlab("Time") + ylab("Ridership") + labs(subtitle = "Twice-Differenced (Lag-12, Lag-1)") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")



pdf("Plots/AmtrakFig_5_4_3e.pdf",height=4,width=7)
grid.arrange(p1, p2, p3, p4, 
             ncol = 2, nrow = 2)
dev.off()

