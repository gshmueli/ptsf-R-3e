#################
# Code to create Figure 2-2


Amtrak.data <- read.csv("Data/Amtrak data.csv")
ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

View(ridership)


# Figure 2-3
pdf("Plots/AmtrakFig_2_2_3e.pdf",height=3,width=5)
ridership |>
  autoplot(Ridership) +
  xlab("Time") + ylab("Ridership") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")
dev.off()

