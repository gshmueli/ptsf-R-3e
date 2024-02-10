
#################
# Code to create Figure 2-4

Amtrak.data <- read.csv("Data/Amtrak.csv")
ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character( Amtrak.data$Month)) ) |>
  as_tsibble(index = Month)

pdf("Plots/AmtrakFig_2_4_3e.pdf",height=4,width=6)
p.ridership <- ridership |>
  autoplot(Ridership) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + 
  xlab("Time") + ylab("Ridership") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")
  
p.ridership.zoom <- ridership |>
  filter_index("1997 Jan" ~ "2000 Dec") |>
  autoplot(Ridership) +
  xlab("Time") + ylab("Ridership")

grid.arrange(p.ridership, p.ridership.zoom, ncol = 1)
dev.off()
