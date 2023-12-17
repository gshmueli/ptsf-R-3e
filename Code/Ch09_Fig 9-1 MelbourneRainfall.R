#################
# Code for creating Figure 9.1

rain.data <- read.csv("MelbourneRainfall.csv")

rain <- rain.data |>
  mutate(Date = dmy(as.character(rain.data$Date))) |>
  as_tsibble(index = Date)

rain <- rain |> 
  mutate(
    Rainy = (rain$RainfallAmount_millimetres > 0),  # create new binary column Rainy
    month = format(Date, format = "%m"),
    year = format(Date, "%Y")
  )

average.rain <- as_tibble(rain) |>
  group_by(month, year) |>
  summarise(pct.Rainy = mean(Rainy)) |>
  group_by(month) |>
  summarise(avg.Rainy = mean(pct.Rainy) * 100)

# plot
pdf("MelbourneRainfallFig_8_1_3e.pdf",height=6,width=9)
as_tibble(rain) |>
  group_by(month, year) |>
  summarise(pct.Rainy = mean(Rainy) * 100, .groups = 'drop') |>
  ggplot(aes(x = month, y = pct.Rainy, group = year, color = year)) +
  geom_line() +
  geom_line(data = average.rain, aes(y = avg.Rainy, x = month), colour="black", group = 1, linetype = "dashed", size = 1.2) +
  xlab("Month") + ylab("Percent of rainy days per month")
dev.off()
