#################
# Code for creating Figure 9.1


rain.data <- read.csv("Data/MelbourneRainfall.csv")

rain <- rain.data |>
  dplyr::mutate(Date = dmy(as.character(rain.data$Date))) |>
  as_tsibble(index = Date)


rain <- rain |> mutate(day = format(Date, "%d"), month = format(Date, "%m"), year = format(Date, "%Y")) |>
  unite("month_year", month:year, sep = "-", remove = FALSE) 

rain <- rain |> mutate(rainy = ifelse(RainfallAmount_millimetres>0,1,0))

rain <-  rain |> group_by(month_year) |> 
            summarise(per.rainy = mean(rainy)) |> 
            mutate(Year = str_sub(month_year, start = 4, end = 7)) |>
            mutate(month = str_sub(month_year, start = 1, end = 2))

avr.rain <- rain |> group_by(month) |> summarise(avr.per.rainy = mean(per.rainy)) |>
            mutate(Year = "mean")

# Plot
# pdf("Plots/MelbourneRainfallFig_8_1_3e.pdf",height=6,width=9)
rain |>
  ggplot(aes(x=month, y=per.rainy, group=Year, color=Year)) +
  geom_line() + 
  geom_line(aes(y=avr.per.rainy), data = avr.rain, colour="black", linetype = "dashed", size = 1.2) +
  xlab("Month") + ylab("Percent of rainy days per month")
# dev.off()



