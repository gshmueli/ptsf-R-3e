#################
# Code for creating Figure 5-2

Amtrak.data <- read.csv("Data/Amtrak.csv")

ridership <- Amtrak.data |>
  dplyr::mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

ma.ridership <- ridership |>
  mutate(MAw12_12 = slide_dbl(Ridership, mean, .before = 5, .after = 6, .complete = TRUE),
         MAw12_2x12 = slide_dbl(MAw12_12, mean, .before = 1, .after = 0, .complete = TRUE))  |>   # Centered moving average for even window
  mutate(MATw12 = slide_dbl(Ridership, mean, .before = 11, .after = 0, .complete = TRUE)) |>  # Trailing MA
  select(Month, Ridership, MAw12_2x12, MATw12)

colnames(ma.ridership)[c(3,4)] <- c("Centered Moving Average", "Trailing Moving Average")

# Create long table
ma.ridership <- ma.ridership |> gather(model, value, Ridership:`Trailing Moving Average`)

pdf("Plots/AmtrakFig_5_2_3e.pdf", height=4, width=6)
ma.ridership |> ggplot(aes(x = Month, y = value)) + 
  geom_line(aes(color = model, linetype = model, size = model) ) +
  scale_size_manual(values = c(1.2 , 0.4, 1.2)) + 
  scale_linetype_manual(values = c("dotdash", "solid","longdash"))+
  scale_color_manual(values=c("blue1", "black","coral"))+
  theme(legend.position = "bottom") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") 
dev.off()
