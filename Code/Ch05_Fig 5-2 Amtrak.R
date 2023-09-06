#################
# Code to create Figure 5-2

Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  dplyr::mutate(Month = yearmonth(as.character( Amtrak.data$Month)) ) |>
  as_tsibble(index = Month)

ma.ridership <- ridership |>
  mutate(MAw12_12 = slide_dbl(Ridership, mean, .before = 5, .after = 6, .complete = TRUE),
         MAw12_2x12 = slide_dbl(MAw12_12, mean, .before = 1, .after = 0, .complete = TRUE))  |> # Centered moving average for even window
  mutate(MATw12 = slide_dbl(Ridership, mean,.before = 11, .after = 0, .complete = TRUE)) |>  # Trailing MA
  select(Month, Ridership, MAw12_2x12, MATw12)

colnames(ma.ridership)[c(3,4)] <- c("Centered Moving Average", "Trailing Moving Average")

# Create long table
ma.ridership <- ma.ridership |> gather(model, value, Ridership:`Trailing Moving Average`)


pdf("Plots/AmtrakFig_5_2_3e.pdf",height=4,width=6)
ma.ridership |> ggplot(aes(x=Month, y=value)) + 
  geom_line(aes(color = model, linetype = model, size = model) ) +
  scale_size_manual(values=c(1.2 , 0.4, 1.2)) + 
  scale_linetype_manual(values=c("dotdash", "solid","longdash"))+
  scale_color_manual(values=c( "blue1", "black","coral"))+
  theme(legend.position="bottom") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") 
dev.off()



### Not a long table alternative


# MA - This is easily computed using slide_dbl() which applies a function to “sliding” time windows. In this case, we use the mean() function with a window of size 5.
ma.centered <- ridership |>
  mutate(MAw12_12 = slide_dbl(Ridership, mean, .before = 5, .after = 6, .complete = TRUE),
         MAw12_2x12 = slide_dbl(MAw12_12 , mean, .before = 1, .after = 0, .complete = TRUE))  


ma.trailing <- ridership |>
  mutate(MATw12 = slide_dbl(Ridership, mean,.before = 11, .after = 0, .complete = TRUE))

# Create long table
ma.centered <- ma.centered |> select(Month, Ridership, MAw12_2x12)
ma.trailing <- ma.trailing |> select(Month, `MATw12`)
fc.table <- left_join(ma.centered, ma.trailing, by = "Month")
colnames(fc.table)[c(3,4)] <- c('Centered Moving Average', 'Trailing Moving Average')
fc.table <- fc.table |> gather(model, value, Ridership:`Trailing Moving Average`)

colors <- c("Ridership" = "black", "Centered Moving Average" = "#F8766D", "Trailing Moving Average" = "#00BFC4")
linetype <- c("Ridership" = "solid", "Centered Moving Average" = "solid", "Trailing Moving Average" = "dashed")
var_size <- c("Ridership" = 1, "Centered Moving Average" = 2, "Trailing Moving Average" = 3)

ridership   |>
  autoplot(Ridership) +
  geom_line(aes(y = Ridership,  colour="Ridership")) +
  geom_line(aes(y = `MAw12_2x12`,  colour="Centered Moving Average"), data = ma.centered , size=1.25) +
  geom_line(aes(y = MATw12,  colour="Trailing Moving Average"), data = ma.trailing, size=1.25, linetype = "dashed") +
  xlab("Time") + ylab("Ridership") +
  theme(legend.position="bottom") +
  scale_color_manual( values = colors) + 
  scale_linetype_manual( values = linetype) + 
  scale_size_manual(values=var_size)+
  labs(color = "Legend",
       linetype = "Legend") # +  scale_x_date(date_breaks = "2 years", date_labels = "%Y")
                                




