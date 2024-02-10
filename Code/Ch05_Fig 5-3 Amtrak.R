#################
# Code for creating Figure 5-3

Amtrak.data <- read.csv("Data/Amtrak.csv")

ridership <- Amtrak.data |>
  dplyr::mutate(Month = yearmonth(as.character( Amtrak.data$Month))) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

ma.trailing <- train.ridership |>
  mutate(MATw12 = slide_dbl(Ridership, mean, .size = 12,  .before = 11, .after = 0 , .complete =TRUE))

last.ma <- tail(ma.trailing, 1)
ma.pred <-  bind_cols(valid.ridership, rep(last.ma$MATw12, nrow(valid.ridership)))  |> 
           select(-"Ridership")
names(ma.pred)[2] <- "fitted"

p.ma <- ridership  |>
  autoplot(Ridership) +
  geom_line(aes(y = MATw12), data = ma.trailing, size=1.0, color="blue1") +
  autolayer(ma.pred, level = NULL, linetype = "dashed", size = 1.25, color = "blue1") +
  xlab("Time") + ylab("Ridership")  +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55") + 
  annotate(geom = "text", x = yearmonth("2002-Aug"), y=2290, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color="grey55") +  
  annotate(geom = "text", x = yearmonth("1996-Aug"), y = 2290, label = "Training", color = "grey37")  +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")

pdf("Plots/AmtrakFig_5_3_3e.pdf", height=4, width=6)
p.ma 
dev.off()
