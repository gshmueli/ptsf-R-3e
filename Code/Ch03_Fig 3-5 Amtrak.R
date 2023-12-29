#################
# Code to create Figure 3-5

Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character( Amtrak.data$Month))) |>
  as_tsibble(index = Month)


train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)


ridership.lm <- train.ridership |>
  model(trend_model = TSLM(Ridership ~  trend() + I(trend()^2)))

ridership.lm.pred <- ridership.lm |> forecast(h = nrow(valid.ridership))
names(ridership.lm.pred)

augment(ridership.lm)

ridership.lm.pred |>
  autoplot(ridership, level = 95) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")


fc <- ridership.lm.pred |>
  autoplot(ridership, level=95) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  geom_line(aes(y = .fitted, colour = .model), data = fitted(ridership.lm), size = 1.25, color = "blue1") +
  autolayer(ridership.lm.pred, alpha = 0.5, level = NULL, linetype = "dashed", size = 1.25) +
  xlab("Time") + ylab("Ridership")  +
  theme(axis.text.y = element_text(angle = 90, hjust = 1)) + # AXIS TICKS: SET AND ROTATE
  theme(legend.position = "none") +
  # + guides(colour = guide_legend(title = "Model")) +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6)+
#  geom_vline(xintercept = yearmonth("2004-April"), linetype="solid", color = "grey55", size = 0.6)+
  geom_segment(aes(x = yearmonth("2001-May"), y = 2500, 
                   xend = yearmonth("2004-Mar"), yend = 2500),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55")+  # error Validation
  annotate(geom="text", x=yearmonth("2002-Aug"), y=2540, label="Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2500, 
                   xend = yearmonth("2001-Mar"), yend = 2500),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color="grey55")+  # error Validation
  annotate(geom="text", x=yearmonth("1996-Aug"), y = 2540, label = "Training", color = "grey37")


pdf("Plots/AmtrakFig_3_6_3e.pdf",height=4,width=6)
fc 
dev.off()
