#################
# Code to create Figure 3.3 and Figure 3.4

Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character(Amtrak.data$Month))) |>
  as_tsibble(index = Month)

train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

ridership.lm <- train.ridership |>
  model(trend_model = TSLM(Ridership ~  trend()  + I(trend()^2)))
augment(ridership.lm)

ridership.lm.pred <- ridership.lm |> forecast(h = nrow(valid.ridership))
names(ridership.lm.pred)
fc.resid <- valid.ridership$Ridership - ridership.lm.pred$.mean

fc.resid <- data.frame(ridership.lm.pred$Month,fc.resid) 
colnames(fc.resid)[1] <- "Month"
fc.resid <- fc.resid |>
  as_tsibble(index = Month)

#### Figure 3.4:
p.model <- ridership  |>
  autoplot(Ridership) +
  geom_line(aes(y = .fitted, colour = .model), data = fitted(ridership.lm), size = 1.25, color = "blue1") +
  autolayer(ridership.lm.pred, level = NULL, linetype = "dashed", size = 1.25) +
  xlab("Time") + ylab("Ridership")  +
  theme(legend.position = "none") +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55") +  
  annotate(geom="text", x=yearmonth("2002-Aug"), y=2290, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55") +  
  annotate(geom = "text", x = yearmonth("1996-Aug"), y=2290, label = "Training", color = "grey37") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")



p.reid <- ridership.lm|> augment() |> 
  autoplot(.resid) + 
  xlab("Time") + ylab("Residuals") +
  autolayer(fc.resid ) +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype = "solid", color = "grey55", size = 0.6)+
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")

  
pdf("Plots/AmtrakFig_3_4_3e.pdf",height=5.2,width=8)
grid.arrange(p.model, p.reid , nrow = 2)
dev.off()


#### Figure 3.5 (histogram):

pdf("Plots/AmtrakFig_3_5_3e.pdf",height=4,width=6)
fc.resid |> 
  ggplot(aes(x = fc.resid)) + # errors of the validation set
  geom_histogram(bins = 7, color = "black", fill = "white") +
  xlab("Forecast Error") + ylab("Frequency")
dev.off()
