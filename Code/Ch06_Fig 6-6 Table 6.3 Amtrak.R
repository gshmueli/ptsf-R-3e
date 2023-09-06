################
# Code to create Table 6.3 & Figure 6.6


Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  dplyr::mutate(Month = yearmonth(as.character( Amtrak.data$Month)) ) %>%
  as_tsibble(index = Month)

train.ridership <- ridership %>% filter_index( ~ "2001 Mar") 
valid.ridership <- ridership %>% filter_index( "2001 Apr" ~ .)

# Seasonality
train.lm.season <- train.ridership %>% model(TSLM(Ridership ~ season()))

report(train.lm.season)

## Match season()year i to month:

coef(train.lm.season)$term


### Figure 6.6

pred.values.train.season <- fitted.values(train.lm.season) # forecasts in training
fc.lm.season <- train.lm.season %>% forecast(h=36)    # forecasts in validation
fc.resid.season <- valid.ridership$Ridership - fc.lm.season$.mean  # errors in validation
  
fc.resid.season <- data.frame(valid.ridership$Month, fc.resid.season) 
colnames(fc.resid.season)[1] <- "Month"
fc.resid.season <- fc.resid.season %>%
  as_tsibble(index = Month)


p.model <- ridership  %>%
  autoplot(Ridership) + 
  geom_line(aes(y=.mean), data = fc.lm.season,  colour="blue1", linetype = "dashed", size = 1) +
  autolayer( pred.values.train.season,  level = NULL, colour="blue1", size = 1) +
  xlab("Time") + ylab("Ridership")  +
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-May"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color="grey55")+  
  annotate(geom="text", x=yearmonth("2003-Jan"), y=2300, label="Validation", color="grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color="grey55")+  
  annotate(geom="text", x=yearmonth("1995-Aug"), y=2300, label="Training", color="grey37") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")

p.resid <- train.lm.season %>% augment() %>% 
  autoplot(.resid) +
  autolayer(fc.resid.season ,  level = NULL)+
  xlab("Time") + ylab("Residuals")  +
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6)+
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")


pdf("Plots/AmtrakFig_6_6_3e.pdf",height=5.5,width=8)
grid.arrange(p.model, p.resid , nrow = 2)
dev.off()


