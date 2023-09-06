#################
# Code to create Figure 9.5

Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data %>%
  dplyr::mutate(Month = yearmonth(as.character(Amtrak.data$Month))) %>%
  as_tsibble(index = Month)

train.ridership <- ridership %>% filter_index( ~ "2001 Mar") 
valid.ridership <- ridership %>% filter_index( "2001 Apr" ~ .)


set.seed(201)

ridership.nnetar <- train.ridership %>% 
                    model(nn = NNETAR(Ridership ~ AR(p = 11, P = 1, period = 12), n_nodes = 7, n_networks = 20))
report(ridership.nnetar)
pred.values.nnetar <- fitted.values(ridership.nnetar)
View(pred.values.nnetar)

fc <- ridership.nnetar %>% forecast(h = 36, times = 100)

dplyr::bind_rows(
  accuracy(ridership.nnetar),
  accuracy(fc, as_tsibble(ridership))
)

# RMSE in the validation period
accuracy(fc, as_tsibble(ridership))$RMSE

#### Figure 9.5 (without training/validation arrows and marks)

ridership %>%
  autoplot(Ridership) +
  geom_line(aes(y = .mean), data = fc, linetype = "dashed", size = 1, colour = "blue1")+
  geom_line(aes(y = .fitted), data = pred.values.nnetar, size = 1, colour = "blue1")+
  xlab("Time") + ylab("Ridership") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")


#### Figure 9.5 with training/validation arrows and marks

pdf("Plots/AmtrakFig_9_5_3e.pdf",height=4,width=6)
ridership %>% 
  autoplot(Ridership) +
  geom_line(aes(y = .mean), data = fc, linetype = "dashed", size = 1, colour = "blue1")+
  geom_line(aes(y = .fitted), data = pred.values.nnetar, size = 1, colour = "blue1")+
  xlab("Time") + ylab("Ridership")  + 
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  geom_vline(xintercept = as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size = 0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55")+  
  annotate(geom = "text", x = yearmonth("2002-Aug"), y = 2290, label = "Validation", color="grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55")+  
  annotate(geom = "text", x = yearmonth("1996-Aug"), y = 2290, label = "Training", color = "grey37")
dev.off()

# Performance of NNETAR without specifying parameters:
set.seed(201)

ridership.optimal <- train.ridership %>% 
  model(nn = NNETAR(Ridership))

fc.optimal <- ridership.optimal %>% forecast(h = 36, times = 100)

dplyr::bind_rows(
  accuracy(ridership.optimal),
  accuracy(fc.optimal, as_tsibble(ridership))
)
