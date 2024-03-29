#################
# Code to create Figure 3.6

Tumblr.data <- read.csv("Data/Tumblr.csv")

tumblr <- Tumblr.data |>
  mutate(Month = yearmonth(as.character(Tumblr.data$Month))) |>
  as_tsibble(index = Month)

people <- tumblr |> 
              select(Month , People.Worldwide) |>
              mutate(People.Worldwide = People.Worldwide/1000000)

people |> autoplot(People.Worldwide) +
  ylab("World wide population (millions)") + xlab("Month")

View(Tumblr.data)

# Train models 
fit.ets.AAN <- people |> #simple exponential smoothing with additive errors
  model(ETS(People.Worldwide ~ error("A") + trend("A") + season("N"), opt_crit = "mse"))

fit.ets.MMN <- people |> 
  model(ETS(People.Worldwide ~ error("M") + trend("M") + season("N"), opt_crit = "mse"))

fit.ets.MMdN <- people |> 
  model(ETS(People.Worldwide ~ error("M") + trend("Md") + season("N"), opt_crit = "mse"))

# Forecast
fc.ets.AAN <- fit.ets.AAN |>
  forecast(h = 115)

fc.ets.MMN <- fit.ets.MMN |>
  forecast(h = 115)

fc.ets.MMdN <- fit.ets.MMdN |>
  forecast(h = 115)

# Plot
AAN.plot <- fc.ets.AAN |>
  autoplot(people, level = c(20,40,60,80)) +
 # geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(fit.ets.AAN)) +
  ylab("World wide population (millions)") + xlab("Month") +
  theme(legend.position = "none") + 
  ylim(0,1200)

MMN.plot <- fc.ets.MMN |>
  autoplot(people, level = c(20,40,60,80)) +
  ylab("World wide population (millions)") + xlab("Month") +
  theme(legend.position = "none") + 
  ylim(0,1200)

MMdN.plot <- fc.ets.MMdN |>
  autoplot(people, level = c(20,40,60,80)) +
  ylab("World wide population (millions)") + xlab("Month") +
  theme(legend.position = "none") + 
  ylim(0,1200)

pdf("Plots/TumblrFig_3_7_3e.pdf",height=5.2,width=8)
grid.arrange(AAN.plot, MMN.plot, MMdN.plot, ncol = 3)
dev.off()
