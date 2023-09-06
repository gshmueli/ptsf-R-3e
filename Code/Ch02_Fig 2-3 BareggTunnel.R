#################
# Code to create Figure 2-3

BareggTunnel <- read.csv("Data/BareggTunnel.csv")
BareggTunnel <- BareggTunnel |>
  mutate(Day = dmy(as.character( BareggTunnel$Day)) ) |>
  as_tsibble(index = Day)

pdf("Plots/BareggTunnelFig_2_3_3e.pdf",height=4,width=6)

p.full <- BareggTunnel |>
  autoplot(Number.of.vehicles) +
  xlab("Time") + ylab("Number of Vehicles") 

p.zoom <- BareggTunnel |>
  filter_index("2004-02-01" ~ "2004-05-31")   |>
  autoplot(Number.of.vehicles) +
  scale_x_date(date_labels = "%m-%Y") +
  xlab("Time") + ylab("Number of Vehicles")

grid.arrange(p.full, p.zoom, nrow = 2)
dev.off()
