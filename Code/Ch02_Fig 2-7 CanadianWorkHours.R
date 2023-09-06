#################
# Code to create Figure 2-7 (Problem #4)

cwd <- read.csv("Data/CanadianWorkHours.csv")

cwd <- cwd |>
  as_tsibble(index = Year)

pdf("Plots/AmtrakFig_2_7_3e.pdf",height=4,width=6)
cwd |>
  autoplot(Hours.per.week) +
  xlab("Year") + ylab("Hours Per Week") 
dev.off()


