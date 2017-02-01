# Getting parameter values for weaning success given various forage potential values

#AdjWeanSuccess(stgg, zonewt, stzone, styear, noadpt = TRUE, normal.wn.succ, t)

library(ggplot2)

# Graph forage potential
years <- 1948:2015
forage.potential.years <- sapply(years, foragePWt, stgg = stgg, zonewt = zonewt, stzone = stzone)
forage.potential.years <- data.frame(years, forage.potential = forage.potential.years)
qplot(forage.potential, data = forage.potential.years, geom = "density")
ggplot(forage.potential.years, aes(x = forage.potential)) + geom_density()

wn.succ_yr1 <- normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential.years$forage.potential)*2))) 
wn.succ_yr2 <- normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential.years$forage.potential))))
weaning_weight <- sapply(forage.potential.years$forage.potential, calfDroughtWeight,  normal.wn.wt = normal.wn.wt)

forage.weaning <- data.frame(forage.potential = forage.potential.years$forage.potential, wn.succ_yr1, wn.succ_yr2, weaning_weight)
ggplot(forage.weaning) + geom_density(aes(x = forage.potential)) 
ggplot(forage.weaning) + geom_density(aes(x = weaning_weight))
