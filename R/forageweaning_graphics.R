# Getting parameter values for weaning success given various forage potential values

#AdjWeanSuccess(stgg, zonewt, stzone, styear, noadpt = TRUE, normal.wn.succ, t)

library(ggplot2)

# Forage potential over years
years <- 1949:2015
foragePWt(station.gauge = station.gauge, styear = 2015, herd = herd, carryingCap = 1)
forage <- sapply(1949:2015, foragePWt, station.gauge = station.gauge, herd = herd, carryingCap = 1)
forage_df <- data.frame(years, forage)

# How is forage related to weaning weights? Assuming no adaptation:
carryingCap <- 1
avg_wean_weights <- sapply(1949:2015, calfWeanWeight, 1)  # This isn't working...
wean_weights_df <- data.frame(years, forage, avg_wean_weights)

# How is forage related to weaning percentage? Assuming no adaptation:
wean_percentage <- lapply(forage, AdjWeanSuccess, noadpt = TRUE, normal.wn.succ, 3)
wean_percentage_yr1 <- sapply(wean_percentage, `[[`, 1)
wean_percentage_yr2 <- sapply(wean_percentage, `[[`, 2)
wean_percentage_df <- data.frame(years, forage, wean_percentage_yr1, wean_percentage_yr2)










####### Old Code




forage.drought <- ifelse(forage.potential < 1, forage.potential, 1)
wn.succ_yr1 <- normal.wn.succ - (1 - forage.drought)/9
wn.succ_yr2 <- normal.wn.succ - (1 - forage.drought)/3
weaning_weight <- sapply(forage.drought, calfDroughtWeight,  normal.wn.wt = normal.wn.wt)

forage.weaning <- data.frame(forage.potential = forage.drought, wn.succ_yr1, wn.succ_yr2, weaning_weight)

# PLOTS
ggplot(forage.weaning) + geom_density(aes(x = forage.potential)) 

ggplot(forage.weaning) + geom_density(aes(x = weaning_weight))
ggplot(forage.weaning) + geom_line(aes(x = forage.potential, y = weaning_weight), color = "brown4", size = 1.1) + 
  ylab("Weaning Weight") + xlab("Forage Production") +
  ggtitle("Forage and Weaning Weight")

#EVAN: These two densities need to be different colors (either fill or the lines). Ideally with a legend that indicates which is which.
# The X-axes should just be "Weaning Success"
ggplot(forage.weaning) + geom_density(aes(x = wn.succ_yr1)) + geom_density(aes(x = wn.succ_yr2)) 

ggplot(forage.weaning) + 
  geom_line(aes(x = forage.potential, y = wn.succ_yr1), color = "firebrick", size = 1.1) + 
  geom_line(aes(x = forage.potential, y = wn.succ_yr2), color = "darkgoldenrod2", size = 1.1) + 
  ylab("Weaning Success") + xlab("Forage Production") + ggtitle("Weaning Success, Year 1 (red) and Year 2 (yellow)")



forage.drought <- ifelse(forage)
wn.succ_yr1 <- ifelse(forage.potential.years$forage.potential < 1, 
                      normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential.years$forage.potential)*2))), 
                      0.94)
wn.succ_yr2 <- ifelse(forage.potential.years$forage.potential < 1,  
                      normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential.years$forage.potential)))),
                      0.94)
weaning_weight <- sapply(forage.potential.years$forage.potential, calfDroughtWeight,  normal.wn.wt = normal.wn.wt)

forage.weaning <- data.frame(forage.potential = forage.drought, wn.succ_yr1, wn.succ_yr2, weaning_weight)
