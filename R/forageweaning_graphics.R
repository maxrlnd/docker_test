# Getting parameter values for weaning success given various forage potential values

#AdjWeanSuccess(stgg, zonewt, stzone, styear, noadpt = TRUE, normal.wn.succ, t)

library(ggplot2)

# Graph forage potential
years <- 1948:2015
forage.potential <- sapply(years, foragePWt, stgg = stgg, zonewt = zonewt, stzone = stzone)
forage.potential.years <- data.frame(years, forage.potential)
qplot(forage.potential, data = forage.potential.years, geom = "density")
ggplot(forage.potential.years, aes(x = forage.potential)) + geom_density()

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
