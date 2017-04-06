# Getting parameter values for weaning success given various forage potential values

#AdjWeanSuccess(stgg, zonewt, stzone, styear, noadpt = TRUE, normal.wn.succ, t)

library(ggplot2)


#lookup to see how to save a graphic to a certain directory, for overwriting. 
# Forage potential over years
years <- 1949:2015
foragePWt(station.gauge = station.gauge, styear = 2015, herd = herd, carryingCap = 1)
forage <- sapply(1949:2015, foragePWt, station.gauge = station.gauge, herd = herd, carryingCap = 1)
forage_df <- data.frame(years, forage)

#Evan Test Changes
ggplot(forage_df, aes(x=forage, y = years)) + geom_point() +
  stat_density_2d() +
  labs(title = 'Forage Potential over 1949-2015', x = "Forage Potential", y = "Years")




ggplot(forage_df, aes(x=forage)) + geom_density()

ggplot(forage_df, aes(x=forage, y = years)) + geom_smooth(se = FALSE) + geom_jitter()




#understanding what kinds of rain levels are leading to what kinds of results in the model. Maybe we need to bing in additional. Forage will be x value, see how things change to that. 

#forage_plot = lm(years ~ forage, data = forage_df, lty = 2)

#plot(forage_plot, main = 'R Regression Line of Data Points')


# How is forage related to weaning weights? Assuming no adaptation:
carryingCap <- 1
avg_wean_weights <- sapply(1949:2015, calfWeanWeight, 1)  # This isn't working...
wean_weights_df <- data.frame(years, forage, avg_wean_weights)

jitter1 = position_jitter(.05)

ggplot(wean_weights_df, aes(x=forage, y = avg_wean_weights)) + 
  geom_jitter(position = jitter1) +
  geom_smooth(se = FALSE, ) +
  labs(title = 'Forage and Average Weaning Weight', x = "Forage", y = "Average Weaning Weight (lb)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(wean_weights_df, aes(years)) + 
  geom_bar()

str(wean_weights_df)

# How is forage related to weaning percentage? Assuming no adaptation:
#Error for me: "Error in FUN(X[[i]], ...) : object 'normal.wn.succ' not found"
#Solved - Need to run normal.wn.succ in testvars.R Script"
wean_percentage <- lapply(forage, AdjWeanSuccess, noadpt = TRUE, normal.wn.succ, 3)
wean_percentage_yr1 <- sapply(wean_percentage, `[[`, 1)
wean_percentage_yr2 <- sapply(wean_percentage, `[[`, 2)
wean_percentage_df <- data.frame(years, forage, wean_percentage_yr1, wean_percentage_yr2)

library(tidyr)
wean_percentage_df %>%
  gather(which_year, wean_pct, -years, -forage) %>%
  
  ggplot(aes(forage, wean_pct, color = which_year)) + 
  geom_point() +
  labs(title = 'Forage and Weaning percent', x = 'forage', y = 'weaning percentage')

#first colum n -were you put the column names from the data frame
#2nd column - where do you put the values
#pipe operator %>% takes what on left, takes it as inptu to the next functgion (stuff to the right of %)
#(Fg(x))
#x%>g%


#Not right, need to add axes and stuff. Also cleanup the code
ggplot(wean_percentage_df, aes(forage,wean_percentage_yr1)) +
  theme_minimal() +
  labs(title = 'Forage and Weaning Percentage', x = "Forage Potential", y = "Weaning Percentage") +
  geom_point(aes(forage,wean_percentage_yr2), col = 'gold', size = 3) +
  geom_point(size = 4, shape = 1)


head(wean_percentage_yr2)

#\facet_grid(. ~wean_percentage)


# How does weaning percentage relate to herdSize?
results <- data.frame(herd = rep(600, 10), cows.culled = rep(.16, 10), 
                      wn.succ = rep(0.80, 10), calves.sold = rep(.77, 10))
for (i in 1:10) {
  herd <- round(getHerdSize(results_1ya = results[i+1,], results_2ya = results[i,], deathRate = .04), 0)
  results[i+2, 1] <- herd
}




weanPercByYears <- function(start_yr, end_yr) {
  wean_percentage_df %>%
    filter(years <= end_yr & years >= start_yr) -> wean_perc_decade
  n <- end_yr - start_yr
  wean_perc <- wean_perc_decade$wean_percentage_yr1
  for (i in 1:n) {
    if(wean_perc_decade$wean_percentage_yr2[i] < wean_perc_decade$wean_percentage_yr1[i + 1]) {
      wean_perc[i + 1] <- wean_perc_decade$wean_percentage_yr2[i]
    }
  }
  wean_perc
}



herdWeanPerc <- function(wean_perc, start_herd = 600, cull = .16,
                         calves_sold = 0.77) {
  n <- length(wean_perc)
  results <- data.frame(herd = rep(start_herd, n), cows.culled = rep(cull, n), 
                        wn.succ = wean_perc, calves.sold = rep(calves_sold, n))
  for (i in 1:n) {
    herd <- round(getHerdSize(results_1ya = results[i+1,], results_2ya = results[i,], deathRate = .04), 0)
    results[i+2, 1] <- herd
  }
  results[1:n,]
}

#What happesn to the herd size if we don't do adaptation or anything else?
#rainfall that's relevant for forage. 
#Don't use forage term, Placeholder/rainfall weighted growth
#Herd stuff is here, 3 different decades. 
wean_perc_2000s <- weanPercByYears(2001, 2010)
herd2000s <- herdWeanPerc(wean_perc = wean_perc_2000s)
forage_df %>%
  filter(years >= 2001 & years <= 2010) -> forage2000s
herd2000s <- cbind(herd2000s, forage2000s)  

wean_perc_1990s <- weanPercByYears(1991, 2000)
herd1990s <- herdWeanPerc(wean_perc = wean_perc_1990s)
forage_df %>%
  filter(years >= 1991 & years <= 2000) -> forage1990s
herd1990s <- cbind(herd1990s, forage1990s)  

wean_perc_1980s <- weanPercByYears(1981, 1990)
herd1980s <- herdWeanPerc(wean_perc = wean_perc_1980s)
forage_df %>%
  filter(years >= 1981 & years <= 1990) -> forage1980s
herd1980s <- cbind(herd1980s, forage1980s)  

head(herd1980s)
#PLot ofr 1980's
#make y axis larger - show 0

#Combine all of these together!
#year 1-10. 
herd1980splot = ggplot(herd1980s, aes(years,herd), col =) + 
  geom_path(linetype = 4) + 
  geom_point(col = 'blue', size = 3) +
  theme_bw() +
  scale_x_continuous("Years", limits = c(1981, 1990), breaks = seq(1981,1990)) +
  scale_y_continuous("Herd Size (AU)", limits = c(560, 600)) +
  labs(title = 'Herd Size from 1980-1990', x = "Years", y = "Herd Size") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('herd1980splot.jpg', plot = herd1980splot, device = 'jpg', path = 'C:/Users/EvanLih/drought_decision_model/figs/')


herd1990splot = ggplot(herd1990s, aes(x = years, y = herd)) + geom_point() + 
  geom_path(linetype = 4) +
  geom_point(col = 'blue', size = 3) +
  theme_bw() +
  scale_x_continuous("Years", limits = c(1991, 2000), breaks = seq(1990,2000)) +
  labs(title = 'Herd Size from 1990-2000', x = "Years", y = "Herd Size") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('herd1990splot.jpg', plot = herd1990splot, device = 'jpg', path = 'C:/Users/EvanLih/drought_decision_model/figs/')


herd2000splot = ggplot(herd2000s, aes(x = years, y = herd)) + geom_point() +
  geom_line(linetype = 4) + 
  geom_point(col = 'blue', size = 3) +
  ylim(560,600) +
  theme_bw() +
  scale_x_continuous("Years", limits = c(2001,2007), breaks = seq(2000,2007)) +
  scale_y_continuous("Herd Size (AU)", limits = c(550, 600)) +
  labs(title = 'Herd Size from 2000s', x = "Years", y = "Herd Size (AU)") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave('herd2000splot.jpg', plot = herd2000splot, device = 'jpg', path = 'C:/Users/EvanLih/drought_decision_model/figs/')



combinedyearstestplot = ggplot(herdstest, aes(x = years, y = herd)) + geom_point() +
  geom_line(aes(group = years)) +
  ylim(560,600) +
  theme_bw() +
  scale_x_continuous("Years", limits = c(1980,2010)) +
  labs(title = 'Herd Size from 2000s', x = "Years", y = "Herd Size (AU)") +
  theme(plot.title = element_text(hjust = 0.5)) 
  
ggsave('combinedyearstestplost.jpg', plot = combinedyearstestplot, device = 'jpg', path = 'C:/Users/EvanLih/drought_decision_model/figs/')

herdstest = rbind(herd2000s, herd1980s, herd1990s)

str(herdstest)

testyears = c(1,2,3,4,5,6,7,8,9,10)



