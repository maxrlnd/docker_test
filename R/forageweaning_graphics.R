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

 