dataPath <- "c:/Users/wtravis/Documents/Earth Analytics-5100-Project Risk/netCDF/"
files <- dir(dataPath)
files <- files[files != "desktop.ini"]
temp <- brick(paste0(dataPath,files[1]))


gridID <- 25002
gridRow <- 37
gridCol <- 102
monthlyData <- brick("c:/Users/admcc/Downloads/precip.V1.0.mon.mean.nc")
monthlyMeans <- brick("c:/Users/admcc/Downloads/precip.V1.0.mon.ltm.nc")
names(monthlyData)
temp <- subset(monthlyData, 1, 1)
temp1 <- as.matrix(temp)
load("c:/Users/wtravis/Documents/Earth Analytics-5100-Project Risk/masterBook.RData")
monthlyNames <- names(monthlyData)
years <- substr(monthlyNames, 2, 5)
years <- as.numeric(years)
years <- unique(years)
noaaRain <- data.table(matrix(0, nrow = length(years), ncol = 13))
setnames(noaaRain, names(station.gauge$stgg))
noaaRain[, Year := years]

avgPrecip <- vector("numeric", 12)
for(i in 1:12){
  currentRaster <- as.matrix(subset(monthlyMeans, i,i))
  avgPrecip[i] <- currentRaster[gridRow, gridCol]
}

for(i in 1:length(monthlyNames)){
  currentRaster <- as.matrix(subset(monthlyData, i, i))
  year <- as.numeric(substr(monthlyNames[i], 2, 5))
  month <- as.numeric(substr(monthlyNames[i], 7, 8))
  day <- as.numeric(substr(monthlyNames[i], 10,11))
  if(day != 1){
    year <- ifelse(month == 12, year + 1, year)
    month <- ifelse(month == 12, 1, month + 1)  
  }
  noaaRain[which(noaaRain == year), month + 1] <- currentRaster[gridRow, gridCol]
}
temp <- noaaRain
temp[, Year := as.character(Year)]
temp[1, Year := "AVG"]
temp[1, 2:13 := as.list(avgPrecip)] 
rbind(temp, t(c("AVG", avgPrecip)))
noaaRain <- temp
