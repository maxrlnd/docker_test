filter.titles <- list("opt" = c("Buy Feed", "Drought and No Adaptation", "No Drought", "Rent Pasture", "Sell Pairs"), 
                      "yr" = c(1950:2017), "ins" = c("With Insurance","No Insurance"),
                      "cols" = c("opt", "yr", "ins", "rev.calf", "rev.ins", "rev.int", "rev.tot", 
                                 "cost.op", "cost.ins", "cost.int", "cost.tot", "profit", "taxes", 
                                 "aftax.inc", "cap.sales", "cap.purch", "cap.taxes", "assets.cow", 
                                 "assets.cash", "net.wrth", "sim.index"))
shapeDrawn <- F

advanceCurrentYear <- function(){
  currentYear <<- currentYear + 1
}

getWinterInfo <- function(){
  tagList(
    h4(paste("Your Current Net Worth is: $", myOuts[currentYear, net.wrth])),
    h4(paste("Your Current Herd is: ", myOuts[currentYear, herd])),
    h4(paste("Your Bank Balance is: $", myOuts[currentYear, assets.cash])),
    h4(paste("Your range is currently at: ", myOuts[currentYear, forage.potential] * 100, "%")),
    h4(paste("You paid: $", myOuts[currentYear, cost.ins], " for insurance"))
  )
}

getJulyInfo <- function(){
  myYear <- startYear + currentYear - 1
  herd <- myOuts[currentYear, herd]
  zones <- station.gauge$zonewt * myOuts[currentYear, zone.change]
  # currentForage <- foragePWt(station.gauge, myYear, herd, herd/carryingCapacity, )
  normalForage <- whatIfForage(station.gauge, zones, myYear, herd, 1, 7, 11, "normal")
  highForage <- whatIfForage(station.gauge, zones, myYear, herd, 1, 7, 11, "high")
  lowForage <- whatIfForage(station.gauge, zones, myYear, herd, 1, 7, 11, "low")
  tagList(
    h4("Rainfall as a percent of normal (100 is average rainfall)"),
    tableOutput("julyRain"),
    p(paste("Your forage is at ",0,"% of normal")),
    p(paste("Normal ", normalForage)),
    p(paste("high ", highForage)),
    p(paste("low ", lowForage))
    
  )
}