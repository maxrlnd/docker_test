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
    h4("Winter Finance Assessment"),
    p(paste0("Your Current Net Worth is: $", myOuts[currentYear, net.wrth])),
    p(paste0("Your Current Herd is: ", myOuts[currentYear, herd])),
    p(paste0("Your Bank Balance is: $", myOuts[currentYear, assets.cash])),
    p(paste0("Your range is currently at: ", myOuts[currentYear, forage.potential] * 100, "%")),
    p(paste0("You paid: $", myOuts[currentYear, cost.ins], " for insurance"))
  )
}

getJulyInfo <- function(){
  ## Establish current state variables 
  myYear <- startYear + currentYear - 1
  herd <- myOuts[currentYear, herd]
  zones <- station.gauge$zonewt
  
  ## Calcualte available forage
  forargeList <- vector("numeric", 3)
  if(currentYear == 1){
    zones <- zones * (1 - (0)/simRuns$forage.constant)
    
  }else{
    zones <- myOuts[currentYear - 1, zone.change] * zones * 
      (1 - (myOuts[currentYear - 1, Gt])/simRuns$forage.constant)
  }
  forageList <- vector("numeric", 3)
  forageList[1] <- whatIfForage(station.gauge, zones, myYear, herd, 1, 7, 11, "normal")
  forageList[2] <- whatIfForage(station.gauge, zones, myYear, herd, 1, 7, 11, "high")
  forageList[3] <- whatIfForage(station.gauge, zones, myYear, herd, 1, 7, 11, "low")
  
  ## Calculate cost of Adaptaiton
  adaptationInten <- sapply(forageList, CalculateAdaptationIntensity)
  adaptationInten <- c(adaptationInten, 1)
  adaptationCost <- sapply(adaptationInten, getAdaptCost, adpt_choice = "feed", pars = simRuns, 
                           days.act = 180, current_herd = herd)
  forageList <- round(forageList, 2) * 100
  adaptationCost <- round(adaptationCost, -2)
  tagList(
    h4("Summer Adaptation Investment Decision"),
    h5("Rainfall as a percent of normal (100 is average rainfall)"),
    tableOutput("julyRain"),
    p(paste0("If rainfall for the rest of the year is average your available forage will be ", forageList[1], "% of normal")),
    p(paste0("If rainfall for the rest of the year is above average your available forage will be ", forageList[2], "% of normal")),
    p(paste0("If rainfall for the rest of the year is below average your available forage will be ", forageList[3], "% of normal")),
    sliderInput("d1AdaptSpent", "How much hay, if any, do you want to purchase for your herd",
                min = 0, max = adaptationCost[4], value = 0, step = 100, width = "600px"),
    p(paste0("If rainfall over the next few months is normal, you should buy $", adaptationCost[1], 
            " of hay to get your herd in ideal shape for market.")),
    p(paste0("If rainfall over the next few months is above normal, you should buy $", adaptationCost[2], 
            " of hay to get your herd in ideal shape for market.")),
    p(paste0("If rainfall over the next few months is below normal, you should buy $", adaptationCost[3], 
            " of hay to get your herd in ideal shape for market."))
  )
}