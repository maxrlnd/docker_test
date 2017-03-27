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

getWinterInfo <- function(currentYear){
  tagList(
    h4("Winter Finance Assessment"),
    p(paste0("Your Current Net Worth is: $", myOuts[currentYear, net.wrth])),
    p(paste0("Your Current Herd is: ", myOuts[currentYear, herd])),
    p(paste0("Your Bank Balance is: $", myOuts[currentYear, assets.cash])),
    p(paste0("Your range is currently at: ", myOuts[currentYear, forage.potential] * 100, "%")),
    p(paste0("You paid: $", myOuts[currentYear, cost.ins], " for insurance"))
  )
}

getJulyInfo <- function(currentYear){
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
  forageList[1] <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 7, 11, "normal")
  forageList[2] <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 7, 11, "high")
  forageList[3] <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 7, 11, "low")
  
  
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
    tableOutput(paste0("julyRain", currentYear)),
    p(paste0("If rainfall for the rest of the year is average your available forage will be ", forageList[1], "% of normal")),
    p(paste0("If rainfall for the rest of the year is above average your available forage will be ", forageList[2], "% of normal")),
    p(paste0("If rainfall for the rest of the year is below average your available forage will be ", forageList[3], "% of normal")),
    sliderInput(paste0("d", currentYear, "AdaptSpent"), "How much hay, if any, do you want to purchase for your herd",
                min = 0, max = adaptationCost[4], value = 0, step = 100, width = "600px"),
    p(paste0("If rainfall over the next few months is normal, you should buy $", adaptationCost[1], 
            " of hay to get your herd in ideal shape for market.")),
    p(paste0("If rainfall over the next few months is above normal, you should buy $", adaptationCost[2], 
            " of hay to get your herd in ideal shape for market.")),
    p(paste0("If rainfall over the next few months is below normal, you should buy $", adaptationCost[3], 
            " of hay to get your herd in ideal shape for market."))
  )
}

getCowSell <- function(forage, wean, currentYear){
  
  ## Calcualte how many cows to sell
  ## Establish current state variables
  herd <- myOuts[currentYear, herd]
  
  
  ## Calcuatle weaned Calves
  calvesAvailable <- floor(herd * wean)
  
  ## Calculate Standard Sales
  standardCowSale <- herd * simRuns$cull.num
  standardCalfSale <- calvesAvailable * simRuns$calf.sell
  tagList(
    h4("Fall Cow and Calf Sales"),
    sliderInput(paste0("calves", currentYear, "Sale"), "How many calves do you want to sell",
                min = 0, max = calvesAvailable, value =  standardCalfSale, step = 1, width = "600px"),
    sliderInput(paste0("cow", currentYear, "Sale"), "How many cows do you want to sell",
                min = 0, max = myOuts[currentYear, herd], value = standardCowSale, step = 1, width = "600px"),
    tags$li("If you sell X calves and Y cows, your herd will stay approximately the 
            same size as it is now. If you sell more, then your herd size will decrease. 
            If you sell fewer, then your herd size will grow."),
    tags$li("Selling a cow now means that you get more revenue this year, 
            but you will produce fewer calves next year."),
    tags$li("Keeping a calf now means that you get less revenue this year, 
            but that calf will start producing calves the year after next.")
    
  )
}

updateOuts <- function(wean, forage, calfSale, indem, adaptCost, cowSales, newHerd, zones, adaptInten, currentYear){
  
  print(zones)
  currentHerd <- myOuts[currentYear, herd]
  pastYear <- currentYear
  currentYear <- currentYear + 1
  myOuts[currentYear, yr := startYear + currentYear - 1]
  myOuts[currentYear, rev.calf := CalculateExpSales(herd = NA, wn.succ = NA, 
                                                     wn.wt = calfDroughtWeight(simRuns$normal.wn.wt, forage), 
                                                     calf.sell = calfSale, p.wn = simRuns$p.wn[pastYear])]
  myOuts[currentYear, rev.ins := indem$indemnity]
  myOuts[currentYear, rev.int := myOuts[pastYear, assets.cash] * simRuns$invst.int]
  myOuts[currentYear, rev.tot := myOuts[currentYear, rev.ins] + myOuts[currentYear, rev.int] + myOuts[currentYear, rev.calf]]
  myOuts[currentYear, cost.op := CalculateBaseOpCosts(herd = currentHerd, cow.cost = simRuns$cow.cost)]
  myOuts[currentYear, cost.ins := indem$producer_prem]
  myOuts[currentYear, cost.adpt := adaptCost]
  myOuts[currentYear, cost.int := ifelse(myOuts[pastYear, assets.cash] < 0,
                                          myOuts[pastYear, assets.cash] * -1 * simRuns$loan.int,
                                          0)]

  myOuts[currentYear, cost.tot := myOuts[currentYear, cost.op] + myOuts[currentYear, cost.ins] +
                                    myOuts[currentYear, cost.adpt] + myOuts[currentYear, cost.int]]
  myOuts[currentYear, profit := myOuts[currentYear, rev.tot] - myOuts[currentYear, cost.tot]]
  myOuts[currentYear, taxes := ifelse(myOuts[currentYear, profit] > 0, myOuts[currentYear, profit] * (0.124+0.15+0.04), 0)]
  myOuts[currentYear, aftax.inc := myOuts[currentYear, profit] - myOuts[currentYear, taxes]]
  myOuts[currentYear, cap.sales := cowSales * simRuns$p.cow]
  myOuts[currentYear, cap.taxes := myOuts[currentYear, cap.sales] * simRuns$cap.tax.rate]
  myOuts[currentYear, assets.cow := newHerd * simRuns$p.cow]
  myOuts[currentYear, assets.cash := myOuts[pastYear, assets.cash] + myOuts[currentYear, aftax.inc] +
                                      myOuts[currentYear, cap.sales] - myOuts[currentYear, cap.purch] -
                                      myOuts[currentYear, cap.taxes]]
  myOuts[currentYear, net.wrth := myOuts[currentYear, assets.cash] + myOuts[currentYear, assets.cow]]
  myOuts[currentYear, wn.succ := wean]
  myOuts[currentYear, forage.potential := forage]
  myOuts[currentYear, herd := newHerd]
  myOuts[currentYear, calves.sold := calfSale / floor(currentHerd * wean)]
  myOuts[currentYear, cows.culled := cowSales / currentHerd]
  myOuts[currentYear, zone.change := sum(zones)]
  myOuts[currentYear, Gt := ifelse(forage < 1, 
                                    1 - forage + forage * adaptCost/getAdaptCost(adpt_choice = "feed", 
                                                                                 pars = simRuns, 
                                                                                 days.act = 180, 
                                                                                 current_herd = currentHerd, 
                                                                                 intens.adj = adaptInten),  
                                    1 - forage)]
}