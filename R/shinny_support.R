

getJulyInfo <- function(currentYear){
  
  "
  Function: getJulyInfo
  Description: Calculate available and predited forage in july, create a
    ui to display info and allow user to select adaptation level
  
  Inputs:
  currentYear = the current year
  
  Outputs:
  tagList = list of ui elements to be displayed
  "
  
  ## Establish current state variables 
  myYear <- startYear + currentYear - 1
  herd <- myOuts[currentYear, herd]
  zones <- station.gauge$zonewt
  print(myYear)
  
  ## Calcualte available forage for normal, high, and low precip over remaining months
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
  ## Round outputs for display
  forageList <- round(forageList, 2) * 100
  adaptationCost <- round(adaptationCost, -2)
  
  ## Create taglist showing all adpatation
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
  "
  Function: getCowSell
  Description: create ui for a user to select how many cow and calves to sell
  
  Inputs:
  forage = the available forage after adaptation has been applied
  wean = wean success
  currentYear = the current year 
  
  Outputs:
  tagList = UI elements in a tag list
  "
  
  ## Calcualte how many cows to sell
  ## Establish current state variables
  herd <- myOuts[currentYear, herd]
  
  
  ## Calcuatle weaned Calves
  calvesAvailable <- floor(herd * wean)
  
  ## Calculate Standard Sales
  standardCowSale <- herd * simRuns$cull.num
  standardCalfSale <- calvesAvailable * simRuns$calf.sell
  
  ## Create UI elements
  tagList(
    h4("Fall Cow and Calf Sales"),
    sliderInput(paste0("calves", currentYear, "Sale"), "How many calves do you want to sell",
                min = 0, max = calvesAvailable, value =  standardCalfSale, step = 1, width = "600px"),
    sliderInput(paste0("cow", currentYear, "Sale"), "How many cows do you want to sell",
                min = 0, max = myOuts[currentYear, herd], value = standardCowSale, step = 1, width = "600px"),
    tags$li(paste("If you sell", standardCalfSale, "calves and",  standardCowSale,  "cows, your herd will stay approximately the 
            same size as it is now. If you sell more, then your herd size will decrease. 
            If you sell fewer, then your herd size will grow.")),
    tags$li("Selling a cow now means that you get more revenue this year, 
            but you will produce fewer calves next year."),
    tags$li("Keeping a calf now means that you get less revenue this year, 
            but that calf will start producing calves the year after next.")
    
  )
}


updateOuts <- function(wean, forage, calfSale, indem, adaptCost, cowSales, newHerd, zones, adaptInten, currentYear){
  "
  Function: updateOuts
  Description: Function to update myOuts after a year of the simulation has been completed
  
  Inputs:
  wean = weaning success of calves
  forage = percent of forage avaialble after adaption has been applied
  calfSale = number of calves being sold
  indem = data.table containing premimum and indemnity info
  adaptCost = amount spent on adaptation
  cowSales = number of cows being sold
  newHerd = size of next year's herd based on cowsales and calf sales from 2ya
  zones = zone information based on precip/adaptation/over grazing from previous year
  adaptInten = intensity of adaptation
  currentYear = the current year
  
  Outputs:
  myOuts = data.table of all outputs
  "
  
  print(zones)
  currentHerd <- myOuts[currentYear, herd]
  pastYear <- currentYear
  currentYear <- currentYear + 1
  myOuts[currentYear, yr := startYear + pastYear - 1]
  myOuts[currentYear, rev.calf := CalculateExpSales(herd = NA, wn.succ = NA, 
                                                     wn.wt = calfDroughtWeight(simRuns$normal.wn.wt, forage), 
                                                     calf.sell = calfSale, p.wn = simRuns$p.wn[pastYear])]
  print(paste("wean", calfDroughtWeight(simRuns$normal.wn.wt, forage)))
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

createNewYr <- function(year){
  "
  Function: createNewYr
  Description: create a list of 1 tabPanel for specified year
  
  Inputs:
  year = year of simulation (not calendar year)
  
  Outputs:
  list of 1 tabset panel with year UI
  "
  list(tabPanel(paste("Year", year),
           fluidRow(
             column(8,
                    uiOutput(paste0("winterInfo", year)),
                    fluidRow(column(12, style = "background-color:white;", div(style = "height:50px;"))),
                    uiOutput(paste0("decision", year)),
                    uiOutput(paste0("insuranceUpdate", year)),
                    uiOutput(paste0("cowSell", year))
             ),
             column(2,
                    fluidRow(column(12, style = "background-color:white;", div(style = "height:170px;"))),
                    actionButton(paste0("year", year, "Start"), "Begin Simulation"),
                    fluidRow(column(12, style = "background-color:white;", div(style = "height:500px;"))),
                    uiOutput(paste0("continue", year)),
                    fluidRow(column(12, style = "background-color:white;", div(style = "height:700px;"))),
                    uiOutput(paste0("sellButton", year))
             )
           )
  ))
}