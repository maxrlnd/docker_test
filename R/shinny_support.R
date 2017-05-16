

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

  #colnames(forageList) <- c("Rain", "Forage", "Cost")
  #rownames(forageList) <- c("Above average", "Average", "Below average")
  
  
  ## Calculate cost of Adaptaiton
  adaptationInten <- sapply(forageList, CalculateAdaptationIntensity)
  adaptationInten <- c(adaptationInten, 1)
  adaptationCost <- sapply(adaptationInten, getAdaptCost, adpt_choice = "feed", pars = simRuns, 
                           days.act = 180, current_herd = herd)
  adaptMax <- max(adaptationCost)
  ## Round outputs for display
  forageList <- round(forageList, 2) * 100
  adaptationCost <- prettyNum(round(adaptationCost, -2), big.mark=",",scientific=FALSE)
  
  hayadvice <- matrix(c(forageList[1],forageList[2], forageList[3], adaptationCost[1], adaptationCost[2], adaptationCost[3]),ncol = 3, byrow = TRUE)
  # colnames(hayadvice) <- c("Normal", "Above", "Below")
  # rownames(hayadvice) <- c("Forage", "Cost")
  ## Create taglist showing all adpatation
  tagList(
    h3(paste0("Year ", currentYear, ": Summer Adaptation Investment Decision")),
    p("It is now the end of June and you are mostly through the most important growing season for forage on your range.
      While good rainfall levels for July and August will still help increase the grass avaialble for your herd, you
      have to decide now how much hay to buy to supplement the grass on your range. Look to the advice below to help
      you decide how much, if any, to invest in hay."),
    br(),
    plotOutput(paste0("rainGraph", currentYear)),
    tableOutput(paste0("julyRain", currentYear)),

    
    
    
    
    
    
    
    p("If rainfall for the rest of the year is average your available forage will be ", span((forageList[1]),style="font-weight:bold;font-size:medium"), "% of normal. In this case,
             you should buy $", span((adaptationCost[1]),style="font-weight:bold;font-size:medium"), " of hay to get your herd in ideal shape for market."),
    p("If rainfall for the rest of the year is above average your available forage will be ", span((forageList[2]),style="font-weight:bold;font-size:medium"), "% of normal.
             In this case, you should buy $", span((adaptationCost[2]),style="font-weight:bold;font-size:medium"), " of hay to get your herd in ideal shape for market."),
    p("If rainfall for the rest of the year is below average your available forage will be ", span((forageList[3]),style="font-weight:bold;font-size:medium"), "% of normal.
             In this case, you should buy $", span((adaptationCost[3]),style="font-weight:bold;font-size:medium"), " of hay to get your herd in ideal shape for market.")

    ,
    br(),
    numericInput(paste0("d", currentYear, "AdaptSpent"), "How much hay, if any, do you want to purchase for your herd?",
                min = 0, max = adaptMax, value = 0, step = 100),
    h5("Remember, if you don't have enough cash on hand, you can borrow money to buy hay at an interest rate of 6.5%")
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
  
  
  ## Calculate weaned Calves
  calvesAvailable <- round(herd * wean)
  
  ## Calculate Standard Sales
  standardCowSale <- round(herd * simRuns$cull.num)
  standardCalfSale <- round(calvesAvailable * simRuns$calf.sell)
  weanWeight <- round(calfDroughtWeight(simRuns$normal.wn.wt, forage), 0)
  
  ## Create UI elements
  tagList(
    br(),
    br(),
    h3(paste0("Year ", currentYear, ": Fall Cow and Calf Sales")),
    p("It is the end of the season and it is time to take your calves to market.
      Use the information below to decide how many cows and calves you want to sell this year."),
    br(),
    if((weanWeight)<600){
    h5(p("Your weaned calves weigh ", span((weanWeight), style="font-weight:bold;font-size:large;color:red") , " pounds, on average.", 
              " Your weaned calves weigh ", span((600 - weanWeight), style="font-weight:bold;font-size:large;color:red"), " pounds below their target weight.
              This means that you're losing out on "))
    }else{
      h5(p("Your weaned calves weigh ", span((weanWeight), style="font-weight:bold;font-size:large;color:green") , " pounds, on average."))
      
      }
    ,
    tags$li("The normal target weight is 600 lbs."), 
    tags$li("If your calves are lighter than 600 lbs, it is because the mother cows
                   may not have had sufficient feed due to low rainfall, insufficient hay, or too many cows on the range."),
    br(),
    
    h5(paste0("You currently have ", myOuts[currentYear, herd], " cows and ", calvesAvailable, " calves.")),
    tags$li(paste0("With the current market price of $",simRuns$p.wn[1], "/pound, each calf you sell will bring in $", 
                   round(weanWeight * simRuns$p.wn[1], 0) , " of cash.")), 
    tags$li(paste0("At the normal target weight, each calf you sell would bring in $", simRuns$p.wn[1]*600, " of cash.")),
    tags$li("For every cow you sell, you will bring in $850 of cash."),
    br(),
    
    h5(paste("If you sell", standardCalfSale, "calves and",  standardCowSale,  "cows, your herd will stay approximately the 
            same size as it is now. If you sell more, then your herd size will decrease. 
            If you sell fewer, then your herd size will grow.")),
    tags$li("Selling a cow now means that you get more revenue this year, 
            but you will produce fewer calves next year."),
    tags$li("Keeping a calf now means that you get less revenue this year, 
            but that calf will start producing calves the year after next."),
    br(),
    
    h5(paste0("Remember, the carrying capacity of your range is ",simRuns$carrying.cap * simRuns$acres, " cow-calf pairs. 
              If your herd is larger than this you risk damaging your range and producing less grass for your herd.")),
    br(),
    sliderInput(paste0("calves", currentYear, "Sale"), "How many calves do you want to sell?",
                min = 0, max = calvesAvailable, value =  standardCalfSale, step = 1, width = "600px"),
    sliderInput(paste0("cow", currentYear, "Sale"), "How many cows do you want to sell?",
                min = 0, max = myOuts[currentYear, herd], value = standardCowSale, step = 1, width = "600px"),
    br()
    
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
  

  currentHerd <- myOuts[currentYear, herd]
  pastYear <- currentYear
  currentYear <- currentYear + 1
  myOuts[currentYear, yr := startYear + pastYear - 1]
  adaptInten <- 
    CalculateAdaptationIntensity(whatIfForage(station.gauge, zones, myOuts[currentYear, yr], currentHerd, carryingCapacity, 10, 11, "normal"))
  
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
  myOuts[currentYear, forage.production := forage]
  myOuts[currentYear, herd := round(newHerd, 0)]
  myOuts[currentYear, calves.sold := ifelse(floor(currentHerd * wean) == 0, 0, calfSale / floor(currentHerd * wean))]
  myOuts[currentYear, cows.culled := ifelse(currentHerd == 0, 0, cowSales / currentHerd)]
  print(paste("zone.change", sum(zones)))
  myOuts[currentYear, zone.change := sum(zones)]
  print(paste("adapt cost", adaptCost))
  print(paste("adapt inten", adaptInten))
  print(paste("adapt needed", getAdaptCost(adpt_choice = "feed", 
                                                     pars = simRuns, 
                                                     days.act = 180, 
                                                     current_herd = currentHerd, 
                                                     intens.adj = adaptInten)))
  print(paste("forage", forage))
  myOuts[currentYear, Gt := ifelse(forage < 1, 
                                    (1 - forage) * (1 - adaptCost/getAdaptCost(adpt_choice = "feed", 
                                                                                 pars = simRuns, 
                                                                                 days.act = 180, 
                                                                                 current_herd = currentHerd, 
                                                                                 intens.adj = adaptInten)),  
                                    1 - forage)]
  print(paste("Gt", myOuts[currentYear, Gt]))
  myOuts[currentYear, forage.potential := sum(zones)]
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
  list(tabPanel(paste0("Year ", year),
           fluidRow(
             column(8,
                    uiOutput(paste0("winterInfo", year)),
                    fluidRow(column(12, style = "background-color:white;", div(style = "height:50px;"))),
                    uiOutput(paste0("decision", year)),
                    uiOutput(paste0("insuranceUpdate", year)),
                    uiOutput(paste0("cowSell", year))
             ),
             column(2,
                    fluidRow(column(12, style = "background-color:white;", div(style = "height:600px;"))),
                    actionButton(paste0("year", year, "Start"), "Begin Simulation"),
                    fluidRow(column(12, style = "background-color:white;", div(style = "height:950px;"))),
                    uiOutput(paste0("continue", year)),
                    fluidRow(column(12, style = "background-color:white;", div(style = "height:700px;"))),
                    uiOutput(paste0("sellButton", year))
             )
           )
  ))
}


shinyInsMat <- function(yy, clv, acres, pfactor, insPurchase, tgrd){
  "
  Author: Adam (based loosely on Joe's work)
  
  Calculates premium and indemification for a specific year and
  grid cell. Currently returns are summed bu this could be done
  on a index interval basis instead.
  
  yy: Year of interest.
  
  clv: RMA coverage level. Accepted values
  are 0.7, 0.75, 0.8, 0.85, 0.9
  
  acres: Insured acres of grazing land.
  
  pfactor: Productivity factor of grazing land.
  
  insPurchase: a matrix of intervals from 1-11
  for which insurance is purchased. For example,
  purchases for the April-May and May-June intervals
  at 50% protection each would be entered as
  
  `rbind(c(3,0.5),c(5,0.5))`
  
  Consecutive intervals are not allowed.
  Returns a data.table of outputs:
  
  year: year of calculations
  prod_prem: producer premium
  indemnity: total indemnity
  full_prem: premium without subsidy
  
  "

  ##Get subsidy rate based on coverage level
  sbdy <- .51 #.51 is the subsidy for the .9 coverage level
  
  ##Set up insurance purchase vector
  ip = rep(0, 11)
  ip[insPurchase[, 1]] = insPurchase[, 2]  # replaces 0's with interval allocations
  insPurchase = ip
  
  ##Calculate policy rate
  plrt = prod(clv, acres, pfactor) * 30.4  # is the baseprice for the cper station
  
  ## Calculate the protection for each index interval
  monthProtec <- plrt * insPurchase
  
  ## Calculate Premium for each Month Below are premiums per 100 for CPER gridcell
  monthPrem <- (monthProtec * .01) * c(24.82, 23.30, 17.96, 16, 13.58, 14.69, 14.54, 17, 21.96, 23.30, 26.63)
  names(monthPrem) <- paste0("i", 1:11)
  ## Calculate Premium subsidy for each month
  subSidy <- round(monthPrem * sbdy, 2)
  
  ## Calculate subsidised premium for each month
  subPrem <- round(monthPrem * (1-sbdy), 2)
  
  ## Fetch index intervals
  intervalIndicies <- data.table(t(intervalNOAA[Year == yy, value]))
  setnames(intervalIndicies, names(subPrem))
  
  ## Calcualte the percent coverage for each month
  ## Rounding can make a significant difference here RMA uses 3 decimals
  coverageAmount <- round(clv - (intervalIndicies * .01), 3)
  
  ## Calculate Indemnities
  indem <- unlist(ifelse(coverageAmount > 0, coverageAmount/clv * monthProtec, 0))
  
  returnTable = data.table(matrix(nrow = 1, ncol = 4, data = c(yy, sum(subPrem), sum(indem), sum(monthPrem))))  # empty matrix - year, indemnity, producer premium x number years
  setnames(returnTable, c("year", "producer_prem", "indemnity", "full_prem"))
  
  return(returnTable)
}

inputToDF <- function(inputList){
  inputList <- inputList[names(inputList) != "enviro"]
  maxLength <- max(sapply(inputList, length))
  returnTable <- data.table("names" = names(inputList))
  returnTable[, paste0("value",1:maxLength) := list(rep("", nrow(returnTable)))]
  returnTable <- returnTable[names != "enviro"]
  for(i in 1:maxLength){
    returnTable[, paste0("value", i) := data.table(sapply(inputList, function(x)as.character(x[i])))]
    returnTable[, paste0("value", i) := as.character(get(paste0("value", i)))]
  }
  
  return(returnTable)
}

createOutputs <- function(practiceRuns, simRuns, indem){
  practiceOuts <- createResultsFrame(practiceRuns)
  practiceOuts[1, cost.ins := indem[[1]]$producer_prem]
  myOuts <- createResultsFrame(simRuns)
  myOuts[1, cost.ins := indem[[1]]$producer_prem]
  practiceOuts <<- practiceOuts
  myOuts <<- myOuts
}
