source("R/decisionFunctions.R")
source("R/InsuranceFunctions.R")
source("R/forageFunctions.R")
source("R/adaptationFunctions.R")
source("R/assetFunctions.R")
source("R/costRevenueFunctions.R")
source("R/initialFunctions.R")
source("R/calfCowFunctions.R")
source("R/herdFunctions.R")


# Simulation Run Functions -------------------------------------------------

sim_run_single <- function(pars,
                           station.gauge,
                           decisionMonth1,
                           decisionMonth2,
                           currentYear,
                           results_1ya,
                           results_2ya){
  
  # Create results data.table
  sim_results <- createResultsFrame()
  
  # Use herd size from the end of previous simulation
  # currentHerd <- getHerdSize(results_1ya, results_2ya, pars$death.rate)
  currentHerd <- pars$herd
  
  carryRatio <- currentHerd / (pars$carrying.cap * pars$acres)
  ## This calculates the forage potential based only on the amount of precip recieved
  # and doesn't account for past impacts on the range or carrying capacity, this is 
  # used for calculating G(t) not sure if it makes sense ####
  rainForage <- foragePWt(station.gauge, currentYear, currentHerd, 1)

  # Preserve Original zonewt
  origZoneWT <- station.gauge$zonewt
  
  ## Adjust zonewt based on forage results from previous simulation
  potential.zonewt <- results_1ya$zone.change * pars$zonewt * (1 - (results_1ya$Gt)/pars$forage.constant)
  station.gauge$zonewt <- ifelse(sum(potential.zonewt) > 1, origZoneWT, potential.zonewt)

  
  # Set current cull rate at standard
  currentCull <- pars$cull.num
  
  ## Run Decision Functions
  purchase_ins <- getInsChoice()
  adpt_choice <- foragePWt(station.gauge, currentYear, currentHerd, carryRatio, T, decisionMonth1) %>%
                getAdptChoice(station.gauge = station.gauge, decisionMonth = decisionMonth1, currentYear =  currentYear)
  calf.sell <- foragePWt(station.gauge, currentYear, currentHerd, carryRatio, T, decisionMonth2)  %>%
              getCalfSales(station.gauge = station.gauge, adpt_choice = adpt_choice, decisionMonth = decisionMonth2, 
                           currentYear =  currentYear, calf.sell = pars$calf.sell)
  
  ## Calculate forage and weening 
  fp.current <- foragePWt(station.gauge, currentYear, currentHerd, carryRatio)
  wn.succ.current <- AdjWeanSuccess(fp.current, 
                            noadpt = ifelse(adpt_choice == "noAdpt", T, F), 
                            pars$normal.wn.succ, t = 1)
  weansucc<- wn.succ.current
  # Calculate base operating cost
  base.op.cost <- CalculateBaseOpCosts(herd = currentHerd, cow.cost = pars$cow.cost)
  
  # Compute insurance premiums and indemnities
  if (purchase_ins & currentHerd > 0){
    rma.ins = with(pars, insMat(yy = currentYear, clv = clv, acres = acres,
                     pfactor = pfactor, insPurchase  =  insp, tgrd = tgrd))
  }else{ # if purchase.insurance set to 0 (no insurance), simply set prem/indem = 0
    rma.ins = cbind(currentYear,matrix(0,1,2))
  }
  
  # Base Cow Assets: No sell/replace
  base.assets.cow <- CalcCowAssets(t = 1, herd = currentHerd, p.cow = pars$p.cow)
  
  # Default purchases to 0 this will need to be adjusted
  base.cap.purch <- 0
  
  # Calculate outcome varialbes based on decisions above
  if(results_1ya$adapt_choice == "sellprs"){
    
    # Find out whether cows are being repurchased
    buyDecision <- getBuyDecision()
    if(buyDecision){
      base.cap.purch <- pars$herd * pars$p.cow
      currentHerd <- pars$herd
    }else if(results_1ya$cap.sales > 0){ # If cows aren't being repurchased then 
      base.cap.taxes <- results_1ya$cap.sales * pars$cap.tax.rate
    }
  }
  if(adpt_choice %in% c("sellprs", "sellprs.norepl") & currentHerd > 0){
    #****This always has early calf sales occuring at the t0 price...work needed
    calf.sales <- CalculateExpSales(herd = currentHerd, 
                                    wn.succ = 1, 
                                    calf.sell = 1, 
                                    wn.wt = pars$calf.wt, 
                                    p.wn = pars$p.calf.t0)
    base.op.cost <- CalculateSellPrsCost(op.cost.adj = op.cost.adj,
                                         herd = currentHerd,
                                         sell.cost = pars$sell.cost,
                                         base.op.cost = base.op.cost,
                                         herdless.op.cost = pars$herdless.op.cost)
    base.cap.sales <- base.assets.cow
    currentHerd <- 0
    
    ## Deal with taxes if pairs are sold and not replaced
    if(adpt_choice == "sellprs.norepl"){
      base.cap.taxes <- base.cap.sales * cap.tax.rate
    }else{
      base.cap.taxes <- 0
    }
    
    #****This should maybe be nonzero as there is sell cost etc included in the cost above...maybe break
    #    that cost out and include it in adpt cost
    adptCost <- 0
    base.assets.cow <- 0
  }else{
    
    # Standard Culling
    base.cap.sales <- with(pars, (currentCull * currentHerd) * p.cow)
    base.cap.taxes <- base.cap.sales * pars$cap.tax.rate
    
    if(adpt_choice != "noAdpt"){
      intens.adj <- CalculateAdaptationIntensity(fp.current)
      
      #****Right now this is always going to come put as 180...this probably needs to be reworked
      #    to take into account the possiblity of selling early...maybe it starts as the time between
      #    decision 1 and decision 2 and if decision 2 isn't to sell early it goes up?
      
      days.act <- with(pars, CalculateDaysAction(act.st.yr, act.st.m, act.end.yr, act.end.m, 1))
      adptCost <- getAdaptCost(adpt_choice, pars, days.act, currentHerd, intens.adj)
      
      ## calculate calf sales
      calf.sales <- with(pars, ifelse(adpt_choice == "rentpast",
                           CalculateRentPastRevenue(normal.wn.wt = normal.wn.wt,
                                                    calf.loss = calf.loss,
                                                    calf.wt.adj = calf.wt.adj,
                                                    calf.sell = calf.sell,
                                                    herd = currentHerd,
                                                    wn.succ = wn.succ.current,
                                                    p.wn = p.wn),
                           CalculateExpSales(herd = currentHerd, 
                                             wn.succ = wn.succ.current, 
                                             calf.sell = calf.sell, 
                                             wn.wt = pars$normal.wn.wt, 
                                             p.wn = pars$p.wn[currentYear - styr + 1])
                          ))
    }else{
      calf.sales <- CalculateExpSales(herd = currentHerd, 
                        wn.succ = wn.succ.current, 
                        calf.sell = calf.sell, 
                        wn.wt = calfDroughtWeight(pars$normal.wn.wt, fp.current), 
                        p.wn = pars$p.wn[currentYear - pars$styr + 1])
      adptCost <- 0
    }
  }
  
  # Adjustments if cows were sold the previous year with no replacement
  if(currentHerd == 0 & results_1ya$adapt_choice == "sellprs.norepl"){
    base.op.cost <- pars$herdless.op.cost
    adpt_choice <- "sellprs.norepl"
  }
  
  # Assign resutls to the return frame 
  sim_results[, yr := currentYear]
  sim_results[, rev.calf := calf.sales]
  sim_results[, rev.ins := rma.ins$indemnity]
  sim_results[, rev.int := results_1ya$assets.cash * pars$invst.int]
  sim_results[, rev.tot := rev.calf + rev.ins + rev.int]
  sim_results[, cost.op := base.op.cost]
  sim_results[, cost.ins := rma.ins$producer_prem]
  sim_results[, cost.int := ifelse(results_1ya$assets.cash < 0,
                                   results_1ya$assets.cash * -1 * pars$loan.int,
                                   0)]
  sim_results[, cost.adpt := adptCost]
  sim_results[, cost.tot := cost.op + cost.ins + cost.int + cost.adpt]
  sim_results[, profit := rev.tot - cost.tot]
  
  #****This tax rate should go in constvars
  #    also not sure why this is different that cap sales?
  ## Calculate taxes on profit
  sim_results[, taxes := ifelse(profit > 0, profit * (0.124+0.15+0.04), 0)]
  sim_results[, cap.sales := base.cap.sales]
  sim_results[, cap.taxes := base.cap.taxes]
  sim_results[, aftax.inc := profit - taxes]
  sim_results[, assets.cash := results_1ya$assets.cash + aftax.inc + 
                cap.sales - cap.purch - cap.taxes]
  sim_results[, assets.cow := base.assets.cow]
  sim_results[, net.wrth := assets.cash + assets.cow]
  sim_results[, wn.succ := wn.succ.current]
  sim_results[, herd := currentHerd]
  sim_results[, forage.production := fp.current]
  sim_results[, adapt_choice := adpt_choice]
  sim_results[, cows.culled := currentCull]
  sim_results[, calves.sold := calf.sell]
  sim_results[, zone.change :=  sum(station.gauge$zonewt)]
  
  #### This is going to need some work once we start incorporating user adaptation choices
  sim_results[, Gt := ifelse(forage.production < 1 & adapt_choice == "noAdpt", 
                                        1 - forage.production + forage.production * (1 - rainForage)/rainForage,  # the .5 should be the "adaptation deficit"
                                        # 0)]
                                        1 - max(forage.production, .9))]
}


# Utility Functions -------------------------------------------------------

':=' <- function(lhs, rhs) {
  # Description: Magical function that allows you to return more than one variable
  #  from other functions.
  # Code from http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value
  
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL))
}

list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}



