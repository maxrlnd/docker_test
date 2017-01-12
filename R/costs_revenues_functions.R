# Support functions to calculate costs and revenues for each drought adaptation option

#### Baseline Costs and Revenues ####
CalculateExpSales <- function(herd, calf.sell, wn.wt, p.wn) {
  "
  Function: CalculateExpSales
  Description: Calculates expected calf revenues for non-drought year
  
  Inputs:
  herd = Size of herd (head of cows, does not include calves)
  calf.sell = Average percentage of calves sold (%)
  wn.wt = Average weight at weaning (pounds)
  p.wn = Expected sale price of calves ($/pound)
  
  Outputs:
  base.sales = Expected revenues from calf sales for a non-drought year
  "
  
  base.sales <- herd * calf.sell * wn.wt * p.wn
  return(base.sales)
}

CalculateBaseOpCosts <- function(herd, cow.cost) {
  base.op.cost <- herd * cow.cost
  return(base.op.cost)
} 

#### Drought Action ####
CalculateDaysAction <- function(act.st.yr, act.st.m, act.end.yr, act.end.m, drought.action) {
  "
  Function: CalculateDaysAction
  Description: Calculate the number of days rancher pays for a drought adaptation action.
  NOTE: This function assumes that the actions take place only in one year.
  
  Inputs:
  act.st.yr = Year the action starts
  act.st.m = Month the action starts
  act.end.yr = Year the action ends
  act.end.m = Month the action ends
  drought.action = ??
  
  Outputs:
  days.act = Number of days drought adaptation action takes place (days)
  "
  
  # Start and End month assumes that action starts/stops on the first of the month
  
  # Error handling
  if (act.st.yr < 1) {
    stop("Action start year ", act.st.yr, "is not greater than 0")
  }
  if (act.st.yr > act.end.yr | act.st.yr == act.end.yr & act.st.m > act.end.m) {
    stop("Action end occurs before action start")
  }
  if (act.st.m < 1 | act.st.m > 12 | act.end.m < 1 | act.end.m > 12) {
    stop("Invalid action start or end month (not between 1 and 12)")
  }
  
  if (act.st.yr == act.end.yr) {
    days.act <- (act.end.m - act.st.m) * 30
  }
  
  # TEMPORARY ERROR: While the model is not equipped to handle multi-year droughts, we will not allow drought
  #  adaptation across multiple years.
  if (act.st.yr != act.end.yr) {
    stop("Model not equipped for multi-year drought adaptation. act.st.yr must equal act.end.yr")
  }
  
  days.act.vect <- days.act * drought.action #Creates a vector of days of action
  days.act.vect
}

#### Option 1: Buy feed ####

CalculateFeedCost <- function(kHayLbs, kOthLbs, p.hay,p.oth, days.feed, herd) {
  "
  Function: CalculateFeedCost
  Description: Calculating the costs of purchasing additional feed
  
  Inputs:
  kHayLbs = Number of pounds of additional hay needed for each cow each day (pounds/head/day).  (Source: UNKNOWN)
  p.hay = Price of hay ($/ton). User input.
  kOthLbs = Number of pounds of additional other feed needed for each cow each day (pounds/head/day). (Source: UNKNOWN)
  p.oth = Price of other feed ($/ton). Currently not a user input. Does not come into play since the model assumes only feeding hay
  days.feed = Number of days additional feed is needed. Generally, equal to days.act. (days)
  herd = Size of herd (head of cows, does not include calves)
  
  Outputs:
  cost.feed = Additional costs to feed the herd over the remainder of the season ($/year)
  "
  # Calculate cost per cow per day * days of feed for the year * number of cows in the herd
  feed.cost <- (kHayLbs / 2000 * p.hay + kOthLbs / 2000 * p.oth) * days.feed * herd
  return(feed.cost)
}


#### Option 2: Rent Pasture ####

CalculateRentPastCost <- function(n.miles, truck.cost, past.rent, oth.cost, days.rent, max.wt, cow.wt, calf.wt, herd) {
  "
  Function: CalculatePastureRentCost
  Description: Calculates the costs of renting pasture and trucking pairs
  NOTE: Not including loan interest costs. This needs to be resolved across all options.
  
  Inputs: 
  n.miles = Distance to rented pasture (miles)
  truck.cost = Trucking cost per loaded mile ($/mile/truck)
  past.rent = Price of renting pasture per animal unit month, where an animal unit is a cow/calf pair ($/pair/month)
  days.rent = Days on rented pasture. Generally, equal to days.act. (days)
  oth.cost = All other non-rental, non-trucking costs ($)
  max.wt = Maximum weight per truck (pounds)
  cow.wt = Average cow weight (pounds)
  calf.wt = Average 'current' weight of calves before trucking to rented pasture (pounds)
  herd = Size of herd (head of cows, does not include calves)
  loan.int = interest rate for borrowed money (%/year)
  
  Outputs:
  cost.rentpast = Total costs of using renting pasture including transport costs ($/year)
  "
  # Calculating number of trucks needed to truck pairs to pasture and to truck cows home (without calves)
  n.trucks.past <- ceiling(herd / ceiling(max.wt / (cow.wt + calf.wt)))
  n.trucks.home <- ceiling(herd / ceiling(max.wt / cow.wt))
  
  # Cost per herd of trucking pairs to pasture and trucking cows back home
  tot.truck.cost <- n.miles * truck.cost * n.trucks.past + n.miles * truck.cost * n.trucks.home
  
  # Cost of renting pasture
  tot.past.rent <- past.rent / 30 * days.rent * herd
  
  # Total costs including transport, rent, and other costs
  cost.rentpast.woint <- ifelse(days.rent > 0, tot.truck.cost + tot.past.rent + oth.cost, 0)
  cost.rentpast <- ifelse(days.rent > 0, cost.rentpast.woint * (1 + loan.int / 365 * days.rent), 0)  # I think we should not include interest here unless it is also included in other adaptation costs
  
  return(cost.rentpast)
}

CalculateRentPastRevenue <- function(expected.wn.wt, calf.loss, calf.wt.adj, calf.sell, herd, p.wn) {
  "
  CalculateRentPastRevenue 
  Description: Calculates calf sale revenues after trucking pairs to rented pastures
  
  Inputs:
  calf.loss = Additional calf deaths due to transport stress (head of calves)
  calf.wt.adj = Adjustment for calf weaning weights (%)
  calf.sell = Average percentage of calves sold (%)
  wn.wt = Average weight at weaning (pounds)
  p.wn = Expected sale price of calves ($/pound)
  herd = Size of herd (head of cows, does not include calves)
  
  Outputs:
  rev.rentpast = Change in revenue due to mortality and weight loss from trucking to rented pasture
  "
  # Number of calves sold after accounting for calf mortality in transport 
  calf.sales.num <- herd * calf.sell - calf.loss
  
  # Selling weight after accounting for weight loss due to transport stress
  sell.wt <- expected.wn.wt * (1 + calf.wt.adj)
  
  # Expected calf sale revenues
  rev.rentpast <- calf.sales.num * sell.wt * p.wn
  rev.rentpast
}

#### Option 3: Sell Pairs & Replace ####
CalculateSellPrsCost <- function(op.cost.adj, herd, sell.cost, base.op.cost, herdless.op.cost) {
  "
  Function: CalculateSellPrsCost
  Description: Calculates the operating costs to sell pairs in year 1 and replacing cows in year 3
  NOTE: It is assumed that cows are replaced on last day of the second year after they are sold. 
  For example, cows sold in 2011 are replaced on 12/31/2013.
  
  Inputs:
  op.cost.adj = Change in operating costs in year 1 per cow ($/cow/year)
  sell.cost = Selling cost per cow ($/cow) NOTE: DO WE COUNT SELLING COSTS IN A NORMAL YEAR? ARE THESE ADDITIONAL?
  herd = Size of herd (head of cows, does not include calves)
  base.cost = Baseline annual cost of operating ranch with full herd ($/year)
  fixed.op.cost = Fixed operating costs for a year without a herd ($/year)
  
  Outputs:
  cost.sellprs = 5x1 vector of changes in operating costs for years 1 through 5 from selling pairs in year 1 and replacing them at the end of year 3
  "
  cost.sellprs <- NULL
  cost.sellprs[1] <- base.op.cost + op.cost.adj * herd + sell.cost * herd  # CORRECT CODE!!! # Yr 1 operating costs includes a reduction in operating cost from not having the herd and the additional cost to sell cows
  # cost.sellprs[1] <- base.op.cost + op.cost.adj * herd  # INCORRECT CODE (replicates excel's exclusion of herd selling costs)
  cost.sellprs[2] <- herdless.op.cost  # fixed 'herdless' operating costs
  cost.sellprs[3] <- herdless.op.cost  # fixed 'herdless' operating costs 
  cost.sellprs[4:5] <- base.op.cost  # Yr 4 & 5 change in op costs are assumed to be normal
  
  cost.sellprs
}

CalculateSellPrsRev <- function(base.sales, herd, wn.succ, calf.wt, p.calf.t0) { 
  "
  Function: CalculateSellPrsRev
  Description: Calculates calf sales revenues due to selling pairs and replacing cows for years 1 through 3
  NOTE: It is assumed that cows are replaced on last day of the second year after they are sold. 
  For example, cows sold in 2011 are replaced on 12/31/2013.
  
  Inputs:
  base.sales = Calf sales in a normal year ($/year)
  p.wn.t0 = Current sale price calves ($/pound)
  herd = Size of herd (head of cows, does not include calves)
  wn.succ = Average percentage of cows that successfully wean calves (%)
  calf.wt = Average 'current' weight of calves (pounds)
  
  Outputs:
  rev.sellprs = 5x1 vector of calf revenues for years 1 through 5.
  "
  # Calf sales revenues
  calf.sales <- rep(NA,5)
  for (i in 1:5) {
    if(i == 1) {
      calf.sales[i] <- herd * wn.succ * calf.wt * p.calf.t0
    }
    if(i == 2 | i == 3) {
      calf.sales[i] <- 0
    }
    if(i > 3) {
      calf.sales[i] <- base.sales[i]
    }
  }
  calf.sales
}
