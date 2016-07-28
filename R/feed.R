# Function: CalculateDaysAction
# Description: Calculate the number of days rancher pays for a drought adaptation action.
# NOTE: This function assumes that the actions take place only in one year.
#
# Inputs:
#  act.st.yr = Year the action starts
#  act.st.m = Month the action starts
#  act.end.yr = Year the action ends
#  act.end.m = Month the action ends
#
# Outputs:
#  days.act = Number of days drought adaptation action takes place (days)
CalculateDaysAction <- function(act.st.yr, act.st.m, act.end.yr, act.end.m) {
  # Warning about end month
  warning("Start and End month assumes that action starts/stops on the first of the month")
  
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
  
  return(days.act)
}

# Function: CalculateFeedCost
# Description: Calculating the costs of purchasing additional feed
#
# Inputs:
#  kHayLbs = Number of pounds of additional hay needed for each cow each day (pounds/head/day).  (Source: UNKNOWN)
#  p.hay = Price of hay ($/ton). User input.
#  kOthLbs = Number of pounds of additional other feed needed for each cow each day (pounds/head/day). (Source: UNKNOWN)
#  p.oth = Price of other feed ($/ton). Currently not a user input. Does not come into play since the model assumes only feeding hay
#  days.feed = Number of days additional feed is needed. Generally, equal to days.act. (days)
#  herd = Size of herd (head of cows, does not include calves)
#
# Outputs:
# cost.feed = Additional costs to feed the herd over the remainder of the season ($/year)
CalculateFeedCost <- function(kHayLbs, kOthLbs, p.hay,p.oth, days.feed, herd) {
  # Calculate cost per cow per day * days of feed for the year * number of cows in the herd
  feed.cost <- (kHayLbs / 2000 * p.hay + kOthLbs / 2000 * p.oth) * days.feed * herd
  return(feed.cost)
}

# Function: CalculatePastureRentCost
# Description: Calculates the costs of renting pasture and trucking pairs
# NOTE: Not including loan interest costs. This needs to be resolved across all options.
#
# Inputs: 
#  n.miles = Distance to rented pasture (miles)
#  truck.cost = Trucking cost per loaded mile ($/mile/truck)
#  past.rent = Price of renting pasture per animal unit month, where an animal unit is a cow/calf pair ($/pair/month)
#  days.rent = Days on rented pasture. Generally, equal to days.act. (days)
#  oth.cost = All other non-rental, non-trucking costs ($)
#  max.wt = Maximum weight per truck (pounds)
#  cow.wt = Average cow weight (pounds)
#  calf.wt = Average 'current' weight of calves before trucking to rented pasture (pounds)
#  herd = Size of herd (head of cows, does not include calves)
#
# Outputs:
#  cost.rentpast = Total costs of using renting pasture including transport costs ($/year)
CalculateRentPastCost <- function(n.miles, truck.cost, past.rent, oth.cost, days.rent, max.wt, cow.wt, calf.wt, herd) {
  # Calculating number of trucks needed to truck pairs to pasture and to truck cows home (without calves)
  n.trucks.past <- ceiling(herd / ceiling(max.wt / (cow.wt + calf.wt)))
  n.trucks.home <- ceiling(herd / ceiling(max.wt / cow.wt))
  
  # Cost per herd of trucking pairs to pasture and trucking cows back home
  tot.truck.cost <- n.miles * truck.cost * n.trucks.past + n.miles * truck.cost * n.trucks.home
  
  # Cost of renting pasture
  tot.past.rent <- past.rent / 30 * days.rent * herd
  
  # Total costs including transport, rent, and other costs (DOES NOT INCLUDE LOAN INTEREST)
  cost.rentpast <- tot.truck.cost + tot.past.rent + oth.cost
  
  return(cost.rentpast)
}

# CalculateRentPastRevenue 
# Description: Calculates the change in revenues due to trucking pairs to rented pastures
#
# Inputs:
#  calf.loss = Additional calf deaths due to transport stress (head of calves)
#  calf.wt.adj = Adjustment for calf weaning weights (%)
#  calf.sell = Average percentage of calves sold (%)
#  wn.wt = Average weight at weaning (pounds)
#  p.wn.yr1 = Expected sale price of calves in year 1 ($/pound)
#  herd = Size of herd (head of cows, does not include calves)
#
# Outputs:
#  rev.rentpast = Change in revenue due to mortality and weight loss from trucking to rented pasture
CalculateRentPastRevenue <- function(wn.wt, calf.loss, calf.wt.adj, calf.sell, herd, p.wn.yr1) {
  # Number of calves sold after accounting for calf mortality in transport 
  actual.calf.sales <- herd * calf.sell - calf.loss
  
  # Selling weight after accounting for weight loss due to transport stress
  actual.sell.wt <- wn.wt * (1 + calf.wt.adj)
  
  # Expected calf revenues for non-drought year
  exp.sales <- herd * calf.sell * wn.wt * p.wn.yr1
  
  # Change in expected revenues
  rev.rentpast <- exp.sales - actual.calf.sales * actual.sell.wt * p.wn.yr1
  return(rev.rentpast)
}

# Function: CalculateSellPairsCost
# Description: Calculates the change in costs due to selling pairs and replacing cows
# NOTE: It is assumed that cows are replaced on last day of the second year after they are sold. 
#  For example, cows sold in 2011 are replaced on 12/31/2013.
#
# Inputs:
#  op.cost.yr1 = Change in operating costs in year 1 per cow ($/cow)
#  op.cost.yr2 = Change in operating costs in year 2 per cow ($/cow)
#  op.cost.yr3up = Change in operating costs in years 3 and up per cow ($/cow)
#  sell.cost = Selling cost per cow ($/cow) NOTE: DO WE COUNT SELLING COSTS IN A NORMAL YEAR? ARE THESE ADDITIONAL?
#  replc.cost = Cost of replacing the cow ($/cow)
#  herd = Size of herd (head of cows, does not include calves)
#
# Outputs:
#  
CalculateSellPrsCostYr1 <- function(op.cost.yr1, herd, sell.cost) {
  # Yr 1 change in operating costs includes change in operating cost from not having the herd and the additional cost to sell cows
  cost.sellprs.yr1 <- op.cost.yr1 * herd + sell.cost * herd  
  return(cost.sellprs.yr1)
}

# Function: CalculateSellPairsCost
# Description: Calculates the change in revenues due to selling pairs and replacing cows
# NOTE: It is assumed that cows are replaced on last day of the second year after they are sold. 
#  For example, cows sold in 2011 are replaced on 12/31/2013.
#
# Inputs:
#  calf.sell = Average percentage of calves sold (%)
#  wn.wt = Average weight at weaning (pounds)
#  p.wn.yr1 = Expected sale price of calves in year 1 ($/pound)
#  p.wn.t0 = Current sale price calves ($/pound)
#  herd = Size of herd (head of cows, does not include calves)
#  wn.succ = Average percentage of cows that successfully wean calves (%)
#  calf.wt = Average 'current' weight of calves (pounds)
#  p.cow = Current sale price of cow ($/cow)
#  
# Outputs:
#
CalculateSellPrsRevenueYr1 <- function(herd, calf.sell, wn.wt, p.wn.yr1, wn.succ, calf.wt, p.calf.t0, p.cow) {
  # Expected calf revenues for non-drought year
  exp.sales <- herd * calf.sell * wn.wt * p.wn.yr1
  
  # Actual calf sales for year 1
  calf.sales.yr1 <- herd * wn.succ * calf.wt * p.calf.t0 
  
  # Cow sales in year 1
  cow.sales <- p.cow * herd
  
  # Change in year 1 revenue from expected
  rev.sellprs.yr1 <- exp.sales - calf.sales.yr1 + cow.sales
  return(rev.sellprs.yr1)
}
  
