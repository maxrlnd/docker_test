# Copyright (c) 2016 Trisha Shrum, Joseph Tuccillo
#
# Authors Comment: This model is jointly developed at the University of 
#  Colorado Earth Lab based on work by Adam McCurdy, Joseph Tuccillo, Kelly Carney, 
#  Bill Travis, Jeffrey Tranel, Rod Sharp, and John Deering.
#
# Description: This script implements a simulation of drought adaptation
#  decisions by Western cattle ranchers. 
#
# Inputs:
#   ...
#
# Outputs:
#   ...

# Source functions
source("R/support_functions.R")

# Source variable assignment script
source("R/vars.R")

#### Main Script ####

# Compute insurance premiums and indemnities
if (!is.null(insp)){
  rma.ins = insMat(tgrd = tgrd,yyr = yyr,clv = clv,acres = acres,
                   pfactor = pfactor,insPurchase  =  insp)
}else{ # if insp is set as NULL (no insurance), simply set prem/indem = 0
  rma.ins = cbind(yyr[1]:yyr[1]+4,matrix(0,5,2))
}

i = 1 # Current year - assuming year 1 for now, should be iterated

# Calculate days of drought adaptation action
days.act <- CalculateDaysAction(act.st.yr,act.st.m,act.end.yr,act.end.m)

# Option 1: Buy additional feed
days.feed <- days.act  # Assumes that feeding days are equivalent to drought adaptation action days
feed.cost <- CalculateFeedCost(kHayLbs, kOthLbs, p.hay, p.oth, days.feed, herd) + rma.ins[,2][i] # Calculates additional costs to feed herd

# Option 2: Truck pairs to rented pasture
days.rent <- days.act # Assumes that pasture rental days are equivalent to drought adaptation action days
cost.rentpast <- CalculateRentPastCost(n.miles = n.miles, truck.cost = truck.cost, past.rent = past.rent,
                                       days.rent = days.rent, oth.cost = oth.cost, max.wt = max.wt,
                                       cow.wt = cow.wt, calf.wt = calf.wt, herd = herd) + rma.ins[,2][i]
rev.rentpast <- CalculateRentPastRevenue(wn.wt = wn.wt, calf.loss = calf.loss, calf.wt.adj = calf.wt.adj, 
                                         calf.sell = calf.sell, herd = herd, p.wn.yr1 = p.wn.yr1) + rma.ins[,3][i]

# Option 3: Sell pairs and replace cows
cost.sellprs.yr1 <- CalculateSellPrsCostYr1(op.cost.yr1 = op.cost.yr1, herd = herd, sell.cost = sell.cost) + rma.ins[,2][i]
rev.sellprs.yr1 <- CalculateSellPrsRevenueYr1(herd = herd, calf.sell = calf.sell, wn.wt = wn.wt, 
                                              p.wn.yr1 = p.wn.yr1, wn.succ = wn.succ, calf.wt = calf.wt, 
                                              p.calf.t0 = p.calf.t0, p.cow = p.cow) + rma.ins[,3][i]
