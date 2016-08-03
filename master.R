# Copyright (c) 2016 Trisha Shrum, Joseph Tuccillo
## Authors Comment: This model is jointly developed at the University of 
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

# Clear environment
rm(list=ls())

# Source functions
source("R/support_functions.R")

# Source variable assignment script
source("R/vars.R")


#### Main Script ####

# Calculate No-Drought Revenues (expected sale price, year 1)
base.sales <- unlist(lapply(1:5,function(i){
  CalculateExpSales(herd = herd, calf.sell = calf.sell, wn.wt = expected.wn.wt, p.wn.yr1 = p.wn[i])
}))

base.op.cost = CalculateBaseOpCosts(herd = herd, cow.cost = cow.cost)

# Compute insurance premiums and indemnities
if (purchase.insurance==1){
  rma.ins = insMat(tgrd = tgrd,yyr = yyr,clv = clv,acres = acres,
                   pfactor = pfactor,insPurchase  =  insp)
}else{ # if purchase.insurance set to 0 (no insurance), simply set prem/indem = 0
  rma.ins = cbind(yyr[1]:yyr[1]+4,matrix(0,5,2))
}

base.cost = base.op.cost + rma.ins[,2] # increment base operating costs with producer premium
base.rev = base.sales + rma.ins[,3] # increment base revenue with indemnity

####No Drought####

baseOutcome=cbind(base.sales,base.op.cost) # with insurance
baseOutcome_ins=cbind(base.rev,base.cost) # without insurance

####Drought Occurs####
# For each option, we calculate the **CHANGE** in costs
# and the **CHANGE** in revenues relative to the no drought baseline.

## No adaptive action

#Baseline drought revenues
base.sales_drought <- unlist(lapply(1:5,function(i){
  CalculateExpSales(herd = herd, calf.sell = calf.sell, wn.wt = wn.wt[i], p.wn.yr1 = p.wn[i])
}))

noAdaptOutcome=cbind(base.sales_drought,
                 base.op.cost)
noAdaptOutcome_ins=cbind(base.sales_drought+rma.ins[,3],
                 base.cost)

# Calculate days of drought adaptation action
days.act <- CalculateDaysAction(act.st.yr,act.st.m,act.end.yr,act.end.m)

## Option 1: Buy additional feed
# Assumes that feeding days are equivalent to drought adaptation action days
# 'days.feed' is vectorized to represent feed purchases in the first year only
days.feed <- c(days.act,rep(0,4))  

# Calculate additional costs to feed herd
feed.cost <- unlist(lapply(1:5,function(i){
  CalculateFeedCost(kHayLbs, kOthLbs, p.hay, p.oth, days.feed[i], herd) + base.cost[i]
  })) 
feedOutcome=cbind(base.rev,feed.cost+base.cost) # generate outcome matrix: col1 - revenue, col2 - cost

## Option 2: Truck pairs to rented pasture
days.rent <- days.act # Assumes that pasture rental days are equivalent to drought adaptation action days
rentpast = lapply(1:5,function(i){
  c(CalculateRentPastRevenue(wn.wt = wn.wt, calf.loss = calf.loss[i], calf.wt.adj = calf.wt.adj[i],
                             calf.sell = calf.sell, herd = herd, p.wn.yr1 = p.wn[i]) + base.rev[i],
        CalculateRentPastCost(n.miles = n.miles, truck.cost = truck.cost, past.rent = past.rent,
                          days.rent = days.rent, oth.cost = oth.cost, max.wt = max.wt,
                          cow.wt = cow.wt, calf.wt = calf.wt, herd = herd) + base.cost[i])
  })
rentOutcome = t(matrix(unlist(rentpast),2,5)) # convert list to matrix


## Option 3: Sell pairs and replace cows
# cost.sellprs.yr1 <- CalculateSellPrsCostYr1(op.cost.yr1 = op.cost.yr1, herd = herd, sell.cost = sell.cost) + rma.ins[,2][i]
# rev.sellprs.yr1 <- CalculateSellPrsRevenueYr1(herd = herd, calf.sell = calf.sell, wn.wt = wn.wt, 
#                                               p.wn.yr1 = p.wn[i], wn.succ = wn.succ, calf.wt = calf.wt, 
#                                               p.calf.t0 = p.calf.t0, p.cow = p.cow, exp.sales = exp.sales) + rma.ins[,3][i]
lapply(1:5,function(i){
  c(CalculateSellPrsRevenueYr1(herd = herd, calf.sell = calf.sell, wn.wt = wn.wt, 
                               p.wn.yr1 = p.wn[i], wn.succ = wn.succ, calf.wt = calf.wt, 
                               p.calf.t0 = p.calf.t0, p.cow = p.cow, exp.sales = exp.sales,invst.int = invst.int) + base.rev,
    CalculateSellPrsCostYr1(op.cost.yr1 = op.cost.yr1, herd = herd, sell.cost = sell.cost) + base.cost)
})