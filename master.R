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

# Base Values: Indicate average year costs and revenues without insurance
base.cost <- base.op.cost  # assumes without insurance, cow costs are the only costs for the producer
base.rev <- base.sales  # assumes that without insurance, calf sales are only revenue. INTEREST MUST BE TAKEN INTO ACCOUNT.  
base.prof <- base.rev - base.cost

base.cost.ins <- base.op.cost + rma.ins[,2] # increment base operating costs with producer premium
#base.rev.ins <- base.sales + rma.ins[,3] # increment base revenue with indemnity
#base.prof.ins <- base.rev.ins - base.cost.ins

####No Drought####

out.nodrght <- OptionOutput(opt = "nodrght",
                            nodrought = TRUE, 
                            rev.calf = base.sales, 
                            cost.op = rep(base.op.cost,5), 
                            rma.ins = rma.ins, 
                            int.invst = invst.int, 
                            int.loan = loan.int)
  
####Drought Occurs####
# For each option, we calculate the **CHANGE** in costs
# and the **CHANGE** in revenues relative to the no drought baseline.

# Calculate days of drought adaptation action
days.act <- CalculateDaysAction(act.st.yr,act.st.m,act.end.yr,act.end.m)


##### Option 0: No adaptation ####
# drought revenues
noadpt.rev.calf <- unlist(lapply(1:5,function(i){
  CalculateExpSales(herd = herd, calf.sell = calf.sell, wn.wt = wn.wt[i], p.wn.yr1 = p.wn[i])
}))

out.noadpt <- OptionOutput(opt = "noadpt",
                           rev.calf = noadpt.rev.calf, 
                           cost.op = rep(base.op.cost,5), 
                           cost.adpt = 0, 
                           rma.ins = rma.ins, 
                           int.invst = invst.int, 
                           int.loan = loan.int)


## Option 1: Buy additional feed
# Assumes that feeding days are equivalent to drought adaptation action days
# 'days.feed' is vectorized to represent feed purchases in the first year only
days.feed <- c(days.act,rep(0,4))  

# Calculate additional costs to feed herd
feed.cost <- unlist(lapply(1:5,function(i){
  CalculateFeedCost(kHayLbs, kOthLbs, p.hay, p.oth, days.feed[i], herd) 
  })) 
out.feed <- cbind(base.rev, feed.cost + base.cost, base.rev - (feed.cost + base.cost))  # generate outcome matrix: col1 - revenue, col2 - cost
out.feed.ins <- cbind(base.rev, feed.cost + base.cost.ins, base.rev - (feed.cost + base.cost.ins))

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
# This function does need to lapply. It takes a vector of inputs and returns a vector of outputs                                   
sellprs <- c(CalculateSellPrsRev(base.sales = base.sales, herd = herd, 
                                 wn.wt = wn.wt, p.wn = p.wn, wn.succ = wn.succ, 
                                 calf.wt = calf.wt, p.calf.t0, p.cow, invst.int, 
                                 cull) + base.rev, 
             CalculateSellPrsCost(op.cost.yr1 = op.cost.yr1, herd = herd, 
                                  sell.cost = sell.cost, 
                                  base.op.cost = base.op.cost, 
                                  fixed.op.cost = fixed.op.cost, 
                                  p.cow.rplc = p.cow.rplc) + base.cost)