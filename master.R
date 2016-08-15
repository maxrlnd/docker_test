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

# Calculate No-Drought Revenues from Calf Sales (aka base sales)
base.sales <- unlist(lapply(1:t,function(i){
  CalculateExpSales(herd = herd, calf.sell = calf.sell, wn.wt = expected.wn.wt, p.wn.yr1 = p.wn[i])
}))

# Calculate No-Drought Operating Costs
base.op.cost = CalculateBaseOpCosts(herd = herd, cow.cost = cow.cost)

# Compute insurance premiums and indemnities
if (purchase.insurance==1){
  rma.ins = insMat(tgrd = tgrd,yyr = yyr,clv = clv,acres = acres,
                   pfactor = pfactor,insPurchase  =  insp)
}else{ # if purchase.insurance set to 0 (no insurance), simply set prem/indem = 0
  rma.ins = cbind(yyr[1]:yyr[1]+(t-1),matrix(0,t,2))
}

# Base Values: Indicate average year costs and revenues without insurance
base.cost <- base.op.cost  # assumes without insurance, cow costs are the only costs for the producer
base.rev <- base.sales  # assumes that without insurance, calf sales are only revenue. INTEREST MUST BE TAKEN INTO ACCOUNT.  
base.prof <- base.rev - base.cost

base.cost.ins <- base.op.cost + rma.ins[,2] # increment base operating costs with producer premium
#base.rev.ins <- base.sales + rma.ins[,3] # increment base revenue with indemnity
#base.prof.ins <- base.rev.ins - base.cost.ins

# Base Cow Assets: No sell/replace
base.assets.cow <- CalcCowAssets(t = t, herd = herd, p.cow = p.cow)

c(base.cap.sales, base.cap.purch) := CalcCapSalesPurch(assets.cow = base.assets.cow, 
                                             t=t, 
                                             cull.num = cull.num, 
                                             p.cow = p.cow)

base.cap.taxes <- CalcCapTaxes(cap.sales = base.cap.sales, 
                          cap.purch = base.cap.purch, 
                          cap.tax.rate = cap.tax.rate,
                          herd = herd,
                          p.cow = p.cow)

####No Drought####

out.nodrght <- OptionOutput(t = t,
                            opt = "nodrght",
                            nodrought = TRUE, 
                            rev.calf = base.sales, 
                            cost.op = rep(base.op.cost,t), 
                            rma.ins = rma.ins, 
                            int.invst = invst.int, 
                            int.loan = loan.int,
                            start.cash = 0,
                            assets.cow = base.assets.cow,
                            cap.sales = base.cap.sales,
                            cap.purch = base.cap.purch,
                            cap.taxes = base.cap.taxes)
  
####Drought Occurs####
# For each option, we calculate the **CHANGE** in costs
# and the **CHANGE** in revenues relative to the no drought baseline.

# Calculate vector of days of drought adaptation action for each year
days.act <- CalculateDaysAction(act.st.yr, act.st.m, act.end.yr, act.end.m, drought.action)

## Option 0: No adaptation ##
# drought revenues
noadpt.rev.calf <- unlist(lapply(1:t,function(i){
  CalculateExpSales(herd = herd, calf.sell = calf.sell, wn.wt = wn.wt[i], p.wn.yr1 = p.wn[i])
}))

out.noadpt <- OptionOutput(t = t,
                           opt = "noadpt",
                           rev.calf = noadpt.rev.calf, 
                           cost.op = rep(base.op.cost,t), 
                           rma.ins = rma.ins, 
                           int.invst = invst.int, 
                           int.loan = loan.int,
                           start.cash = 0,
                           assets.cow = base.assets.cow,
                           cap.sales = base.cap.sales,
                           cap.purch = base.cap.purch,
                           cap.taxes = base.cap.taxes)


## Option 1: Buy additional feed
days.feed <- days.act  # Assumes that feeding days are equivalent to drought adaptation action days

# Calculate operating costs including costs to buy feed
feed.cost <- CalculateFeedCost(kHayLbs, kOthLbs, p.hay, p.oth, days.feed, herd) + base.op.cost

out.feed <- OptionOutput(t = t,
                         opt = "feed", 
                         rev.calf = base.sales, 
                         cost.op = feed.cost, 
                         rma.ins = rma.ins,
                         int.invst = invst.int,
                         int.loan = loan.int,
                         start.cash = 0,
                         assets.cow = base.assets.cow,
                         cap.sales = base.cap.sales,
                         cap.purch = base.cap.purch,
                         cap.taxes = base.cap.taxes)

## Option 2: Truck pairs to rented pasture
days.rent <- days.act # Assumes that pasture rental days are equivalent to drought adaptation action days

# Calculate calf revenues in drought after trucking pairs to rented pasture
calf.rev.rentpast <- CalculateRentPastRevenue(expected.wn.wt = expected.wn.wt, 
                                              calf.loss = calf.loss, 
                                              calf.wt.adj = calf.wt.adj,
                                              calf.sell = calf.sell, 
                                              herd = herd, 
                                              p.wn = p.wn)

# Calculate operating costs to truck pairs to rented pasture. Assumes base operating cost is unchanged.
cost.op.rentpast <- CalculateRentPastCost(n.miles = n.miles, 
                                          truck.cost = truck.cost, 
                                          past.rent = past.rent,
                                          days.rent = days.rent, 
                                          oth.cost = oth.cost, 
                                          max.wt = max.wt,
                                          cow.wt = cow.wt, 
                                          calf.wt = calf.wt, 
                                          herd = herd) + base.op.cost

out.rentpast <- OptionOutput(t = t,
                             opt = "rentpast",
                             rev.calf = calf.rev.rentpast, 
                             cost.op = cost.op.rentpast, 
                             rma.ins = rma.ins, 
                             int.invst = invst.int, 
                             int.loan = loan.int,
                             start.cash = 0,
                             assets.cow = base.assets.cow,
                             cap.sales = base.cap.sales,
                             cap.purch = base.cap.purch,
                             cap.taxes = base.cap.taxes)

## Option 3: Sell pairs and replace cows

calf.rev.sellprs <- CalculateSellPrsRev(base.sales = base.sales, 
                                        herd = herd, 
                                        wn.succ = wn.succ, 
                                        calf.wt = calf.wt, 
                                        p.calf.t0 = p.calf.t0)

cost.op.sellprs <- CalculateSellPrsCost(op.cost.adj = op.cost.adj, 
                                        herd = herd, 
                                        sell.cost = sell.cost, 
                                        base.op.cost = base.op.cost, 
                                        herdless.op.cost = herdless.op.cost)

assets.cow.sellprs <- CalcCowAssets(herd = herd, 
                            p.cow = p.cow, 
                            sell.year = 1, 
                            replace.year = 3)

c(cap.sales, cap.purch) := CalcCapSalesPurch(assets.cow = assets.cow, 
                                             t=t, 
                                             cull.num = cull.num, 
                                             p.cow = p.cow)

cap.taxes <- CalcCapTaxes(cap.sales = cap.sales, 
                          cap.purch = cap.purch, 
                          cap.tax.rate = cap.tax.rate,
                          herd = herd,
                          p.cow = p.cow)

out.sellprs <- OptionOutput(t = t,
                            opt = "sellprs",
                            rev.calf = calf.rev.sellprs, 
                            cost.op = cost.op.sellprs, 
                            rma.ins = rma.ins, 
                            int.invst = invst.int, 
                            int.loan = loan.int,
                            start.cash = 0,
                            assets.cow = assets.cow,
                            cap.sales = cap.sales,
                            cap.purch = cap.purch,
                            cap.taxes = cap.taxes)

## Option 4: Sell pairs and don't replace

calf.rev.sellprs.norepl <- c(calf.rev.sellprs[1],rep(0,(t-1)))

cost.op.sellprs.norepl <- c(cost.op.sellprs[1],rep(herdless.op.cost,(t-1)))

assets.cow <- CalcCowAssets(t = t, 
                            herd = herd, 
                            p.cow = p.cow, 
                            sell.year = 1)

c(cap.sales, cap.purch) := CalcCapSalesPurch(assets.cow = assets.cow, 
                                             t=t, 
                                             cull.num = cull.num, 
                                             p.cow = p.cow)

cap.taxes <- CalcCapTaxes(cap.sales = cap.sales, 
                          cap.purch = cap.purch, 
                          cap.tax.rate = cap.tax.rate,
                          herd = herd,
                          p.cow = p.cow)

out.sellprs.norepl <- OptionOutput(t = t,
                                   opt = "sellprs.norepl",
                                   rev.calf = calf.rev.sellprs.norepl, 
                                   cost.op = cost.op.sellprs.norepl, 
                                   rma.ins = rma.ins, 
                                   int.invst = invst.int, 
                                   int.loan = loan.int,
                                   start.cash = 0,
                                   assets.cow = assets.cow,
                                   cap.sales = cap.sales,
                                   cap.purch = cap.purch,
                                   cap.taxes = cap.taxes)

## Bringing outcome df's from each option together
outcomes <- rbind(out.nodrght, out.noadpt, out.feed, out.rentpast, out.sellprs, out.sellprs.norepl)

