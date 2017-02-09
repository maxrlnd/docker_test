filter.titles <- list("opt" = c("Buy Feed", "Drought and No Adaptation", "No Drought", "Rent Pasture", "Sell Pairs"), 
                      "yr" = c(1950:2017), "ins" = c("With Insurance","No Insurance"),
                      "cols" = c("opt", "yr", "ins", "rev.calf", "rev.ins", "rev.int", "rev.tot", 
                                 "cost.op", "cost.ins", "cost.int", "cost.tot", "profit", "taxes", 
                                 "aftax.inc", "cap.sales", "cap.purch", "cap.taxes", "assets.cow", 
                                 "assets.cash", "net.wrth", "sim.index"))
shapeDrawn <- F
load("data/insGridShp.RData")
