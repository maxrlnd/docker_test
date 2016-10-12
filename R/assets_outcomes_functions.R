# Calculating Assets and Total Net Worth

CalcCowAssets <- function(t, herd, p.cow, sell.year = NA, replace.year = NA) {
  # Function: CapitalAssets
  # Description: Caluclated the cow assets for each year.
  
  # Inputs:
  #  herd
  #  p.cow
  #  sell.year = Single numeric value. Equal to t where year 1 is t=1.
  #  replace.year = Single numeric value. Equal to t where year 1 is t=1.
  
  # Output:
  #  6x1 vector of cow assets for each year, including t=0
  
  cow.assets <- rep(NA,t)
  
  if(is.na(sell.year)) {
    cow.assets[1:t] <- herd * p.cow
    cow.assets <- c(herd * p.cow, cow.assets)  # Adding time 0 cow assets to make a 6x1 vector
    return(cow.assets)
  }
  
  if(sell.year > 0 & is.na(replace.year)) {
    cow.assets[1:sell.year] <- herd * p.cow  # Allows for sale of herd outside of year 1
    cow.assets[sell.year:t] <- 0  # Replaces sell year with 0, leaves prior years with herd, all subsequent years with no herd 
    cow.assets <- c(herd * p.cow, cow.assets)  # Adding time 0 cow assets to make a 6x1 vector
    return(cow.assets)
  }  
  
  if(sell.year > 0 & replace.year > 0 ) {
    cow.assets[1:sell.year] <- herd * p.cow  # Allows for sale of herd outside of year 1
    cow.assets[sell.year:(replace.year-1)] <- 0  # Replaces sell year with 0, leaves prior years with herd
    cow.assets[replace.year:5] <- herd * p.cow  # After replacing, assumes no additional sales
    cow.assets <- c(herd * p.cow, cow.assets)  # Adding time 0 cow assets to make a 6x1 vector
    return(cow.assets)
  }
}



CalcCapSalesPurch <- function(assets.cow, t, cull.num, p.cow) {
  # Description: Calculates vectors of capital sales and capital purchases from 
  #  changes in assets.cow. Assumes sale/purchase of cows is only capital sales/purchase
  #
  # Inputs: 
  #  assets.cow = tx1 vector of the value of cow assets each year.
  #
  # Outputs:
  #  cap.sales = tx1 vector of capital sales for each year
  #  cap.purch = tx1 vector of capital purchases for each year
  n <- length(assets.cow)
  cap.sales <- c(0, rep(NA, t))
  cap.purch <- c(0, rep(NA, t))
  for (i in 2:n) {
    # If cow assets increase and they were not 0 in the prior year, then cap sales equal to normal culling and
    if(assets.cow[i] > assets.cow[i-1] & assets.cow[i-1] != 0) { 
      cap.sales[i] <- cull.num * p.cow  # Normal culling 
      cap.purch[i] <- assets.cow[i] - assets.cow[i-1]
    }
    # If cow assets increase and they were 0 in the prior year, then no cap sales and cap purchases equal to change in assets
    if(assets.cow[i] > assets.cow[i-1] & assets.cow[i - 1] == 0) { 
      cap.sales[i] <- 0
      cap.purch[i] <- assets.cow[i] - assets.cow[i-1]
    }
    # If cow assets decrease, then capital sales are equal to the change in cow assets, then cap sales equal to the change in assets
    if(assets.cow[i] < assets.cow[i - 1]) {
      cap.sales[i] <- assets.cow[i-1] - assets.cow[i] 
      cap.purch[i] <- 0
    }
    # If cow assets are unchanged, then capital sales are equal to the normal culling
    if(assets.cow[i] == assets.cow[i - 1] & assets.cow[i] != 0) {
      cap.sales[i] <- cull.num * p.cow
      cap.purch[i] <- 0
    }
    # If cow assets are unchanged at 0, then capital sales and purch are 0
    if(assets.cow[i] == assets.cow[i - 1] & assets.cow[i] == 0) {
      cap.sales[i] <- 0
      cap.purch[i] <- 0
    }
  }
  list(cap.sales, cap.purch)
}


CalcCapTaxes <- function(herd, p.cow, cap.sales, cap.purch, cap.tax.rate, drought.emrg = 1)  {
  # Function: CalcCapTaxes
  # Description: Calculates capital taxes on herd sales. Tax treatment is 
  # different depending on whether herd is sold and replaced by the end of the
  # third year or if the herd is sold and not replaced during a drought emergency.
  # Assumes that the entire herd is sold and replaced at the same rate. 
  # Not sure how the tax code treats changes in prices. This abstracts away from that. 
  # The price dynamics could matter here, but for now we are leaving them out.
  #
  # Inputs: 
  #  cap.sales
  #  cap.purch
  #  cap.tax.rate
  #  drought.emrg = Binary variable to indicate whether drought emergency was in place when the herd was sold. 
  #    currently set to a default of 1. This only matters if the herd is sold and not replaced.
  #
  # Outputs:
  #  cap.taxes <- 5x1 vector of capital taxes
  
  n <- length(cap.sales)
  cap.taxes <- cap.sales * cap.tax.rate  # default value is standard capital tax rate
  herd.value <- herd * p.cow
  
  # Special tax treatment for herd sales due to drought:
  for (i in 1:n) {
    if(cap.sales[i] == herd.value & cap.purch[i] == 0 & cap.purch[i+1] == 0 & cap.purch[i+2] == 0 & drought.emrg == 1) {  # if herd is sold and not replaced by the end of the 2nd year after the purchase and there is a drought emergency
      cap.taxes[i] <- 0
      cap.taxes[i+1] <- cap.sales[i] * cap.tax.rate  # then the capital taxes can be delayed by one year
    }
    if(cap.sales[i] == herd.value & cap.purch[i] == 0 & cap.purch[i+1] == 0 & cap.purch[i+2] == 0 & drought.emrg == 0) {  # if herd is sold and not replaced by the end of the 2nd year after the purchase and there is not a declared drought emergency
      cap.taxes[i] <- cap.sales[i] * cap.tax.rate  # then the capital taxes occur in the year of the sale
    }  
    if(cap.sales[i] == herd.value & cap.purch[i+1] == herd.value | cap.sales[i] == herd.value & cap.purch[i+2] == herd.value ) {   # if herd is sold and replaced within 2 years, then there are no capital taxes
      cap.taxes[i] <- 0
    }
  }
  cap.taxes
}

OptionOutput <- function(t, opt, nodrought = FALSE, rev.calf, rev.oth = NULL, 
                         cost.op, rma.ins, int.invst, int.loan, start.cash, 
                         assets.cow, cap.sales, cap.purch, cap.taxes) {
  # Function: OptOutput
  # Desciption: Takes in cost and revenue variables and outputs data.frame with 
  # all relevant outcome variables
  #
  # Inputs:
  # t = Number of years
  # opt = String to label option: "nodrght", "noadpt", etc.
  # nodrought = OPTIONAL value. default is set to false. set to true for no drought option
  # rev.calf = tx1 vector of revenue from actual calf sales in years 1 through t
  # rev.oth = OPTIONAL tx1 vector of non-calf revenues (created to account for interest on sale of cows in year 1)
  # int.invst = Interest rate on investments
  # int.loan = Interest rate on loans
  # rma.ins = tx3 matrix of insurance year, premium, and payouts
  # cost.op = tx1 vector of operating costs for years 1 through t, including any adaptation costs
  # int.invst = interest rate on positive cash assets (savings)
  # int.loan = interest rate on negative cash asssets (loans)
  # start.cash = starting cash assets at t=0
  # assets.cow = tx1 vector of the value of cow assets in each year
  #
  # Outputs:
  #  out = dataframe of all major variables of interest:
  #   opt
  #   yr, 
  #   ins, 
  #   rev.calf
  #   rev.ins
  #   rev.int
  #   rev.tot
  #   cost.op 
  #   cost.ins 
  #   cost.int
  #   cost.tot
  #   profit
  #   taxes
  #   aftax.inc
  #   cap.sales 
  #   cap.purch 
  #   cap.taxes 
  #   assets.cow
  #   assets.cash
  #   net.wrth
  
  n <- (t + 1) * 2   # sets length as years plus 1 for initial year, times 2 for insurance and no insurance
  option <- rep(opt, n)
  yr <- c(0, 1:t, 0, 1:t)
  ins <- c(rep(1, t+1), rep(0, t+1))
  
  rev.calf <- c(0, rev.calf, 0, rev.calf)
  cost.op <- c(0, cost.op, 0, cost.op)
  
  cost.ins <- c(0, rma.ins[, 2], 0, rep(0, t))  # insurance for 2:6, no insurance for 8:12
  
  if(nodrought == FALSE) {
    rev.ins <- c(0, rma.ins[, 3], 0, rep(0, t))  # potential payout for 2:6, no payout for 8:12
  } else {
    rev.ins <- rep(0, n) # no drought, no payout
  }
  
  if(!is.null(rev.oth)) {  # if other revenues are passed through, then they are included in total revenues
    rev.oth <- c(0, rev.oth, 0, rev.oth)
    rev.tot.noint <- rev.calf + rev.ins + rev.oth
  } else {
    rev.tot.noint <- rev.calf + rev.ins 
  }
  
  out <- data.frame(opt, yr, ins, rev.calf, rev.ins, rev.tot.noint, cost.op, cost.ins, cap.sales, cap.purch, cap.taxes, assets.cow)
  
  #WARNING: UGLY UGLY CODE AHEAD. Get a handle on dplyr and revisit.
  #Split into ins and non-insurance; calculate interest income, profits, and cash assets; then put back together
  out.ins <- out[out$ins==1, ]
  out.noins <- out[out$ins==0,]
  
  out.ins$rev.int <- c(0, rep(NA, t))
  out.ins$cost.int <- c(0, rep(NA, t))
  out.ins$rev.tot <- c(0, rep(NA, t))
  out.ins$cost.tot <- c(0, rep(NA, t))
  out.ins$profit <- c(0, rep(NA, t))
  out.ins$taxes <- c(0, rep(NA, t))
  out.ins$aftax.inc <- c(0, rep(NA, t))
  out.ins$assets.cash <- rep(start.cash, t+1)
  for (i in 2:(t+1)) {
    if(out.ins$assets.cash[i - 1] > 0) {
      out.ins$rev.int[i] <- out.ins$assets.cash[i - 1] * (invst.int)
    } else {
      out.ins$rev.int[i] <- 0
    }
    if(out.ins$assets.cash[i - 1] < 0) {
      out.ins$cost.int[i] <- -1 * (out.ins$assets.cash[i - 1] * (loan.int))
    } else {
      out.ins$cost.int[i] <- 0
    }
    out.ins$rev.tot[i] <- out.ins$rev.int[i] + out.ins$rev.tot.noint[i]
    out.ins$cost.tot[i] <- out.ins$cost.int[i] + out.ins$cost.op[i] + out.ins$cost.ins[i]
    out.ins$profit[i] <- out.ins$rev.tot[i] - out.ins$cost.tot[i]
    out.ins$taxes[i] <- ifelse(out.ins$profit[i] > 0, out.ins$profit[i] * (0.124+0.15+0.04), 0)  # taxes only if positive profits. i wonder if EITC applies here?
    out.ins$aftax.inc[i] <- out.ins$profit[i] - out.ins$taxes[i]
    out.ins$assets.cash[i] <- out.ins$assets.cash[i-1] + out.ins$aftax.inc[i] + out.ins$cap.sales[i] - out.ins$cap.purch[i] - out.ins$cap.taxes[i]
  }
  
  out.noins$rev.int <- c(0, rep(NA, t))
  out.noins$cost.int <- c(0, rep(NA, t))
  out.noins$rev.tot <- c(0, rep(NA, t))
  out.noins$cost.tot <- c(0, rep(NA, t))
  out.noins$profit <- c(0, rep(NA, t))
  out.noins$taxes <- c(0, rep(NA, t))
  out.noins$aftax.inc <- c(0, rep(NA, t))
  out.noins$assets.cash <- rep(start.cash, t+1)
  for (i in 2:(t+1)) {
    if(out.noins$assets.cash[i - 1] > 0) {
      out.noins$rev.int[i] <- out.noins$assets.cash[i - 1] * (invst.int)
    } else {
      out.noins$rev.int[i] <- 0
    }
    if(out.noins$assets.cash[i - 1] < 0) {
      out.noins$cost.int[i] <- -1 * (out.noins$assets.cash[i - 1] * (loan.int))
    } else {
      out.noins$cost.int[i] <- 0
    }
    out.noins$rev.tot[i] <- out.noins$rev.int[i] + out.noins$rev.tot.noint[i]
    out.noins$cost.tot[i] <- out.noins$cost.int[i] + out.noins$cost.op[i] + out.noins$cost.ins[i]
    out.noins$profit[i] <- out.noins$rev.tot[i] - out.noins$cost.tot[i]
    out.noins$taxes[i] <- ifelse(out.noins$profit[i] > 0, out.noins$profit[i] * (0.124+0.15+0.04), 0)  # taxes only if positive profits. i wonder if EITC applies here?
    out.noins$aftax.inc[i] <- out.noins$profit[i] - out.noins$taxes[i]
    out.noins$assets.cash[i] <- out.noins$assets.cash[i-1] + out.noins$aftax.inc[i] + out.noins$cap.sales[i] - out.noins$cap.purch[i] - out.noins$cap.taxes[i]
  }
  
  
  out <- rbind(out.ins, out.noins)  # Recombine ins and no ins dataframes
  out$rev.tot.noint <- NULL  # Remove total revenue without insurance variable
  out$net.wrth <- out$assets.cash + out$assets.cow  # Calculate total net worth
  
  # Reorder variables for output
  out <- out[c("opt", "yr", "ins", "rev.calf", "rev.ins", "rev.int", "rev.tot",  
               "cost.op", "cost.ins", "cost.int", "cost.tot", "profit", "taxes", 
               "aftax.inc", "cap.sales", "cap.purch", "cap.taxes", "assets.cow", 
               "assets.cash", "net.wrth")]
  out
}  
