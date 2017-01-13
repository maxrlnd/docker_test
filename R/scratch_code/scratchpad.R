# Cow sales (sell in year 1, no sales in years 2 and 3)
cow.sales <- c(p.cow * (herd - cull), 0, 0)
cow.sales.nocull <- c(p.cow * herd, 0, 0)  # Cow sales not including a reduction from normally culled cows

# Interest income from cow sales
pr <- c(cow.sales.nocull[1],0,0,0)  # Investment principle. Incorrect carry over from excel file. Change to cow.sales
int.inc <- NULL
for (i in 1:3) {
  if (i == 1) {
    int.inc[i] <- pr[i] * invst.int / 365 * days.act
    pr[i + 1] <- pr[i] + int.inc[i]
  }
  else {
    int.inc[i] <- pr[i] * invst.int
    pr[i + 1] <- pr[i] + int.inc[i]
  }
}




OptionOutput <- function(opt, nodrought = FALSE, rev.calf, rev.oth = NULL, 
                         cost.op, rma.ins, int.invst, int.loan, cow.sales = rep(0,5), 
                         cow.repl = rep(0,5), cap.tax.rate = 0.15) {
  # Function: OptOutput
  # Desciption: Takes in cost and revenue variables and outputs data.frame with 
  # all relevant outcome variables
  #
  # Inputs:
  # opt = String to label option: "nodrght", "noadpt", etc.
  # nodrought = OPTIONAL value. default is set to false. set to true for no drought option
  # rev.calf = 5x1 vector of revenue from actual calf sales in years 1 through 5
  # rev.oth = OPTIONAL 5x1 vector of non-calf revenues (created to account for interest on sale of cows in year 1)
  # int.invst = Interest rate on investments
  # int.loan = Interest rate on loans
  # rma.ins = 5x3 matrix of insurance year, premium, and payouts
  # cost.op = 5x1 vector of operating costs for years 1 through 5, including any adaptation costs
  
  opt <- rep(opt, 11)
  t <- c(0,rep(1:5,2))
  ins <- c(0,rep(1,5),rep(0,5))
  
  rev.calf <- c(0,rep(rev.calf, 2))
  cost.op <- c(0, rep(cost.op, 2))
  
  if(nodrought == FALSE) {
    rev.ins <- c(0, rma.ins[, 3], rep(0, 5))  # potential payout for 2:6, no payout for 7:11
  } else {
    rev.ins <- rep(0,11) # no drought, no payout
  }
  
  cost.ins <- c(0, rma.ins[, 2], rep(0,5))  # insurance for 2:6, no insurance for 7:11
  
  rev.int <- c(0, rep(0, 10)) #PLACEHOLDER. NOT CURRENTLY CALCULATING INTEREST
  cost.int <- c(0, rep(0, 10))  # PLACEHOULDER. NOT CURRENTLY CALCULATING INTEREST
  
  if(!is.null(rev.oth)) {  # if other revenues are passed through, then they are included in total revenues
    rev.oth <- c(0, rep(rev.oth, 2))
    rev.tot <- rev.calf + rev.ins + rev.int + rev.oth
  } else {
    rev.tot <- rev.calf + rev.ins + rev.int
  }
  
  cost.tot <- cost.op + cost.ins + cost.int
  profit <- rev.tot - cost.tot
  taxes <- ifelse(profit > 0, profit * (0.124+0.15+0.04), 0)  # taxes only if positive profits. i wonder if EITC applies here?
  aftax.inc <- profit - taxes
  
  cap.sales <- c(0, rep(cow.sales, 2))  # assumes sale of cows is only capital sales
  cap.purch <- c(0, rep(cow.repl, 2))  # assumes purchase of replacement cows is only capital purchase
  
  is.positive <- function(x) {
    x > 0
  }
  
  x <- which(cap.sales > 0) %>%
    length > 0
  y <- which(cap.purch > 0) %>%
    length > 0
  
  if (x == TRUE & y == TRUE) {  # then cows have been sold and replaced
    cap.taxes == rep(0,t)  # no capital taxes should be incurred
  }
  if (x == TRUE & y == FALSE) {  # then cows have been sold and not replaced
    cap.taxes == cap.sales * cap.tax.rate
  }
  
  y <- is.positive(cap.purch)
  
  if (length(x) > 0 & length())
    
    for (i in 1:length(cap.sales)) {
      if (cap.sales > 0) {
        
      }
    } 
  cap.taxes <- 
    
    
    cap.taxes <- if(rep(NA, 11)
                    assets.cow <- rep(NA, 11)
                    assets.cash <- rep(NA, 11)
                    net.wrth <- rep(NA, 11)
                    
                    out <- cbind(opt, t, ins, rev.calf, rev.ins, rev.int, rev.tot, cost.op, 
                                 cost.ins, cost.int, cost.tot, profit, taxes, aftax.inc,
                                 cap.sales, cap.taxes, assets.cow, assets.cash, net.wrth)
}  
