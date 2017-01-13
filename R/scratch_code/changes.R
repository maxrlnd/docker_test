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
  cap.sales <- c(0, rep(NA, t), 0, rep(NA, t))
  cap.purch <- c(0, rep(NA, t), 0, rep(NA, t))
  for (i in 2:n) {
    # If cow assets increase and they were not 0 in the prior year, then cap sales equal to normal culling and
    if(assets.cow[i] > assets.cow[i-1] & assets.cow[i-1] != 0) { 
      cap.sales[i] <- cull.num * p.cow  # Normal culling 
      cap.purch[i] <- assets.cow[i] - assets.cow[i-1]
    }
    # If cow assets increase and they were 0 in the prior year, then no cap sales and cap purchases equal to change in assets
    if(assets.cow[i] > assets.cow[i-1] & assets.cow[i] == 0) { 
      cap.sales[i] <- 0
      cap.purch[i] <- assets.cow[i] - assets.cow[i-1]
    }
    # If cow assets decrease, then capital sales are equal to the change in cow assets, then cap sales equal to the change in assets
    if(assets.cow[i] < assets.cow[i - 1]) {
      cap.sales[i] <- assets.cow[i-1] - assets.cow[i] 
      cap.purch[i] <- 0
    }
    # If cow assets are unchanged, then capital sales are equal to the normal culling
    if(assets.cow[i] == assets.cow[i - 1]) {
      cap.sales[i] <- cull.num * p.cow
      cap.purch[i] <- 0
    }
  }
  list(cap.sales, cap.purch)
}



Vars:
  cull.num <- 15  # Number of cows culled in a normal year