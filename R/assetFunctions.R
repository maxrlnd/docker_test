# Assets Functions --------------------------------------------------------

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
    if(t != 1){
      cow.assets <- c(herd * p.cow, cow.assets)  # Adding time 0 cow assets to make a 6x1 vector
    }
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
  if(t > 1){
    cap.sales <- c(0, rep(NA, t))
    cap.purch <- c(0, rep(NA, t))
  }else{
    cap.sales <- NA
    cap.purch <- NA
  }
  
  for (i in 2:n) {
    # If cow assets increase and they were not 0 in the prior year, then cap sales equal to normal culling and
    if(assets.cow[i] > assets.cow[i-1] & assets.cow[i-1] != 0) {
      cap.sales[i] <- cull.num * p.cow  # Normal culling
      cap.purch[i] <- assets.cow[i] - assets.cow[i-1]
      print("hello")
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

