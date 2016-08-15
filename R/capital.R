CalcCapTaxes <- function()  

cap.sales <- rep(NA, 11)
n <- length(cap.sales)
cap.taxes <- rep(0,n)

# Capital sales and replacement assumes that the entire herd is sold and replaced at the same rate. 
# Not sure how the tax code treats changes in prices. This abstracts away from that. 
# The price dynamics could matter here, but for now we are leaving them out.
for (i in 1:n) {
  if(cap.sales[i] > 0 & cap.purch[i] == 0 & cap.purch[i+1] == 0 & cap.purch[i+2] == 0 & drought.emrg == 1) {  # if herd is sold and not replaced by the end of the 2nd year after the purchase and there is a drought emergency
    cap.taxes[i+1] <- cap.sales[i] * cap.tax.rate  # then the capital taxes can be delayed by one year
  }
  if(cap.sales[i] > 0 & cap.purch[i] == 0 & cap.purch[i+1] == 0 & cap.purch[i+2] == 0 & drought.emrg == 0) {  # if herd is sold and not replaced by the end of the 2nd year after the purchase and there is not a declared drought emergency
    cap.taxes[i+1] <- cap.sales * cap.tax.rate  # then the capital taxes can be delayed by one year
  }  
}

assets.cash <- rep(NA, 11)
net.wrth <- rep(NA, 11)