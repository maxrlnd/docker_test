# Load Libraries
library(pracma)
library("tseries")
require(lubridate)
require(dplyr)
library(raster)
library("ncdf4")
library("rgdal")
library("fields")
library(data.table)
library(spdep)
library(ggplot2)
library("rasterVis")
library("RSelenium")
library("trend")
library(rgeos)
library("maptools")
library(rgdal)
library(readxl)
library(fpc)

load("data/grid_base.RData") # Load base grid data
load("data/insurance_base.RData") # Load base insurance data

# Load gridded precip
# Get the gridded precip file if not in 'data' folder
# ideally I can get this to work with git-lfs in the future...
if(!"noaaIndex.RData" %in% list.files("data")){
  cat("No NOAA Index data found. Fetching the files...\n")
  download.file("https://o365coloradoedu-my.sharepoint.com/personal/jotu9073_colorado_edu/_layouts/15/guestaccess.aspx?guestaccesstoken=CLfTQ%2fVIj3v7cel5UkM65Noqx%2bHEgIDoS0lzqM5FHUw%3d&docid=08a40bf084add47629568d4d4161e283e&rev=1",destfile="data/noaaIndex.RData")
}
load("data/noaaIndex.RData")

gridToRaster <- function(grid, rasterTemplate){
  # Author: Adam
  # 
  # Simple wrapper to turn a matrix into a raster using a template
  # 
  # Args:
  #   grid: data in matrix format
  #   rasterTemplate: template to use in rasterization of the matrix
  #
  # Returns: Raster of grid data
  #
  require(raster)
  return(raster(grid, template = rasterTemplate))
}


dataToRast<-function(inData,target.var=NULL){
  
  "
  Author: Joe

  Convert a data.frame/data.table field
  to raster. 

  inData: input data frame/table. 

  target.var: character representing fields
    to map to raster grid
  "
  
  grd.idx=match(inData$grid,as.numeric(gridMatrix))
  dataRast=gridMatrix
  dataRast[grd.idx]=inData[[target.var]]
  dataRast[-grd.idx]=NA
  dataRast=gridToRaster(dataRast,tempRaster)
  return(dataRast)
  
}

droughtCalculator<-function(yy,clv,acres,pfactor,insPurchase,mask=NULL){
  
  "
  Author: Joe
  
  Emulates RMA's precipitation-based
  insurance index in raster. NOTE
  that premium/indemnity estimates
  will be slightly off those of RMA
  because our index values 'intervalNOAA'
  slightly disagree.
  
  yy: Year of interest.
  
  clv: RMA coverage level. Accepted values 
  are 0.7, 0.75, 0.8, 0.85, 0.9
  
  acres: Insured acres of grazing land. 
  
  pfactor: Productivity factor of grazing land.
  
  insPurchase: a matrix of intervals from 1-11
  for which insurance is purchased. For example,
  purchases for the April-May and May-June intervals
  at 50% protection each would be entered as 
  
  `rbind(c(3,0.5),c(5,0.5))`
  
  Consecutive intervals are not allowed. 
  
  Returns a list of outputs: 
  
  $prem_noSbdy: total premium with subsidy
  $prem_wSbdy: total premium without subsidy
  $prodPrem: producer premium
  $indemrate: indemnity rate (stack, by month)
  $indemnity: indemnity (stack, by month)
  #indemtot: total indemnity 
  
  "
  
  ##Make sure inputs are valid
  if(!yy %in% 2000:2015){
    stop("Input year must occur in the range 2000-2015.")
  }
  # if(!clv %in% round(seq(0.7,0.9,by=0.05),1)){
  #   stop("Invalid coverage level. Accepted coverage levels are 0.7, 0.75, 0.8, 0.85, 0.9.")
  # }
  # if(!pfactor %in% round(seq(0.6:1.5,by=0.01),1)){
  #   stop("Productivity factor must range from 0.6-1.5, in increments of 0.01.")
  # }
  if(min(dist(insPurchase[,1]))==1){
    stop("Insurance allocation for consecutive intervals is not permitted.")
  }
  if(nrow(insPurchase)<=1){
    stop("Insurance must be allocated for at least two intervals.")
  }
  if(prod(insPurchase[,1] %in% 1:11)==0){
    stop("Insurance allocation intervals must range from 1-11.")
  }
  if(max(insPurchase[,2])>0.6){
    stop("Insurance allocation may not exceed 60% per interval.")
  }
  if(sum(insPurchase[,2])!=1){
    stop("Insurance allocation must sum to 100%.")
  }
  
#   ##Clip by mask if specified
#   if(!is.null(mask)){
#     mask=subRast(gridRast,mask) #assign grid indices to mask
#     basePrice=subRast(basePrice,mask)
#     prem100=prem100[grid %in% as.numeric(as.matrix(mask)),]
#     intervalNOAA=intervalNOAA[grid %in% as.numeric(as.matrix(mask)),]
#     grid_elig=grid_elig[grid_elig$grid %in% as.numeric(as.matrix(mask)),]
#   }
  
  ##Get subsidy rate based on coverage level
  sbdy=covsub[coverage.trigger==clv,subsidy.rate]
  
  ##Set up insurance purchase vector
  ip=rep(0,11)
  ip[insPurchase[,1]]=insPurchase[,2]
  insPurchase=ip
  
  ##Calculate policy rate
  plrt=prod(clv,acres,pfactor)*basePrice
  
  ##Generate inputs for computing premiums
  premInt=stack() #Premium/$100 rate
  protection=stack() #Protection amount
  actualidx=stack() #"Actual Index Value" from RMA (precip index)
  eligmask=stack() #eligibility mask
  for (i in which(insPurchase>0)){
    
    premInt=stack(premInt,
                  dataToRast(prem100,paste0("i",i)))
    
    protection=stack(protection,
                     plrt*insPurchase[i])
    
    actualidx=stack(actualidx,
                    dataToRast(intervalNOAA[Year==yy & interval==i,value,grid],"value")/100)
    
    eligmask=stack(eligmask,
                   dataToRast(data.frame(grid=grid_elig$grid,
                                         value=ifelse(grid_elig[[paste0("i",i)]]==1,1,NA)),"value"))
    
  }
  
  # names(premInt)=paste0("i",which(insPurchase>0))
  # names(protection)=paste0("i",which(insPurchase>0))
  # names(actualidx)=paste0("i",which(insPurchase>0))
  
  ##Compute premiums
  prem_noSbdy=sum(premInt*protection*0.01)
  prem_wSbdy=prem_noSbdy*sbdy
  prodPrem=prem_noSbdy*(1-sbdy)
  
  ##round premiums to match RMA 
  #(as closely as possible)
  prem_noSbdy=round(prem_noSbdy,2)
  prem_wSbdy=round(prem_wSbdy,2)
  prodPrem=round(prodPrem,2)
  actualidx=round(actualidx,3)
  
  ##Compute indemnities
  rma_rc=rbind(c(-Inf,clv,1),c(clv,Inf,0))
  iscov=reclassify(actualidx,rma_rc) #binary raster of payouts triggered at 'clv'
  indemrate=((clv-actualidx)/clv)*iscov*eligmask #pct diff from 'clv' if payout triggered
  indemnity=indemrate*protection
  indemtot=sum(indemnity) #total indemnity amount
  
  ##Prepare outputs
  outList=list()
  outList$prem_noSbdy=prem_noSbdy
  outList$prem_wSbdy=prem_wSbdy
  outList$prodPrem=prodPrem
  outList$indemrate=indemrate
  outList$indemnity=indemnity
  outList$indemtot=indemtot
  
  return(outList)
  
}

dcInfo<-function(dc,tgrd){
  
  "
  Author: Joe
  
  Extracts drought calculator info from a 
  grid cell. 

  dc: 'droughtCalculator' output (class LIST)

  tgrd: target grid cell ID
  "
  
  dcinf=lapply(dc,function(X){
    
    extract(X,rastPt[rastPt$layer==tgrd,])
    
  })
  
  return(dcinf)
  
}

CalculateDaysAction <- function(act.st.yr, act.st.m, act.end.yr, act.end.m) {

	"
	 Function: CalculateDaysAction
 Description: Calculate the number of days rancher pays for a drought adaptation action.
 NOTE: This function assumes that the actions take place only in one year.

 Inputs:
  act.st.yr = Year the action starts
  act.st.m = Month the action starts
  act.end.yr = Year the action ends
  act.end.m = Month the action ends

 Outputs:
  days.act = Number of days drought adaptation action takes place (days)
	"

  # Warning about end month
  warning("Start and End month assumes that action starts/stops on the first of the month")
  
  # Error handling
  if (act.st.yr < 1) {
    stop("Action start year ", act.st.yr, "is not greater than 0")
  }
  if (act.st.yr > act.end.yr | act.st.yr == act.end.yr & act.st.m > act.end.m) {
    stop("Action end occurs before action start")
  }
  if (act.st.m < 1 | act.st.m > 12 | act.end.m < 1 | act.end.m > 12) {
    stop("Invalid action start or end month (not between 1 and 12)")
  }
  
  if (act.st.yr == act.end.yr) {
    days.act <- (act.end.m - act.st.m) * 30
  }
  
  # TEMPORARY ERROR: While the model is not equipped to handle multi-year droughts, we will not allow drought
  #  adaptation across multiple years.
  if (act.st.yr != act.end.yr) {
    stop("Model not equipped for multi-year drought adaptation. act.st.yr must equal act.end.yr")
  }
  
  return(days.act)
}

CalculateFeedCost <- function(kHayLbs, kOthLbs, p.hay,p.oth, days.feed, herd) {

	"
	 Function: CalculateFeedCost
 Description: Calculating the costs of purchasing additional feed

 Inputs:
  kHayLbs = Number of pounds of additional hay needed for each cow each day (pounds/head/day).  (Source: UNKNOWN)
  p.hay = Price of hay ($/ton). User input.
  kOthLbs = Number of pounds of additional other feed needed for each cow each day (pounds/head/day). (Source: UNKNOWN)
  p.oth = Price of other feed ($/ton). Currently not a user input. Does not come into play since the model assumes only feeding hay
  days.feed = Number of days additional feed is needed. Generally, equal to days.act. (days)
  herd = Size of herd (head of cows, does not include calves)

 Outputs:
 cost.feed = Additional costs to feed the herd over the remainder of the season ($/year)
"
  # Calculate cost per cow per day * days of feed for the year * number of cows in the herd
  feed.cost <- (kHayLbs / 2000 * p.hay + kOthLbs / 2000 * p.oth) * days.feed * herd
  return(feed.cost)
}

CalculateRentPastCost <- function(n.miles, truck.cost, past.rent, oth.cost, days.rent, max.wt, cow.wt, calf.wt, herd) {
"
 Function: CalculatePastureRentCost
 Description: Calculates the costs of renting pasture and trucking pairs
 NOTE: Not including loan interest costs. This needs to be resolved across all options.

 Inputs: 
  n.miles = Distance to rented pasture (miles)
  truck.cost = Trucking cost per loaded mile ($/mile/truck)
  past.rent = Price of renting pasture per animal unit month, where an animal unit is a cow/calf pair ($/pair/month)
  days.rent = Days on rented pasture. Generally, equal to days.act. (days)
  oth.cost = All other non-rental, non-trucking costs ($)
  max.wt = Maximum weight per truck (pounds)
  cow.wt = Average cow weight (pounds)
  calf.wt = Average 'current' weight of calves before trucking to rented pasture (pounds)
  herd = Size of herd (head of cows, does not include calves)

 Outputs:
  cost.rentpast = Total costs of using renting pasture including transport costs ($/year)
"
  # Calculating number of trucks needed to truck pairs to pasture and to truck cows home (without calves)
  n.trucks.past <- ceiling(herd / ceiling(max.wt / (cow.wt + calf.wt)))
  n.trucks.home <- ceiling(herd / ceiling(max.wt / cow.wt))
  
  # Cost per herd of trucking pairs to pasture and trucking cows back home
  tot.truck.cost <- n.miles * truck.cost * n.trucks.past + n.miles * truck.cost * n.trucks.home
  
  # Cost of renting pasture
  tot.past.rent <- past.rent / 30 * days.rent * herd
  
  # Total costs including transport, rent, and other costs (DOES NOT INCLUDE LOAN INTEREST)
  cost.rentpast <- tot.truck.cost + tot.past.rent + oth.cost
  
  return(cost.rentpast)
}

CalculateRentPastRevenue <- function(wn.wt, calf.loss, calf.wt.adj, calf.sell, herd, p.wn.yr1) {
"
 CalculateRentPastRevenue 
 Description: Calculates the change in revenues due to trucking pairs to rented pastures

 Inputs:
  calf.loss = Additional calf deaths due to transport stress (head of calves)
  calf.wt.adj = Adjustment for calf weaning weights (%)
  calf.sell = Average percentage of calves sold (%)
  wn.wt = Average weight at weaning (pounds)
  p.wn.yr1 = Expected sale price of calves in year 1 ($/pound)
  herd = Size of herd (head of cows, does not include calves)

 Outputs:
  rev.rentpast = Change in revenue due to mortality and weight loss from trucking to rented pasture
"
  # Number of calves sold after accounting for calf mortality in transport 
  actual.calf.sales <- herd * calf.sell - calf.loss
  
  # Selling weight after accounting for weight loss due to transport stress
  actual.sell.wt <- wn.wt * (1 + calf.wt.adj)
  
  # Expected calf revenues for non-drought year
  exp.sales <- herd * calf.sell * wn.wt * p.wn.yr1
  
  # Change in expected revenues
  rev.rentpast <- exp.sales - actual.calf.sales * actual.sell.wt * p.wn.yr1
  return(rev.rentpast)
}

CalculateSellPrsCostYr1 <- function(op.cost.yr1, herd, sell.cost) {
"
 Function: CalculateSellPairsCost
 Description: Calculates the change in costs due to selling pairs and replacing cows
 NOTE: It is assumed that cows are replaced on last day of the second year after they are sold. 
  For example, cows sold in 2011 are replaced on 12/31/2013.

 Inputs:
  op.cost.yr1 = Change in operating costs in year 1 per cow ($/cow)
  op.cost.yr2 = Change in operating costs in year 2 per cow ($/cow)
  op.cost.yr3up = Change in operating costs in years 3 and up per cow ($/cow)
  sell.cost = Selling cost per cow ($/cow) NOTE: DO WE COUNT SELLING COSTS IN A NORMAL YEAR? ARE THESE ADDITIONAL?
  replc.cost = Cost of replacing the cow ($/cow)
  herd = Size of herd (head of cows, does not include calves)

 Outputs:
  
"
  # Yr 1 change in operating costs includes change in operating cost from not having the herd and the additional cost to sell cows
  cost.sellprs.yr1 <- op.cost.yr1 * herd + sell.cost * herd  
  return(cost.sellprs.yr1)
}

CalculateSellPrsRevenueYr1 <- function(herd, calf.sell, wn.wt, p.wn.yr1, wn.succ, calf.wt, p.calf.t0, p.cow) 
"
 Function: CalculateSellPairsCost
 Description: Calculates the change in revenues due to selling pairs and replacing cows
 NOTE: It is assumed that cows are replaced on last day of the second year after they are sold. 
  For example, cows sold in 2011 are replaced on 12/31/2013.

 Inputs:
  calf.sell = Average percentage of calves sold (%)
  wn.wt = Average weight at weaning (pounds)
  p.wn.yr1 = Expected sale price of calves in year 1 ($/pound)
  p.wn.t0 = Current sale price calves ($/pound)
  herd = Size of herd (head of cows, does not include calves)
  wn.succ = Average percentage of cows that successfully wean calves (%)
  calf.wt = Average 'current' weight of calves (pounds)
  p.cow = Current sale price of cow ($/cow)
  
 Outputs:

"
  # Expected calf revenues for non-drought year
  exp.sales <- herd * calf.sell * wn.wt * p.wn.yr1
  
  # Actual calf sales for year 1
  calf.sales.yr1 <- herd * wn.succ * calf.wt * p.calf.t0 
  
  # Cow sales in year 1
  cow.sales <- p.cow * herd
  
  # Change in year 1 revenue from expected
  rev.sellprs.yr1 <- exp.sales - calf.sales.yr1 + cow.sales
  return(rev.sellprs.yr1)
}

