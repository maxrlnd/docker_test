# Desciption: This script sets variables values for the drought_decision_model
# NOTE: Currently, the default values are taken from the excel model. Eventually this will be replaced by
#  a workable interface

## Precip and forage potential 
# These variables need to be set first because our 
# cow/calf weights by year depend upon them.

if(!exists(station.gauge,envir=globalenv())){
  stop("Station gauge information is required.")
}

if(!exists(constvars,envir=globalenv())){
  stop("Constant variable information is required.")
}


# if(!exists("target.loc")){ # Use COOP sites or CPER: Default to CPER
#   
#   ## Zone Weights
#   stzone=3 # state forage zone
#   # multiple operations since reading from
#   # external file that may be replaced
#   zonewt=read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx",sheet="Drought Calculator",skip = 5)[5:8,]
#   zonewt=sapply(data.frame(zonewt[,which(names(zonewt)=="Jan"):which(names(zonewt)=="Dec")]),as.numeric)
#   
#   ## Station precip gauge 
#   stgg=data.frame(read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx","CPER Precip",skip = 1))
#   stgg=stgg[,-which(names(stgg) %in% c("TOTAL","Var.15"))]
#   stgg=stgg[stgg$Year %in% c(1948:2016,"AVE"),]
#   
#   ## Target grid cell
#   tgrd = 25002  # target grid cell - CPER default 
#   
# }else{ #Custom location specified (COOP site and MLRA forage potential weights)
#   
#   ## Fetch data
#   wrc.state="co" # For pulling COOP sites & mlra forage weights
#   load("data/coops.RData") # Shortcut for sourcing 'R/coop_scraper.R'
#   # source("R/coop_scraper.R") # the long way
#   mlra=readOGR("data","mlra_v42") # load MLRA zone data
#   target.coop=coops[[which(names(coops)==target.loc)]]
#   
#   ## Zone weights
#   mlra.idx=COOP_in_MRLA(target.coop) # MLRA index
#   zonewt=getMRLAWeights(wrc.state) # zone weights
#   stzone=which(zonewt[,1]==mlra.idx) # not a great workaround...should fix 'foragePwt' function instead
#   zonewt=zonewt[,-1] # not a great workaround...should fix 'foragePwt' function instead
#   
#   ## Station precip gauge
#   stgg=target.coop$precip
#   stgg=rbind(stgg,rep(NA,ncol(stgg)))
#   stgg[nrow(stgg),][,1]="AVE"
#   stgg[nrow(stgg),][,-1]=colMeans(stgg[-nrow(stgg),][,-1],na.rm=T)
#   
#   ## Target grid cell
#   tgrd = target.coop$grid  # target grid cell - custom site
#   
# }  

# Setting input values to defaults in excel file (temporary placeholder)

# if specified, use a random starting year
# if(exists("random.starts")){
if("random.starts"){
  styr=round(runif(1,1948,2010))
}else{
  styr=2002 # starting year in five-year period 
}

## Static vs dynamic vars, based on forage
if(use.forage){
  wn.wt <- calfWeanWeight(styr) # dynamic by year based on precip/forage
}else{
  wn.wt <- c(calfWeanWeight(styr)[1],rep(expected.wn.wt,4)) # year 1 only based on precip/forage
}

# Drought action var's
drought.action = ifelse(1:5 %in% act.st.yr:act.end.yr, 1, 0)
calf.loss <- ifelse(drought.action==1,2,0) 
calf.wt.adj <- ifelse(drought.action==1,-0.1,0)

# Wean weights
# make this constant??
# previously 'p.wn.yr1', now vectorized for iteration
p.wn<-c(1.31,1.25,1.25,1.25,1.25) 

# act.st.yr <- 1
# act.st.m <- 6
# act.end.yr <- 1
# act.end.m <- 12
# kHayLbs <- 22
# kOthLbs <- 0
# p.hay <- 100  # This should be a user input variable
# p.oth <- 0  # This should be a user input variable
# herd <- 600  # User input variable
# n.miles <- 300
# truck.cost <- 4.00
# past.rent <- 16.49
# oth.cost <- 300
# max.wt <- 40000
# cow.wt <- 1200
# calf.wt <- 375
# expected.wn.wt <- 600 # expected calf weight at weaning
# calf.loss <- ifelse(drought.action==1,2,0) 
# calf.wt.adj <- ifelse(drought.action==1,-0.1,0)
# calf.sell <- 0.75
# p.wn <- c(1.31,1.25,1.25,1.25,1.25) # previously 'p.wn.yr1', now vectorized for iteration
# wn.succ <- 0.94
# p.calf.t0 <- 1.45
# p.cow <- 850
# cow.cost = 500
# purchase.insurance = 1 # dummy - purchase insurance (1) or do not (0)
# invst.int = 0.0125
# cap.tax.rate <- 0.15
# t <- 5  # number of years in the model
# cull.num <- 15  # Number of cows culled in a normal year
# 
# ## Other Information
# loan.int <- 0.065  # interest rate for borrowed money (%/year)
# 
# ## Option 3 Variables: Sell pairs and replace cows
# op.cost.adj <- -100  # Change in operating costs in year 1 per cow ($/cow/year). Negative value represents reduced costs
# herdless.op.cost <- 5000  # Operating costs incurred without a herd ($/year)
# sell.cost <- 20  # Selling cost per cow ($/cow) NOTE: DO WE COUNT SELLING COSTS IN A NORMAL YEAR? ARE THESE ADDITIONAL?
# replc.cost <- 850  # Cost of replacing the cow ($/cow)

## set target insurance years
# yyr=2002:2006 # all five years
yyr=styr:(4+styr) # all five years
# yyr=c(2002:2003,2005) # we can also set this for individual years
# yyr=2002 # or just one year - the "one year, one drought" model

## set insurance variables
clv=0.9 # insurance coverage level (0.7 - 0.9 in increments of 0.05)
acres=3000 # ranch acres
pfactor=1 # productivity factor (0.6 - 1.5)

# Insurance purchases
# Use Excel model choices by default,
# otherwise automatically allocate based
# upon forage potential
# if(!exists("autoSelect.insurance")){
if("autoSelect.insurance"){
  insp=rbind(c(3,0.5),c(5,0.5)) # insurance purchase
}else{
  insp=insAlloc(fpwt=zonewt[stzone,],niv=2) # automatic selection
}
  
# SpatialPoints representation of target gridcell 
# for fetching insurance results
tgrd_pt = rastPt[rastPt@data$layer == tgrd, ]  

## Precip, Forage Potential, and Calf Weight variables
styear=yyr[1] # Starting "drought" year
# dr_start=6 # Drought adaptive action starts
dr_start=act.st.m # Drought adaptive action starts
# dr_end=8 # Drought action ends 
dr_end=act.end.m # Drought action ends 
# calf_currently=375 # Average calf weight "currently"
# calf_wean=600 # Expected average calf weight "at weaning"

>>>>>>> eea54a17bcecd50378f1ff4a05c1ef965e9a563e
