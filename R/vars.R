# Desciption: This script sets variables values for the drought_decision_model
# NOTE: Currently, the default values are taken from the excel model. Eventually this will be replaced by
#  a workable interface

## Zone Weights 

# # CPER
# stzone=3 # state forage zone
# # multiple operations since reading from
# # external file that may be replaced
# zonewt=read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx",sheet="Drought Calculator",skip = 5)[5:8,]
# zonewt=sapply(data.frame(zonewt[,which(names(zonewt)=="Jan"):which(names(zonewt)=="Dec")]),as.numeric)

# Custom location (COOP site and MLRA forage potential weights)
wrc.state="co" # For pulling COOP sites & mlra forage weights
load("data/coops.RData") # Shortcut for sourcing 'R/coop_scraper.R'
# source("R/coop_scraper.R") # the long way
mlra=readOGR("data","mlra_v42") # load MLRA zone data
mlra.idx=COOP_in_MRLA(coops[[which(names(coops)=="BOULDER, COLORADO")]]) # MLRA index
zonewt=getMRLAWeights(wrc.state) # zone weights
stzone=which(zonewt[,1]==mlra.idx) # not a great workaround...should fix 'foragePwt' function instead
zonewt=zonewt[,-1] # not a great workaround...should fix 'foragePwt' function instead

## Station precip gauge

# # CPER Default, from excel model
# # multiple operations since reading from
# # external file that may be replaced
# stgg=data.frame(read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx","CPER Precip",skip = 1))
# stgg=stgg[,-which(names(stgg) %in% c("TOTAL","Var.15"))]
# stgg=stgg[stgg$Year %in% c(1948:2016,"AVE"),]

# Custom location 
stgg=coops[[which(names(coops)=="BOULDER, COLORADO")]]$precip
stgg=rbind(stgg,rep(NA,ncol(stgg)))
stgg[nrow(stgg),][,1]="AVE"
stgg[nrow(stgg),][,-1]=colMeans(stgg[-nrow(stgg),][,-1],na.rm=T)

# Setting input values to defaults in excel file (temporary placeholder)
styr=2002 # starting year in five-year period 
act.st.yr <- 1
act.st.m <- 6
act.end.yr <- 1
act.end.m <- 12
drought.action = ifelse(1:5 %in% act.st.yr:act.end.yr, 1, 0)
kHayLbs <- 22
kOthLbs <- 0
p.hay <- 100  # This should be a user input variable
p.oth <- 0  # This should be a user input variable
herd <- 600  # User input variable
n.miles <- 300
truck.cost <- 4.00
past.rent <- 16.49
oth.cost <- 300
max.wt <- 40000
cow.wt <- 1200
calf.wt <- 375
expected.wn.wt <- 600 # expected calf weight at weaning
# wn.wt <- calfWeanWeight(styr) # dynamic by year based on precip/forage
wn.wt <- c(calfWeanWeight(styr)[1],rep(expected.wn.wt,4)) # year 1 only based on precip/forage
calf.loss <- ifelse(drought.action==1,2,0) 
calf.wt.adj <- ifelse(drought.action==1,-0.1,0)
calf.sell <- 0.75
p.wn <- c(1.31,1.25,1.25,1.25,1.25) # previously 'p.wn.yr1', now vectorized for iteration
wn.succ <- 0.94
p.calf.t0 <- 1.45
p.cow <- 850
cow.cost = 500
purchase.insurance = 1 # dummy - purchase insurance (1) or do not (0)
invst.int = 0.0125
cap.tax.rate <- 0.15
t <- 5  # number of years in the model

## Other Information
loan.int <- 0.065  # interest rate for borrowed money (%/year)

## Option 3 Variables: Sell pairs and replace cows
op.cost.adj <- -100  # Change in operating costs in year 1 per cow ($/cow/year). Negative value represents reduced costs
herdless.op.cost <- 5000  # Operating costs incurred without a herd ($/year)
sell.cost <- 20  # Selling cost per cow ($/cow) NOTE: DO WE COUNT SELLING COSTS IN A NORMAL YEAR? ARE THESE ADDITIONAL?
replc.cost <- 850  # Cost of replacing the cow ($/cow)

## Target grid cell
tgrd = 25002  # target grid cell 
tgrd_pt = rastPt[rastPt@data$layer == tgrd, ]  # SpatialPoints representation of target gridcell

## set target insurance years
yyr=2002:2006 # all five years
# yyr=c(2002:2003,2005) # we can also set this for individual years
# yyr=2002 # or just one year - the "one year, one drought" model

## set insurance variables
clv=0.9 # insurance coverage level (0.7 - 0.9 in increments of 0.05)
acres=3000 # ranch acres
pfactor=1 # productivity factor (0.6 - 1.5)
insp=rbind(c(3,0.5),c(5,0.5)) # insurance purchase

## Precip, Forage Potential, and Calf Weight variables
styear=yyr[1] # Starting "drought" year
# dr_start=6 # Drought adaptive action starts
dr_start=act.st.m # Drought adaptive action starts
# dr_end=8 # Drought action ends 
dr_end=act.end.m # Drought action ends 
# calf_currently=375 # Average calf weight "currently"
calf_wean=600 # Expected average calf weight "at weaning"

