"
Author: Joe

This script serves as a scratchpad for porting the various components
of the Ranch Drought model to R code. Each section/subsection should 
be converted to a wrapper function(s) in 'support_functions.R'. 
"

source("R/support_functions.R") # load libraries, data, and functions

####Range Insurance####

tgrd=25002 # target grid cell 
tgrd_pt=rastPt[rastPt@data$layer==tgrd,] # SpatialPoints representation of target gridcell
insp=rbind(c(3,0.5),c(5,0.5)) # insurance purchase

## set target insurance years
# yyr=2002:2006 # all five years
# yyr=c(2002:2003,2005) # we can also set this for individual years
yyr=2002 # or just one year - the "one year, one drought" model

## set insurance variables
clv=0.9 # insurance coverage level (0.7 - 0.9 in increments of 0.05)
acres=3000 # ranch acres
pfactor=1 # productivity factor (0.6 - 1.5)

## Generate insurance info
fiveYears=matrix(0,5,3) #empty matrix - year, indemnity, producer premium x number years 
fiveYears[,1]=seq(yyr[1],yyr[1]+4) #populate years
#**PARALLELIZE THIS??**#
for(yy in yyr){
  
  fiveYears[which(fiveYears[,1]==yy),]=c(yy,
                                         unlist(
                                           dcInfo(dc=droughtCalculator(
                                                               yy=yy,
                                                               clv=clv,
                                                               acres=acres,
                                                               pfactor=pfactor,
                                                               insPurchase=insp
                                                               ),
                                                  tgrd=tgrd)[c("prodPrem","indemtot")]
                                           )
                                         )
}

###Precip, Forage Potential, and Calf Weight####

##define inputs
styear=yyr[1] # Starting "drought" year
dr_start=6 # Drought adaptive action starts
dr_end=8 # Drought action ends 
calf_currently=375 # Average calf weight "currently"
calf_wean=600 # Expected average calf weight "at weaning"
stzone=3 # state forage zone

##Read in Station precip gauge 
#in the excel model, this is the CPER gauge.
stgg=data.frame(read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx","CPER Precip",skip = 1))
stgg=stgg[,-which(names(stgg) %in% c("TOTAL","Var.15"))]
stgg=stgg[stgg$Year %in% c(1948:2016,"AVE"),]

##Build precip weights
#Zone weights from CO Drought Calculator 
zonewt=read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx",sheet="Drought Calculator",skip = 5)[5:8,]
zonewt=data.frame(zonewt[,which(names(zonewt)=="Jan"):which(names(zonewt)=="Dec")])
zonewt=sapply(zonewt,as.numeric)
zonewt=zonewt[stzone,] # subset weights by zone
yprecip=stgg[stgg$Year==styear,][,-1] # monthly precip amounts for start year
ave=stgg[stgg$Year=="AVE",][,-1] # average precip since 1948
pidx=yprecip/ave # Monthly precip "index"

## Forage potential weights
"
Generate a typology of years 
1948-2015 (k-medoids) based on 
monthly precip values observed at the 
station. 

For the forage potential weights:

Use the product of deviation in precip from
long-term (1948-2015) average * zone weights
for months occurring before & during the 
decision month. 

For months occurring after 
the decision month, use the product of group 
average deviation from the long-term 
average * zone weight.

This approach roughly approximates a 'best guess'
scenario based on rain gauge observations -
what should my precip for the remainder of the year
look like given what I know by the decision month? 
"

cper=stgg[1:which(stgg$Year==2015),] # subset by period 1948-2015
cper_clust=pamk(cper[,-1]) # Find optimal groups of years, k = 2-10
yy_group=cper_clust[[1]]$clustering[which(cper$Year==styear)] # Group membership for target year
yy_ave=colMeans(cper[which(cper_clust[[1]]$clustering==yy_group),][,-1]) # Group mean vector
yidx=yy_ave/ave # Expected index values for year (group mean vector / long-term average)

# Generate forage potential weights 
foragewt=unlist(c((zonewt*pidx)[1:dr_start],(zonewt*yidx)[(dr_start+1):12]))

# Compute annual forage potential weight for zone
stfwt=sum(foragewt) # not sure why the rows are subsetting as lists!

## calf weights 
"
Calf drought weight is computed as the
difference between expected and current
calf weights scaled by annual forage
potential weight.
"
calf_drwt=calf_currently+(stfwt*(calf_wean-calf_currently))

