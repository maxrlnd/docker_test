# Desciption: This script sets variables values for the drought_decision_model
# NOTE: Currently, the default values are taken from the excel model. Eventually this will be replaced by
#  a workable interface

# Setting input values to defaults in excel file (temporary placeholder)
act.st.yr <- 1
act.st.m <- 6
act.end.yr <- 1
act.end.m <- 12
kHayLbs <- 22
kOthLbs <- 0
p.hay <- 100  # This should be a user input variable
p.oth <- 0  # This should be a user input variable
herd <- 600 # User input variable
n.miles <- 300
truck.cost <- 4.00
past.rent <- 16.49
oth.cost <- 300
max.wt <- 40000
cow.wt <- 1200
calf.wt <- 375
wn.wt <- 600
calf.loss <- 2 
calf.wt.adj <- -0.1 
calf.sell <- 0.75
p.wn.yr1 <- 1.31
wn.succ <- 0.94
p.calf.t0 <- 1.45
p.cow <- 850

## Target grid cell
tgrd=25002 # target grid cell 
tgrd_pt=rastPt[rastPt@data$layer==tgrd,] # SpatialPoints representation of target gridcell

## set target insurance years
# yyr=2002:2006 # all five years
# yyr=c(2002:2003,2005) # we can also set this for individual years
yyr=2002 # or just one year - the "one year, one drought" model

## set insurance variables
clv=0.9 # insurance coverage level (0.7 - 0.9 in increments of 0.05)
acres=3000 # ranch acres
pfactor=1 # productivity factor (0.6 - 1.5)
insp=rbind(c(3,0.5),c(5,0.5)) # insurance purchase

## Precip, Forage Potential, and Calf Weight variables
styear=yyr[1] # Starting "drought" year
dr_start=6 # Drought adaptive action starts
dr_end=8 # Drought action ends 
calf_currently=375 # Average calf weight "currently"
calf_wean=600 # Expected average calf weight "at weaning"
stzone=3 # state forage zone

