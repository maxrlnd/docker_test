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