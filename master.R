# Copyright (c) 2016 Trisha Shrum, Joseph Tuccillo
## Authors Comment: This model is jointly developed at the University of
#  Colorado Earth Lab based on work by Adam McCurdy, Joseph Tuccillo, Kelly Carney,
#  Bill Travis, Jeffrey Tranel, Rod Sharp, and John Deering.
#
# Description: This script implements a simulation of drought adaptation
#  decisions by Western cattle ranchers.
#
# Inputs:
#   ...
#
# Outputs:
#   ...

# # Clear environment
# # prevent from erasing custom location/insurance selection if set
 rm(list = ls()[!ls() %in% c("target.loc", "autoSelect.insurance",
                             "random.starts", "masterRunner", "runs")])

# Source functions
source("R/load.R")
source("R/support_functions.R")
source("R/weaning_success.R")

#### Setup ####

# Populate a new environment with station gauge info.
# Default location is CPER site
getStationGauge()

# Populate a new environment with constant (user) variables
getConstantVars()


#### Generate Model Inputs ####
generateRunParams <- function(acres.param = 3000){
  getSimVars(random.starts = TRUE, 
             use.forage = FALSE,
             random.acres=FALSE, 
             random.productivity=TRUE,
             acres = acres.param) # with simulated vars
  return(append(append(as.list(station.gauge), as.list(constvars)), as.list(simvars)))
}



#### Non-parallel model run ####
runs <- 100
simruns <- rlply(runs, generateRunParams(acres.param = 3000))  # list of simulation variables for runs
list.index <- seq_along(simruns)  # creating an index of the list number to store in the sim_outcomes and match back with the simruns variables
for (i in 1:runs) {
  simruns[[i]]$sim.index <- list.index[i] 
}
outs <- lapply(simruns, sim_run)
outs <- do.call("rbind", outs)


