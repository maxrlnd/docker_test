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
 detach(constvars)
 detach(station.gauge)

# Source functions
source("R/load.R")
source("R/dynamicFunctions.R")
source("R/weaning_success.R")

#### Setup ####

# Populate a new environment with station gauge info.
# Default location is CPER site
## Currently this creates new variables through side effects (<<-) which
 # are confusing and bad coding practice, I've changed this to make the code more readable
 # also these were created using environments which is better done with list I've updated that
station.gauge <- getStationGauge()

# Populate a new environment with constant (user) variables
## Did the same thing as with getStationGauge above
constvars <- getConstantVars()


#### Generate Model Inputs ####
## I don't like this...a funciton in the run file is inconsistent and a bit klunky 
## especially if we're trying to change any of the parpameters (random.starts etc)
## what if we move this get rid of this and have simVars return the appended  list
## of station.gague, constvars, and simvars..it already has them all so why use two
## functions.
generateRunParams <- function(acres.param = 3000){
  simvars <- getSimVars(
             station.gauge,         
             constvars,
             start_year = 2002, 
             sim_length = 5,
             use.forage = T,
             random.acres=FALSE, 
             random.productivity=TRUE,
             acres = acres.param) # with simulated vars
  return(append(append(station.gauge, constvars), (simvars)))
}



#### Non-parallel model run ####
runs <- 1
simruns <- rlply(runs, generateRunParams(acres.param = 3000))  # list of simulation variables for runs
list.index <- seq_along(simruns)  # creating an index of the list number to store in the sim_outcomes and match back with the simruns variables
for (i in 1:runs) {
  simruns[[i]]$sim.index <- list.index[i] 
}
outs <- lapply(simruns, sim_run_single)
outs <- rbindlist(outs)


