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

# Clear environment
# prevent from erasing custom location/insurance selection if set
rm(list = ls()[!ls() %in% c("target.loc", "autoSelect.insurance", 
                            "random.starts", "masterRunner", "runs")])

# Source functions
source("R/load.R")
source("R/costs_revenues_functions.R")
source("R/vars_functions.R")
source("R/insurance_functions.R")
source("R/assets_outcomes_functions.R")
source("R/support_functions.R")
source("R/sim_run.R")

# Populate a new environment with 
# station gauge info 
# Default location is CPER site
getStationGauge()

# Populate a new environment with 
# constant (user) variables
getConstantVars()

# Assemble station gauge/constant variables environments
# into a "baseline varaibles" list 
# base_vars=append(as.list(station.gauge),as.list(constvars))

#### SIMPLE EXAMPLE ####
# Get simulated vars
# getSimVars() # default settings
getSimVars(random.starts = TRUE, use.forage = TRUE) # with simulated vars

run_vars = append(append(as.list(station.gauge), as.list(constvars)), as.list(simvars))

out <- sim_run(run_vars)
stopifnot(is.data.frame(out))

#### Perform all runs ####
generateRunParams <- function(){
  getSimVars(random.starts = TRUE, use.forage = TRUE) # with simulated vars
  return(append(append(as.list(station.gauge), as.list(constvars)), as.list(simvars)))
}

simruns <- rlply(10, generateRunParams())  # list of simulation variables for runs

sim_outcomes <- lapply(simruns, sim_run)
sim_outcomes <- do.call("rbind", sim_outcomes)


# Parallelize simulation runs
sfInit(parallel = TRUE, cpus = 4)
sfExportAll(debug = TRUE)

# load packages on workers
sfLibrary(plyr)
sfLibrary(dplyr)
sfLibrary(raster)
sfLibrary(data.table)
sfLibrary(readxl)
sfLibrary(rgdal)

parouts <- sfLapply(simruns, sim_run)
parouts <- do.call("rbind", parouts)
sfStop()

#### TEST VISUALIZATION ####

save(sim_outcomes,"misc/demo_sim_100.RData")
load(sim_outcomes,"misc/demo_sim_100.RData") # reload original run

# tidy df
sim_out_sub=sim_outcomes[sim_outcomes$yr==5,][,c("opt","ins","aftax.inc")]

ggplot(data=sim_out_sub,aes(x=aftax.inc))+
  geom_histogram()+
  facet_grid(ins~opt,scales="free")

# vectors of outcomes by opt/ins
sim_out_v=split(sim_out_sub$aftax.inc,
                paste(sim_out_sub$opt,ifelse(sim_out_sub$ins==1,"Insured","Uninsured"),sep=" - "))
hist(sim_out_v$`noadpt - Uninsured`)
