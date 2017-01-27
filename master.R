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
runs <- 1
simruns <- rlply(runs, generateRunParams(acres.param = 3000))  # list of simulation variables for runs
list.index <- seq_along(simruns)  # creating an index of the list number to store in the sim_outcomes and match back with the simruns variables
for (i in 1:runs) {
  simruns[[i]]$sim.index <- list.index[i] 
}
outs <- lapply(simruns, sim_run)
outs <- do.call("rbind", outs)



#### Parallelized model run #### 
runs <- 100
simruns <- rlply(runs, generateRunParams(acres.param = 3000))  # list of simulation variables for runs
list.index <- seq_along(simruns)  # creating an index of the list number to store in the sim_outcomes and match back with the simruns variables
for (i in 1:runs) {
  simruns[[i]]$sim.index <- list.index[i] 
}

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

save(parouts, file = "output/simulation_results_baseline10.RData")
save(simruns, file = "output/simulation_inputs_baseline10.RData")



#### Summary ####
# quick summary of output for final year networth by option and insurance
finalyr <- parouts[parouts$yr == 5,]
networth <- summarize(group_by(finalyr, opt, ins), avg.netwrth = mean(net.wrth))

# Expected utility, quick pass
exp_utility <- select(parouts, opt, yr, ins, aftax.inc, net.wrth, sim.index) %>%
  filter(yr > 0)
exp_utility$inc.min1 <- ifelse(exp_utility$aftax.inc <= 0, 1, exp_utility$aftax.inc) 
exp_utility$inc.util <- log(exp_utility$inc.min1)
exp_utility$wealth.util <- log(exp_utility$net.wrth)
eu <- group_by(exp_utility, ins, opt, sim.index) %>%
  summarize(exp.util.inc = mean(inc.util), exp.util.wealth = mean(wealth.util))

d <- group_by(eu, ins) %>%
  density(exp.util.wealth)

eu.noins <- filter(eu, ins==0, opt == "noadpt")
eu.ins <- filter(eu, ins==1, opt == "noadpt")
summarize(eu.noins, m = mean(exp.util.wealth))
summarize(eu.ins, m = mean(exp.util.wealth))
summarize(eu.noins, m = mean(exp.util.inc))
summarize(eu.ins, m = mean(exp.util.inc))

p <- eu.noins$exp.util.inc
d <- density(p)
plot(d)

parouts %>% filter(opt == "noadpt", yr == 5) %>% select(opt, ins, net.wrth) ->  noadpt.results
write.csv(noadpt.results, file="output/noadapt_montecarlo.csv")

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
