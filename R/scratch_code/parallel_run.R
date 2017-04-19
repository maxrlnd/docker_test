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
