# Calf Weight Functions ---------------------------------------------------

AdjWeanSuccess <- function(forage.potential, noadpt = FALSE, normal.wn.succ, t) {
  # Description: Adusts weaning success downward for the year of the drought and the following year
  # NOTE: This equation is based on what I consider to be "reasonable" estimates
  #  of weaning success based on forage potential. We need to find a source
  #  that gives a better idea of the relationship
  
  wn.succ <- NULL
  
  if(noadpt == FALSE | forage.potential >= 1) {
    wn.succ <- rep(normal.wn.succ, t)
  }
  if(noadpt == TRUE & forage.potential < 1) {
    if(t > 1){
      wn.succ[1] <- normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential)*2))) 
      wn.succ[2] <- normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential))))
      wn.succ[3:t] <- normal.wn.succ                                
    }else{
      wn.succ <- normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential)*2))) 
    }
  }
  return(wn.succ)
}  

calfDroughtWeight<-function(normal.wn.wt, forage.potential){
  "
  Description: If forage potential is less than 1, then the calf weight is less
  #****Is this method of reducing weight back up by the literature?
  "
  if(forage.potential < 1) {
    wn.wt <- normal.wn.wt * (1 - (1 - forage.potential)/3)
  }
  else{
    wn.wt <- normal.wn.wt
  }
  wn.wt 
}

calfWeanWeight <- function(styr, sim_length){
  
  "
  Compute calf weights based on station/grid cell
  forage potential for a n-year period.
  
  Inputs:
  
  styr: starting year of the n-year period.
  
  Wean weights are computed for each of the five years as a summed product of
  the target location's forage potential weights and precipitation index by
  interval. These are returned as a matrix of calf weights by year,
  `calf_weights_ann`.
  "
  
  if(!exists("station.gauge", envir = globalenv())){
    stop("Station gauge information is required.")
  }
  
  if(!exists("constvars", envir = globalenv())){
    stop("Constant variable information is required.")
  }
  
  ## I honestly don't think these ever need to be attached here (station.gauge might need to be)
  ## since they're attached in the functions that are calling them and this funciton inherits their environments
  ## but I kept these just incase
  constAtt <- F
  stationAtt <- F
  if(!any("constvars" %in% search())){
    attach(constvars)
    constAtt <- T
  }
  if(!any("station.gauge" %in% search())){
    attach(station.gauge)
    stationAtt <- T
  }
  
  ## Calculate for potential for all years of the simulation
  ## In a dynamic model we may want to decrement these based on previous decisions
  ## but that might be best done elsewhere in the code
  forage.weights = unlist(lapply(seq(styr, styr + (sim_length - 1)),function(i){
    foragePWt(station.gauge, i, herd, carryingCap)
  }))
  
  ## Calculate wean weight for each year of the simulation
  ## Right now this is capped at normal.wn.wt is this correct?
  calf_weights_ann = unlist(lapply(forage.weights, function(i){ # annual calf weights
    calfDroughtWeight(normal.wn.wt, i)
  }))
  if(constAtt)detach(constvars)
  if(stationAtt)detach(station.gauge)
  
  return(calf_weights_ann)
}

getHerdSize <- function(results_1ya, results_2ya, deathRate){
  currentHerd <- (results_1ya$herd * (1 - deathRate) * 
                    (1 - results_1ya$cows.culled) + 
                    (results_2ya$herd * results_2ya$wn.succ) *
                    (1 - results_2ya$calves.sold))
  return(currentHerd)
}
