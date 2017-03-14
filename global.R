#### Setup ####

# Populate a new environment with station gauge info.
# Default location is CPER site
station.gauge <- getStationGauge()

# Populate a new environment with constant (user) variables
constvars <- getConstantVars()

practiceVars <- getSimVars(
  station.gauge,
  constvars,
  start_year = 2002,
  sim_length = 3,
  use.forage = T,
  random.acres=FALSE,
  random.productivity=TRUE,
  acres = 3000)

simvars <- getSimVars(
  station.gauge,
  constvars,
  start_year = 2002,
  sim_length = 10,
  use.forage = T,
  random.acres=FALSE,
  random.productivity=TRUE,
  acres = 3000)

practiceRuns <- (append(append(station.gauge, constvars), (practiceVars)))
simRuns <- (append(append(station.gauge, constvars), (simvars)))

practiceOuts <- createResultsFrame(practiceRuns)
myOuts <- createResultsFrame(simRuns)

currentYear <- 1

practiceYear <- 1

startYear <- 2002

acres <- 3000

carryingCapacity <- constvars$carrying.cap * acres