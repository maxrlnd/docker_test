#### Setup ####
library(shiny)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(shinyjs)

source("R/load.R")
source("R/dynamicFunctions.R")
source("R/shinny_support.R")


## Code to disable tab
jscode <- '
shinyjs.init = function() {
$(".nav").on("click", ".disabled", function (e) {
e.preventDefault();
return false;
});
}
'
## Css for disabled tabs
css <- '
.disabled {
background: white !important;
cursor: default !important;
color: white !important;
}
'
# Populate a new environment with station gauge info.
# Default location is CPER site
station.gauge <- getStationGauge()

# Populate a new environment with constant (user) variables
constvars <- getConstantVars()

## Acres of range
acres <- 3000

## Carrying capacity based on acres and carrying.cap constant
carryingCapacity <- constvars$carrying.cap * acres

## create state variables for practice runs
practiceVars <- getSimVars(
  station.gauge,
  constvars,
  start_year = 2002,
  sim_length = 3,
  use.forage = T,
  random.acres=FALSE,
  random.productivity=TRUE,
  acres)

## create state variables for full runs
simvars <- getSimVars(
  station.gauge,
  constvars,
  start_year = 2002,
  sim_length = 10,
  use.forage = T,
  random.acres=FALSE,
  random.productivity=TRUE,
  acres)

## Create list of constant vars, state vars, and station gauges
practiceRuns <- (append(append(station.gauge, constvars), (practiceVars)))
simRuns <- (append(append(station.gauge, constvars), (simvars)))

## Create results frames for practice and simulation
practiceOuts <- createResultsFrame(practiceRuns)
myOuts <- createResultsFrame(simRuns)

## Set starting year, and simulation length
currentYear <- 1
practiceYear <- 1
startYear <- 2002
simLength <- 5

## Calcualte indemnities for all years of the simulation
indem <- lapply(startYear:(startYear + simLength - 1), function(x){
  with(simRuns, insMat(yy = x, clv = clv, acres = acres,
                                 pfactor = pfactor, insPurchase  =  insp, tgrd = tgrd))
})

## Is insurance purchased?
# purchaseInsurance <- sample(c(T, F), 1)
purchaseInsurance <- T

## Create JS to switch between year tabs
yearHandler <- paste0('if(typeMessage == ', 1:simLength, '){
  console.log("got here");
  $("a:contains(Year ', 1:simLength, ')").click();
}', collapse = "")