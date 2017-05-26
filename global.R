#### Setup ####
library(shiny)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(shinyjs)
library(shinyBS)
source("R/load.R")
source("R/dynamicFunctions.R")
source("R/shinny_support.R")
source("R/simUI.R")


## Code to disable tab
jscode <- '
shinyjs.init = function() {
$(".nav").on("click", ".disabled", function (e) {
e.preventDefault();
return false;
});
}
'
## js code to reset
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

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

## Set Starting Years
startYear <- 1999
startYearprac <- 1951

## create state variables for practice runs
practiceVars <- getSimVars(
  station.gauge,
  constvars,
  start_year = startYearprac,
  sim_length = 5,
  use.forage = T,
  random.acres=FALSE,
  random.productivity=FALSE,
  acres)

## create state variables for full runs
simvars <- getSimVars(
  station.gauge,
  constvars,
  start_year = startYear,
  sim_length = 10,
  use.forage = T,
  random.acres=FALSE,
  random.productivity=FALSE,
  acres)

## Create list of constant vars, state vars, and station gauges
practiceRuns <- (append(append(station.gauge, constvars), (practiceVars)))
practiceRuns$p.wn <- rep(1.30, length(practiceRuns$p.wn))
simRuns <- (append(append(station.gauge, constvars), (simvars)))
simRuns$p.wn <- rep(1.30, length(simRuns$p.wn))

## Set simulation length
simLength <- 10
practiceLength <- 5



## Calcualte indemnities for all years of the simulation
indem <- lapply(startYear:(startYear + simLength - 1), function(x){
  with(simRuns, shinyInsMat(yy = x, clv = clv, acres = acres,
                            pfactor = pfactor, insPurchase  =  insp, tgrd = tgrd))
})

indemprac <- lapply(startYearprac:(startYearprac + practiceLength - 1), function(x){
  with(practiceRuns, shinyInsMat(yy = x, clv = clv, acres = acres,
                                 pfactor = pfactor, insPurchase  =  insp, tgrd = tgrd))
})

## Calculate binary variable for hypothetical payout based on the weather
## If 
indemnity <- lapply(indem, "[[", 3) # Pulling the value of the indemnity from the (list of) dataframes
whatifIndem <- sapply(indemnity > 0, ifelse, 1, 0)  # Creating a binary variable where a year is eligible for a payout if you have insurance

indemnityprac <- lapply(indemprac, "[[", 3) # Pulling the value of the indemnity from the (list of) dataframes
whatifIndemprac <- sapply(indemnityprac > 0, ifelse, 1, 0)


## Create results frames for practice and simulation
createOutputs(practiceRuns, simRuns, indem, indemprac)

## Is insurance purchased?
# purchaseInsurance <- sample(c(T, F), 1)
purchaseInsurance <- T
# 
# if(!purchaseInsurance){
#   indem <- lapply(indem, function(x){
#     x[, c("producer_prem", "indemnity", "full_prem") := 0]
#     return(x)
#   })
# }

## Counter to keep track of quiz
quizCounter <- 0

## Create JS to switch between year tabs
yearHandler <- paste0('if(typeMessage == ', 1:simLength, '){
  console.log("got here");
  $("a:contains(Year ', 1:simLength, ')").click();
}', collapse = "")

NUM_PAGES <- 5
currentPage <- 1

debugMode <<- T

`%then%` <- shiny:::`%OR%`
genericWrong <- "This is incorrect please try again"
