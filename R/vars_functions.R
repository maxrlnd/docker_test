# Functions that create variables for simulations

getConstantVars<-function(){
  
  "
  Reads in constant variables into a
  `constvars` environment using the 
  file `data/constant_vars.csv`.
  "
  
  # Remove the constvars environment if it exists
  if(exists("constvars")){
    rm("constvars",envir=globalenv())
  }
  
  constvars<<-new.env()
  
  cvars=read.csv("data/constant_vars.csv",stringsAsFactors = F)
  for(i in 1:nrow(cvars)){
    assign(cvars[i,]$Variable,cvars[i,]$Value,envir=constvars)
  }
  
}

getSimVars = function(random.starts = FALSE, use.forage = FALSE,
                      autoSelect.insurance = FALSE, clv = 0.9, acres = 3000,
                      pfactor = 1,
                      random.coverage = FALSE,
                      random.acres = FALSE,
                      random.productivity = FALSE,
                      drought.adaptation.cost.factor = 1){
  
  "
  Note that this will default to the excel 
  model var's

  Inputs:
  random.starts = TRUE: Choose random year to start simulations, then years following are temporally sequencial
  random.starts = FALSE: 2002 is the start year
  use.forage = TRUE: Calf weights without adaptation follow forage potential predictions
  autoSelect.insurance = TRUE: Choose optimal insurance coverage periods based on forage potential
  autoSelect.insurance = FALSE: Insure 50% for 3rd interval and 50% for 5th interval
  clv: Coverage level of insurance
  acres: Size of ranching operation. Modeled to only impact the insurance premium/payout.
  pfactor: Productivity factor relative to other farms in the grid. Relevant only to insurance premium/payout.
  drought.adaptation.cost.factor: Adjusts the impact of the low forage potential on costs of adaptation.
  
  "
  options(warn = -1) # doesn't seem to get rid of warnings...
  
  ## Ensure the baseline environments exist
  if(!exists("station.gauge", envir = globalenv())){
    stop("Station gauge information is required.")
  }
  
  if(!exists("constvars", envir = globalenv())){
    stop("Constant variable information is required.")
  }
  
  # Remove the `simvars` environment if one currently exists
  if(exists("simvars", envir = globalenv())){
    rm("simvars", envir = globalenv())
  }
  
  # Create a fresh simulation vars environment
  simvars <<- new.env()
  
  ## Range of years
  # if specified, use a random starting year
  if(random.starts){
    assign("styr", round(runif(1, 1948, 2010)), envir = simvars)
  }else{
    assign("styr", 2002, envir = simvars) # starting year in five-year period 
  }
  
  ## Static vs dynamic vars, based on forage
  attach(constvars) # use direct reference to `constvars` environment
  if(use.forage){
    assign("wn.wt", calfWeanWeight(get("styr", simvars)), envir = simvars) # dynamic by year based on precip/forage
  }else{
    assign("wn.wt", c(calfWeanWeight(get("styr", simvars))[1], rep(expected.wn.wt, 4)), envir = simvars) # year 1 only based on precip/forage
  }

  # Drought action var's
  assign("drought.action", ifelse(1:5 %in% act.st.yr:act.end.yr, 1, 0), envir = simvars)
  assign("calf.loss", ifelse(get("drought.action", simvars) == 1, 2, 0), envir = simvars)
  assign("calf.wt.adj", ifelse(get("drought.action", simvars) == 1, -0.1, 0), envir = simvars)
  detach(constvars)
  assign("drought.adaptation.cost.factor", drought.adaptation.cost.factor, envir = simvars)
  
  ## Wean weights
  # make this constant??
  # previously 'p.wn.yr1', now vectorized for iteration
  # (I only have this here because vectors can't be represented well in `constant_vars.csv`)
  assign("p.wn", c(1.31, 1.25, 1.25, 1.25, 1.25), envir = simvars)
  
  
  ## set target insurance years
  attach(simvars)
  assign("yyr", styr:(4 + styr), envir = simvars) # all five years
  detach(simvars)
  
  ## Set Insurance variables
  assign("clv", clv, envir = simvars) # insurance coverage level (0.7 - 0.9 in increments of 0.05)
  assign("acres", acres, envir = simvars) # ranch acres
  assign("pfactor", pfactor, envir = simvars) # productivity factor (0.6 - 1.5)

  # insurance coverage level (0.7 - 0.9 in increments of 0.05)
  if(random.coverage){
    assign("clv",round_any(runif(1,0.7,0.9),0.05),envir=simvars)
  }else{
    assign("clv",clv,envir=simvars)
  }
  
  # ranch acres
  # sample from a random normal distribution with
  # mean ranch size = 3000 (default) and sd = 500
  # these are arbitrary choices!! should revise as needed
  if(random.acres){
    assign("acres",round(sample(rnorm(10000,mean=3000,sd=500),size=1)),envir=simvars)
  }else{
    assign("acres",acres,envir=simvars) 
  }
  
  # ranch productivity factor (0.6 - 1.5)
  if(random.productivity){
    assign("pfactor",round_any(runif(1,0.6,1.2),0.01),envir=simvars)
  }else{
    assign("pfactor",pfactor,envir=simvars) 
  }
  
  # Insurance purchases
  # Use Excel model choices by default,
  # otherwise automatically allocate based
  # upon forage potential
  attach(station.gauge) # reference the `station gauge` environment's vars
  if(autoSelect.insurance == TRUE){
    assign("insp", insAlloc(fpwt = zonewt[stzone, ], niv = 2), envir = simvars) # automatic selection
  }else{
    assign("insp", rbind(c(3, 0.5), c(5, 0.5)), envir = simvars) # insurance purchase
  }
  detach(station.gauge)
  
  ## Precip, Forage Potential, and Calf Weight variables
  assign("styear", get("yyr", simvars)[1], envir = simvars) # Starting "drought" year
  assign("dr_start", get("act.st.m", envir = constvars), envir = simvars) # Drought adaptive action starts
  assign("dr_end", get("act.end.m", envir = constvars), envir = simvars) # Drought action ends 
  
}
