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

getSimVars=function(random.starts=F,use.forage=F,autoSelect.insurance=F,clv=0.9,acres=3000,pfactor=1){
  
  "
  Note that this will default to the excel 
  model var's
  "
  options(warn=-1) # doesn't seem to get rid of warnings...
  
  ## Ensure the baseline environments exist
  if(!exists("station.gauge",envir=globalenv())){
    stop("Station gauge information is required.")
  }
  
  if(!exists("constvars",envir=globalenv())){
    stop("Constant variable information is required.")
  }
  
  # Remove the `simvars` environment if one currently exists
  if(exists("simvars",envir=globalenv())){
    rm("simvars",envir=globalenv())
  }
  
  # Create a fresh simulation vars environment
  simvars<<-new.env()
  
  ## Range of years
  # if specified, use a random starting year
  if(random.starts){
    assign("styr",round(runif(1,1948,2010)),envir=simvars)
  }else{
    assign("styr",2002,envir=simvars) # starting year in five-year period 
  }
  
  ## Static vs dynamic vars, based on forage
  attach(constvars) # use direct reference to `constvars` environment
  if(use.forage){
    assign("wn.wt",calfWeanWeight(get("styr",simvars)),envir=simvars) # dynamic by year based on precip/forage
  }else{
    assign("wn.wt",c(calfWeanWeight(get("styr",simvars))[1],rep(expected.wn.wt,4)),envir=simvars) # year 1 only based on precip/forage
  }
  
  # Drought action var's
  assign("drought.action",ifelse(1:5 %in% act.st.yr:act.end.yr, 1, 0),envir=simvars)
  assign("calf.loss",ifelse(get("drought.action",simvars)==1,2,0),envir=simvars)
  assign("calf.wt.adj",ifelse(get("drought.action",simvars)==1,-0.1,0),envir=simvars)
  detach(constvars)
  
  ## Wean weights
  # make this constant??
  # previously 'p.wn.yr1', now vectorized for iteration
  # (I only have this here because vectors can't be represented well in `constant_vars.csv`)
  assign("p.wn",c(1.31,1.25,1.25,1.25,1.25),envir=simvars)
  
  
  ## set target insurance years
  attach(simvars)
  assign("yyr",styr:(4+styr),envir=simvars) # all five years
  detach(simvars)
  
  ## Set Insurance variables
  assign("clv",clv,envir=simvars) # insurance coverage level (0.7 - 0.9 in increments of 0.05)
  assign("acres",acres,envir=simvars) # ranch acres
  assign("pfactor",pfactor,envir=simvars) # productivity factor (0.6 - 1.5)
  
  # Insurance purchases
  # Use Excel model choices by default,
  # otherwise automatically allocate based
  # upon forage potential
  attach(station.gauge) # reference the `station gauge` environment's vars
  if(autoSelect.insurance){
    assign("insp",insAlloc(fpwt=zonewt[stzone,],niv=2),envir=simvars) # automatic selection
  }else{
    assign("insp",rbind(c(3,0.5),c(5,0.5)),envir=simvars) # insurance purchase
  }
  detach(station.gauge)
  
  ## Precip, Forage Potential, and Calf Weight variables
  assign("styear",get("yyr",simvars)[1],envir=simvars) # Starting "drought" year
  assign("dr_start",get("act.st.m",envir=constvars),envir=simvars) # Drought adaptive action starts
  assign("dr_end",get("act.end.m",envir=constvars),envir=simvars) # Drought action ends 
  
}
