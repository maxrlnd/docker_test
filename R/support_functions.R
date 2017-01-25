
# Variable Assignment Functions -------------------------------------------

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

getSimVars = function(random.starts = FALSE,
                      use.forage = FALSE,
                      autoSelect.insurance = FALSE,
                      clv = 0.9,
                      acres = 3000,
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


# Weather Functions -------------------------------------------------------

getStationGauge<-function(target.loc="CPER"){

  "
  Author: Joe

  Returns precipitation record and locational attributes for the target
  location. Default is Central Plains Experimental Range (CPER) but alternative
  locations at COOP sites across Colorado may be specified.

   If CPER (default):

     Zone Weights `zonewt` are read from the Excel model which is based on [xx]
      drought calculator state forage potential weights that we cannot reproduce.
      We are missing spatial reference information necessary to assign each target
       location to a state zone.

     Weights Zone `stzone` which corresponds to the state weights zone in which
      CPER resides (Colorado Zone 3).

     Station Gauge `stgg`, historical precipitation totals dating back to 1948,
      which are also read in from the Excel model. Precip totals are collected at
       CPER itself and do not rely on precip data from COOP sites.

     Target Grid Cell `tgrid`, for reading in PRF index values at a given point
      in time. `tgrid` is assigned to the PRF grid cell where we assume CPER exists,
       25002.

   If COOP site (user-specified):

     Zone Weights `zonewt` are based upon the Major Land Resource Area in which
      the COOP site resides. The MLRA forage potential is an average of plant growth
       curves calculated for a series of ecological site surveys (ESS) performed for
        that MLRA (using functions `COOPinMRLA` `getMLRAWeights`). Our decision to
         use an average plant growth curve is a placeholder that could be replaced,
          for example, by a regression framework like the one used to calculate
           the state weights. Alternatively, we could contact the authors of the
            [xx] state weights and use these instead of MLRAs (since these are
               likely more accurate than the weights used here).

     Weights Zone which in this case corresponds to the MLRA site within which
      the target COOP site resides.

     Station Gauge `stgg`, which is read from the historical precipitation data
      `precip` attached to the target COOP site list `target.coop`.

     Target Grid Cell which belongs to the list of COOP site attributes
      `target.coop`. This variable is computed by converting the coordinates of
       the COOP site to a SpatialPoint object and finding the underlying PRF
       grid cell.

 Upon completion of the function `getStationGauge`, a new sub-environment
  `station.gauge` is generated, which contains `zonewt`, `stzone`, `stgg`,
   and `tgrd` based on the target location.

  "

  # clear station gauge environment if previously written
  if(exists("station.gauge",envir = globalenv())){
    rm("station.gauge",envir=globalenv())
  }

  if(target.loc=="CPER"){ # Use COOP sites or CPER: Default to CPER

    ## Zone Weights
    stzone <- 3 # state forage zone
    # multiple operations since reading from
    # external file that may be replaced
    zonewt <- read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx", sheet = "Drought Calculator", skip = 5)[5:8, ]
    zonewt <- sapply(data.frame(zonewt[, which(names(zonewt) == "Jan"):which(names(zonewt) == "Dec")]), as.numeric)

    ## Station precip gauge
    stgg <- data.frame(read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx", "CPER Precip", skip = 1))
    stgg <- stgg[, -which(names(stgg) %in% c("TOTAL", "Var.15"))]
    stgg <- stgg[stgg$Year %in% c(1948:2016, "AVE"), ]

    ## Target grid cell
    tgrd = 25002  # target grid cell - CPER default

  }else{ #Custom location specified (COOP site and MLRA forage potential weights)

    ## Fetch data
    wrc.state <- "co" # For pulling COOP sites & mlra forage weights
    load("data/coops.RData") # Shortcut for sourcing 'R/coop_scraper.R'
    # source("R/coop_scraper.R") # the long way
    mlra <- readOGR("data", "mlra_v42") # load MLRA zone data
    target.coop <- coops[[which(names(coops) == target.loc)]]

    ## Zone weights
    mlra.idx <- COOP_in_MRLA(target.coop) # MLRA index
    zonewt <- getMRLAWeights(wrc.state) # zone weights
    stzone <- which(zonewt[, 1] == mlra.idx) # not a great workaround...should fix 'foragePwt' function instead
    zonewt <- zonewt[, -1] # not a great workaround...should fix 'foragePwt' function instead

    ## Station precip gauge
    stgg <- target.coop$precip
    stgg <- rbind(stgg, rep(NA, ncol(stgg)))
    stgg[nrow(stgg), ][, 1] <- "AVE"
    stgg[nrow(stgg), ][, -1] <- colMeans(stgg[-nrow(stgg), ][, -1],na.rm = TRUE)

    ## Target grid cell
    tgrd <- target.coop$grid  # target grid cell - custom site

  }

  # Write vars to new env
  station.gauge <<- new.env()
  assign("zonewt", zonewt, envir = station.gauge)
  assign("stzone", stzone, envir = station.gauge)
  assign("stgg", stgg , envir = station.gauge)
  assign("tgrd", tgrd, envir = station.gauge)

  # SpatialPoints representation of target gridcell
  # for fetching insurance results
  assign("tgrd_pt", rastPt[rastPt@data$layer == tgrd, ],envir = station.gauge)

}


# Raster Functions --------------------------------------------------------

gridToRaster <- function(grid, rasterTemplate){
  # Author: Adam
  #
  # Simple wrapper to turn a matrix into a raster using a template
  #
  # Args:
  #   grid: data in matrix format
  #   rasterTemplate: template to use in rasterization of the matrix
  #
  # Returns: Raster of grid data
  #
  require(raster)
  return(raster(grid, template = rasterTemplate))
}

dataToRast<-function(inData,target.var=NULL){

  "
  Author: Joe

  Convert a data.frame/data.table field
  to raster.

  inData: input data frame/table.

  target.var: character representing fields
    to map to raster grid
  "

  grd.idx=match(inData$grid,as.numeric(gridMatrix))
  dataRast=gridMatrix
  dataRast[grd.idx]=inData[[target.var]]
  dataRast[-grd.idx]=NA
  dataRast=gridToRaster(dataRast,tempRaster)
  return(dataRast)

}


# Insurance Functions -----------------------------------------------------

dcInfo <- function(dc, tgrd){

  "
  Author: Joe

  Extracts drought calculator info from a
  grid cell.

  dc: 'droughtCalculator' output (class LIST)

  tgrd: target grid cell ID
  "

  dcinf = lapply(dc, function(X){
    extract(X, rastPt[rastPt$layer == tgrd, ])
  })
  return(dcinf)
}

droughtCalculator <- function(yy, clv, acres, pfactor, insPurchase, mask = NULL){
  "
  Author: Joe

  Emulates RMA's precipitation-based
  insurance index in raster. NOTE
  that premium/indemnity estimates
  will be slightly off those of RMA
  because our index values 'intervalNOAA'
  slightly disagree.

  yy: Year of interest.

  clv: RMA coverage level. Accepted values
  are 0.7, 0.75, 0.8, 0.85, 0.9

  acres: Insured acres of grazing land.

  pfactor: Productivity factor of grazing land.

  insPurchase: a matrix of intervals from 1-11
  for which insurance is purchased. For example,
  purchases for the April-May and May-June intervals
  at 50% protection each would be entered as

  `rbind(c(3,0.5),c(5,0.5))`

  Consecutive intervals are not allowed.

  Returns a list of outputs:

  $prem_noSbdy: total premium with subsidy
  $prem_wSbdy: total premium without subsidy
  $prodPrem: producer premium
  $indemrate: indemnity rate (stack, by month)
  $indemnity: indemnity (stack, by month)
  #indemtot: total indemnity

  "

  ##Make sure inputs are valid
  #   if(!yy %in% 2000:2015){
  #     stop("Input year must occur in the range 2000-2015.")
  #   }
  # if(!clv %in% round(seq(0.7,0.9,by=0.05),1)){
  #   stop("Invalid coverage level. Accepted coverage levels are 0.7, 0.75, 0.8, 0.85, 0.9.")
  # }
  # if(!pfactor %in% round(seq(0.6:1.5,by=0.01),1)){
  #   stop("Productivity factor must range from 0.6-1.5, in increments of 0.01.")
  # }
  if(min(dist(insPurchase[, 1])) == 1){
    stop("Insurance allocation for consecutive intervals is not permitted.")
  }
  if(nrow(insPurchase) <= 1){
    stop("Insurance must be allocated for at least two intervals.")
  }
  if(prod(insPurchase[, 1] %in% 1:11) == 0){
    stop("Insurance allocation intervals must range from 1-11.")
  }
  if(max(insPurchase[, 2]) > 0.6){
    stop("Insurance allocation may not exceed 60% per interval.")
  }
  if(sum(insPurchase[, 2]) != 1){
    stop("Insurance allocation must sum to 100%.")
  }

  #   ##Clip by mask if specified
  #   if(!is.null(mask)){
  #     mask=subRast(gridRast,mask) #assign grid indices to mask
  #     basePrice=subRast(basePrice,mask)
  #     prem100=prem100[grid %in% as.numeric(as.matrix(mask)),]
  #     intervalNOAA=intervalNOAA[grid %in% as.numeric(as.matrix(mask)),]
  #     grid_elig=grid_elig[grid_elig$grid %in% as.numeric(as.matrix(mask)),]
  #   }

  ##Get subsidy rate based on coverage level
  sbdy <- covsub[coverage.trigger == clv, subsidy.rate]

  ##Set up insurance purchase vector
  ip = rep(0, 11)
  ip[insPurchase[, 1]] = insPurchase[, 2]  # replaces 0's with interval allocations
  insPurchase = ip

  ##Calculate policy rate
  plrt = prod(clv, acres, pfactor) * basePrice  # why use prod() instead of multiplying?

  ##Generate inputs for computing premiums
  premInt=stack() #Premium/$100 rate
  protection=stack() #Protection amount
  actualidx=stack() #"Actual Index Value" from RMA (precip index)
  eligmask=stack() #eligibility mask
  for (i in which(insPurchase>0)){

    premInt=stack(premInt,
                  dataToRast(prem100,paste0("i",i)))

    protection=stack(protection,
                     plrt*insPurchase[i])

    actualidx=stack(actualidx,
                    dataToRast(intervalNOAA[Year==yy & interval==i,value,grid],"value")/100)

    eligmask=stack(eligmask,
                   dataToRast(data.frame(grid=grid_elig$grid,
                                         value=ifelse(grid_elig[[paste0("i",i)]]==1,1,NA)),"value"))

  }

  # names(premInt)=paste0("i",which(insPurchase>0))
  # names(protection)=paste0("i",which(insPurchase>0))
  # names(actualidx)=paste0("i",which(insPurchase>0))

  ##Compute premiums
  prem_noSbdy=sum(premInt*protection*0.01)
  prem_wSbdy=prem_noSbdy*sbdy
  prodPrem=prem_noSbdy*(1-sbdy)

  ##round premiums to match RMA
  #(as closely as possible)
  prem_noSbdy=round(prem_noSbdy,2)
  prem_wSbdy=round(prem_wSbdy,2)
  prodPrem=round(prodPrem,2)
  actualidx=round(actualidx,3)

  ##Compute indemnities
  rma_rc=rbind(c(-Inf,clv,1),c(clv,Inf,0))
  iscov=reclassify(actualidx,rma_rc) #binary raster of payouts triggered at 'clv'
  indemrate=((clv-actualidx)/clv)*iscov*eligmask #pct diff from 'clv' if payout triggered
  indemnity=indemrate*protection
  indemtot=sum(indemnity) #total indemnity amount

  ##Prepare outputs
  outList=list()
  outList$prem_noSbdy=prem_noSbdy
  outList$prem_wSbdy=prem_wSbdy
  outList$prodPrem=prodPrem
  outList$indemrate=indemrate
  outList$indemnity=indemnity
  outList$indemtot=indemtot

  return(outList)
}

insMat <- function(tgrd, yyr, clv, acres, pfactor, insPurchase){

  "

  Generates a matrix representing insurance
  premium payments and indemnities for a
  specified grid cell over a five-year interval.

  tgrd: target grid cell

  yyr: starting year

  clv: coverage level

  acres: insured acres

  pfactor: land productivity factor

  insPurchase: a matrix representing
  insurance allocation to two-month
  intervals, with rows written in the
  format [mm,amt]
  "

  ## Generate insurance info
  fiveYears = matrix(0, 5, 3)  # empty matrix - year, indemnity, producer premium x number years
  fiveYears[, 1] = seq(yyr[1], yyr[1] + 4)  # populate years in first column
  #**PARALLELIZE THIS??**#
  for(yy in yyr){

    fiveYears[which(fiveYears[, 1] == yy), ] = c(yy,
                                                 unlist(
                                                   dcInfo(
                                                     dc = droughtCalculator(
                                                       yy = yy,
                                                       clv = clv,
                                                       acres = acres,
                                                       pfactor = pfactor,
                                                       insPurchase = insPurchase
                                                     ),
                                                     tgrd = tgrd)[c("prodPrem", "indemtot")]
                                                 )
    )
  }

  return(fiveYears)

}

rescaleInsAlloc<-function(alloc_choice,max.alloc=0.6,min.alloc=0.1){

  "
  Helper function for rescaling insurance
  allocation percentages by a maximum/
  minimum allocation percentage.

  Currently this function is set up so that
  a compromise will be reached if rescaling
  the top allocation percentage to the maximum
  produces values of <10% (or <`min.alloc`) for
  the remaining intervals. The remaining intervals
  will be rescaled to the minimum amount, potentially
  leaving the top allocation below `max.alloc`
  "

  if(max.alloc>0.6){
    stop("Maximum allocation cannot exceed 0.6.")
  }

  if(min.alloc<0.1){
    stop("Minimum allocation must be at least 0.1.")
  }

  if(min.alloc>max.alloc){
    stop("Minimum allocation cannot be greater than maximum allocation.")
  }

  if(max.alloc<(1/length(alloc_choice))){
    warning("Maximum allocation is less than uniform allocation size. Allocation amounts will
            be disproportionate to their original values.")
  }

  if(min.alloc>(1/length(alloc_choice))){
    warning("Maximum allocation is less than uniform allocation size. Allocation amounts will
            be disproportionate to their original values.")
  }

  if(alloc_choice[1]>max.alloc){
    alloc_choice[1]=max.alloc
    alloc_choice[-1]=(alloc_choice[-1]/sum(alloc_choice[-1]))*(1-max.alloc)
  }

  # Readjust min if remaining intervals
  # receive <min% allocation
  if(any(alloc_choice[-1]<min.alloc)){

    uidx=which(alloc_choice<min.alloc) # index of alloc intervals < min%
    alloc_choice[uidx]=min.alloc
    alloc_choice[-uidx]=(alloc_choice[-uidx]/sum(alloc_choice[-uidx]))*(1-(min.alloc*length(uidx)))

  }

  # Prevent returning negative weights
  if(any(alloc_choice<0)){
    stop("Allocation percentages must be positive.")
  }

  return(alloc_choice)

  }

insAlloc<-function(fpwt, niv = 2, by.rank = T, max.alloc=0.6, min.alloc = 0.1){

  "
  Automates range insurance allocation to two-month
  RMA intervals using a grid cell/COOP site's forage
  potential weights. Returns a matrix formatted as the
  `insPurchase` input for function `insMat`.

  Allocation for chosen two-month intervals is roughly
  proportional to the relative value of each interval's
  forage potential weight. Adjustments to allocation
  percentages are automatically made if a selection is invalid
  for one or more intervals, either too high (>60%) or too low
  (10%).
  User-specified min/max allocation percentages falling within
  this range may also be substituted by setting the `max.alloc`
  and `min.alloc` arguments.


  Inputs:

  `fpwt`: A vector of monthly forage potential weights
  for the target site. Monthly intervals are averaged
  to two-month intervals to match RMA insurance
  selections.

  `niv`: Number of two-month intervals to insure.

  `by.rank`: if TRUE (default), ranks forage potential
  weights by interval in descending order and selects
  the `niv` most highly ranked non-consecutive intervals
  to insure.

  If FALSE, selects the combination of `niv` non-
  consecutive two-month intervals with the highest
  average forage potential weights.

  `max.alloc`: Maximum insurance protection allocation. Must
  not exceed 0.6.

  `min.alloc`: Minimum insurance protection allocation. Must
  be at least 0.1.

  "

  fpwt_iv=forageWeights2Intervals(fpwt) # bin forage potential weights into intervals

  if(by.rank){

    fpwt_iv_rank=rank(-fpwt_iv) # rank interval weights descending
    names(fpwt_iv_rank)=paste0("i",1:11)
    top_iv=which.min(fpwt_iv_rank) # index of top-ranked interval

    wt_iv=top_iv
    cand_iv=fpwt_iv_rank[-c((top_iv-1):(top_iv+1))] # candidate secondary intervals

    for(i in 2:niv){

      excl_iv=unique(unlist(lapply(wt_iv,function(X)-1:1+X))) #intervals to exclude (prev. chosen/overlapping)
      cand_iv=fpwt_iv_rank[-excl_iv]

      # Append the highest-ranked choice to 'wt_iv'
      if(length(cand_iv)>0){
        iv_select=names(cand_iv[which.min(cand_iv)]) # get top-ranked candidate weight
        iv_select=as.numeric(substr(iv_select,2,nchar(iv_select))) # get original index from interval name
        wt_iv=c(wt_iv,iv_select)
      }else{ # end if no choices left
        break
      }

    }

  }else{ #use max combination of weights / number of intervals specified

    # Separate intervals
    # this ensures no overlap
    odd_iv=seq(1,11,by=2)
    even_iv=seq(2,10,by=2)

    # All combos of intervals
    iv_comb=cbind(combn(odd_iv,niv),combn(even_iv,niv))

    # Mean of weights for interval combos
    combwt=c()
    for(i in 1:ncol(iv_comb)){

      combwt=c(combwt,
               (sum(fpwt_iv[iv_comb[,i]])/niv))

    }

    # Select best intervals
    wt_iv=iv_comb[,which.max(combwt)]


  }

  wt_choice=fpwt_iv[wt_iv] # get weight values
  wt_alloc=wt_choice/sum(wt_choice) #Convert weights to allocation amounts
  wt_out=cbind(wt_iv,wt_alloc) # convert to matrix
  wt_out=wt_out[order(-wt_out[,2]),] # sort descending

  # Readjust max/min allocation percentages if too high/low
  if(max(wt_out[,2])>max.alloc | min(wt_out[,2])<min.alloc){
    wt_out[,2]=rescaleInsAlloc(wt_out[,2],max.alloc,min.alloc)
  }

  # Round allocation pcts to 2 signif. digits
  # since PRF decision model only runs on
  # whole number allocation pcts
  wt_out[,2]=round(wt_out[,2],2)

  if(sum(wt_out[,2])!=1){
    warning("Allocation percentages do not sum to 1.")
  }

  return(wt_out)

}


# Forage Functions -------------------------------------------------------

foragePWt <- function(stgg, zonewt, stzone, styear, decision = FALSE){

  "
  Returns a weight representing
  annual forage potential for a
  given gridcell or station
  gauge's annual precip record.

  By default, computes the sum of weighted product of
  forage potential and long-term precipitation deviation
  from average for a given year relative to a grid cell
  or station gauge's period of record.

  A 'decision making under uncertainty' mode is also
  available (when 'decision' is set to TRUE):

    To build monthly weights, generates a typology
    of years 1948-2015 (k-medoids) based on
    monthly precip values observed at the
    station.

    To assemble the weights:

      Use the product of deviation in precip from
      long-term (1948-2015) average * zone weights
      for months occurring before & during the
      decision month.

      For months occurring after
      the decision month, use the product of group
      average deviation from the long-term
      average * zone weight.

    This approach roughly approximates a 'best guess'
    scenario based on rain gauge observations -
    what should my precip for the remainder of the year
    look like given what I know by the decision month?

  **EVENTUALLY NEEDS INPUTS FOR
    STATE**

    stgg: station gauge or grid cell precip record
    stzone: state zone
    zonewt: weights for state zone
    styear: year of interest
    decision: use 'decision under uncertainty' mode
      (default FALSE)

  "

  ## Subset zone weights and prep index
  zonewt <- zonewt[stzone, ]  # subset weights by station/grid zone
  yprecip <- stgg[stgg[, 1] == styear, ][, -1]  # monthly precip amounts for start year
  # ave <- stgg[stgg$Year == "AVE", ][, -1]  # average precip since 1948
  ave <- stgg[nrow(stgg), ][, -1]  # average monthly precip since 1948
  pidx  <- yprecip / ave # Monthly precip "index"

  if(decision){ #"decision making under uncertainty" mode

    ## Group years in period of record by monthly precip
    cper <- stgg[1:which(stgg$Year == 2015),] # subset by period 1948-2015
    cper_clust <- pamk(cper[, -1]) # Find optimal groups of years, k = 2-10
    yy_group <- cper_clust[[1]]$clustering[which(cper$Year == styear)] # Group membership for target year
    yy_ave <- colMeans(cper[which(cper_clust[[1]]$clustering == yy_group), ][, -1]) # Group mean vector
    yidx <- yy_ave/ave # Expected index values for year (group mean vector / long-term average)

    # Generate forage potential weights
    # not sure why the rows are subsetting as lists!
    foragewt <- unlist(c((zonewt * pidx)[1:dr_start], (zonewt * yidx)[(dr_start + 1):12]))

  }else{ #default: long-term precip & zone weights

    foragewt = zonewt * pidx

  }

  # Compute annual forage potential weight for zone
  forage.potential <- sum(foragewt)

  forage.potential

}

getMRLAWeights<-function(state.code){

  "
  Computes forage potential weights using the
  mean of plant growth curves by MRLA for a
  specified state.

  Data source:

  https://esis.sc.egov.usda.gov/WelcomeFSG/pgFSGSelectFormat.aspx

  Inputs:

  state.code: two-letter state code (for referencing
  plant growth potential curves)

  "

  forage_mlra=read.table(paste0("data/",state.code,"_mlra.txt"),sep="|")
  forage_mlra[,1]=substr(forage_mlra[,1],3,4) # Get MRLA code
  forage_mlra=forage_mlra[!forage_mlra$V1 %in% c("00","01","99"),] # Remove placeholder plant growth sites
  forage_mlra=forage_mlra[,c(1,which(names(forage_mlra)=="V4"):ncol(forage_mlra))] # subset ID and plant growth curve
  names(forage_mlra)=c("MLRA","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

  forage_mlra=aggregate(.~MLRA,data=forage_mlra,FUN=mean) # Compute mean plant growth curves by MLRA
  forage_mlra[,-1]=forage_mlra[,-1]/100 # convert to decimal to match drought calculator fgp weights

  return(forage_mlra)
}

COOP_in_MRLA<-function(coop){

  "
  Returns the MLRA in which a specified
  coop site is located.
  "

  coop.pt <- SpatialPoints(t(rev(coop$loc)), proj4string = CRS(proj4string(mlra)))
  # fwt=forage_mlra[forage_mlra$MLRA==(coop.pt %over% mlra)$MLRARSYM,][,-1]
  return(as.numeric(as.character(((coop.pt %over% mlra)$MLRARSYM))))

}

forageWeights2Intervals<-function(fpwt){

  "
  Helper function for binning monthly
  forage weights into 2-month intervals
  matching the RMA insurance.
  "

  fpwt_iv=c()
  for(m in 1:11){

    fpwt_iv=c(fpwt_iv,(sum(fpwt[m],fpwt[m+1])/2)) # need to calc mean manually - not sure why
    names(fpwt_iv[m])=paste0("i",m)

  }

  return(fpwt_iv)

}


# Calf Weight Functions ---------------------------------------------------

calfDroughtWeight<-function(expected.wn.wt, calf.wt, forage.potential){
  return(calf.wt + (forage.potential * (expected.wn.wt - calf.wt)))
}

calfWeanWeight <- function(styr){

  "
  Compute calf weights based on station/grid cell
  forage potential for a five-year period.

  Inputs:

    styr: starting year of the five-year period.

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

  attach(station.gauge)
  attach(constvars)
  forage.weights = unlist(lapply(seq(styr, styr + 4),function(i){
    foragePWt(stgg, zonewt, stzone, i)
  }))
  calf_weights_ann = unlist(lapply(forage.weights, function(i){ # annual calf weights
    calfDroughtWeight(expected.wn.wt, calf.wt, i)
  }))
  detach(station.gauge)
  detach(constvars)

  calf_weights_ann
}

# Drought Adaptation Functions --------------------------------------------

CalculateDaysAction <- function(act.st.yr, act.st.m, act.end.yr, act.end.m, drought.action) {
  "
  Function: CalculateDaysAction
  Description: Calculate the number of days rancher pays for a drought adaptation action.
  NOTE: This function assumes that the actions take place only in one year.

  Inputs:
  act.st.yr = Year the action starts
  act.st.m = Month the action starts
  act.end.yr = Year the action ends
  act.end.m = Month the action ends
  drought.action = ??

  Outputs:
  days.act = Number of days drought adaptation action takes place (days)
  "

  # Start and End month assumes that action starts/stops on the first of the month

  # Error handling
  if (act.st.yr < 1) {
    stop("Action start year ", act.st.yr, "is not greater than 0")
  }
  if (act.st.yr > act.end.yr | act.st.yr == act.end.yr & act.st.m > act.end.m) {
    stop("Action end occurs before action start")
  }
  if (act.st.m < 1 | act.st.m > 12 | act.end.m < 1 | act.end.m > 12) {
    stop("Invalid action start or end month (not between 1 and 12)")
  }

  if (act.st.yr == act.end.yr) {
    days.act <- (act.end.m - act.st.m) * 30
  }

  # TEMPORARY ERROR: While the model is not equipped to handle multi-year droughts, we will not allow drought
  #  adaptation across multiple years.
  if (act.st.yr != act.end.yr) {
    stop("Model not equipped for multi-year drought adaptation. act.st.yr must equal act.end.yr")
  }

  days.act.vect <- days.act * drought.action #Creates a vector of days of action
  days.act.vect
}

CalculateAdaptationIntensity <- function(forage.potential, drought.adaptation.cost.factor = 1) {
  " Description: Takes forage potential and an adaptation intensity factor to 
    provide a scalar of drought action. If forage potential is above 1 (no drought), 
    then this variable goes to 0 (no adaptation). 
	  Inputs: 
      adpt.intensity.factor (parameter that scales adaptation actions to reflect 
        actual adaptation behavior. Currently defaults to 1 which assumes a 
        one-to-one ratio of drops in forage percentage to need for forage 
        replacement.)
      forage.potential (the percentage of average forage produced in a year 
        based on rainfall. See forage potential functions.)
    Output: drght.act.adj (scales action to account for forage potential's 
      deviation from the norm.)
    Assumptions: The variable has a maximum of 1, which assumes that drought 
      actions are parameterized at full forage replacement for the full herd.
  "
  drght.act.adj <- ifelse(forage.potential >= 1, 0, (1 - forage.potential) * drought.adaptation.cost.factor)
  drght.act.adj <- ifelse(drght.act.adj > 1, 1, drght.act.adj)  # putting a ceiling of this variable at 1 (no more than 100% of drought action)
  drght.act.adj
}


# Costs and Revenues ------------------------------------------------------
# Baseline Costs and Revenues
CalculateExpSales <- function(herd, calf.sell, wn.wt, p.wn) {
  "
  Function: CalculateExpSales
  Description: Calculates expected calf revenues for non-drought year

  Inputs:
  herd = Size of herd (head of cows, does not include calves)
  calf.sell = Average percentage of calves sold (%)
  wn.wt = Average weight at weaning (pounds)
  p.wn = Expected sale price of calves ($/pound)

  Outputs:
  base.sales = Expected revenues from calf sales for a non-drought year
  "

  base.sales <- herd * calf.sell * wn.wt * p.wn
  return(base.sales)
}

CalculateBaseOpCosts <- function(herd, cow.cost) {
  base.op.cost <- herd * cow.cost
  return(base.op.cost)
}

# Option 1: Buy feed
CalculateFeedCost <- function(kHayLbs, kOthLbs, p.hay,p.oth, days.feed, herd) {
  "
  Function: CalculateFeedCost
  Description: Calculating the costs of purchasing additional feed

  Inputs:
  kHayLbs = Number of pounds of additional hay needed for each cow each day (pounds/head/day).  (Source: UNKNOWN)
  p.hay = Price of hay ($/ton). User input.
  kOthLbs = Number of pounds of additional other feed needed for each cow each day (pounds/head/day). (Source: UNKNOWN)
  p.oth = Price of other feed ($/ton). Currently not a user input. Does not come into play since the model assumes only feeding hay
  days.feed = Number of days additional feed is needed. Generally, equal to days.act. (days)
  herd = Size of herd (head of cows, does not include calves)

  Outputs:
  cost.feed = Additional costs to feed the herd over the remainder of the season ($/year)
  "
  # Calculate cost per cow per day * days of feed for the year * number of cows in the herd
  feed.cost <- (kHayLbs / 2000 * p.hay + kOthLbs / 2000 * p.oth) * days.feed * herd
  return(feed.cost)
}

# Option 2: Rent Pasture
CalculateRentPastCost <- function(n.miles, truck.cost, past.rent, oth.cost, days.rent, max.wt, cow.wt, calf.wt, herd) {
  "
  Function: CalculatePastureRentCost
  Description: Calculates the costs of renting pasture and trucking pairs
  NOTE: Not including loan interest costs. This needs to be resolved across all options.

  Inputs:
  n.miles = Distance to rented pasture (miles)
  truck.cost = Trucking cost per loaded mile ($/mile/truck)
  past.rent = Price of renting pasture per animal unit month, where an animal unit is a cow/calf pair ($/pair/month)
  days.rent = Days on rented pasture. Generally, equal to days.act. (days)
  oth.cost = All other non-rental, non-trucking costs ($)
  max.wt = Maximum weight per truck (pounds)
  cow.wt = Average cow weight (pounds)
  calf.wt = Average 'current' weight of calves before trucking to rented pasture (pounds)
  herd = Size of herd (head of cows, does not include calves)
  loan.int = interest rate for borrowed money (%/year)

  Outputs:
  cost.rentpast = Total costs of using renting pasture including transport costs ($/year)
  "
  # Calculating number of trucks needed to truck pairs to pasture and to truck cows home (without calves)
  n.trucks.past <- ceiling(herd / ceiling(max.wt / (cow.wt + calf.wt)))
  n.trucks.home <- ceiling(herd / ceiling(max.wt / cow.wt))

  # Cost per herd of trucking pairs to pasture and trucking cows back home
  tot.truck.cost <- n.miles * truck.cost * n.trucks.past + n.miles * truck.cost * n.trucks.home

  # Cost of renting pasture
  tot.past.rent <- past.rent / 30 * days.rent * herd

  # Total costs including transport, rent, and other costs
  cost.rentpast.woint <- ifelse(days.rent > 0, tot.truck.cost + tot.past.rent + oth.cost, 0)
  cost.rentpast <- ifelse(days.rent > 0, cost.rentpast.woint * (1 + loan.int / 365 * days.rent), 0)  # I think we should not include interest here unless it is also included in other adaptation costs

  return(cost.rentpast)
}

CalculateRentPastRevenue <- function(expected.wn.wt, calf.loss, calf.wt.adj, calf.sell, herd, p.wn) {
  "
  CalculateRentPastRevenue
  Description: Calculates calf sale revenues after trucking pairs to rented pastures

  Inputs:
  calf.loss = Additional calf deaths due to transport stress (head of calves)
  calf.wt.adj = Adjustment for calf weaning weights (%)
  calf.sell = Average percentage of calves sold (%)
  wn.wt = Average weight at weaning (pounds)
  p.wn = Expected sale price of calves ($/pound)
  herd = Size of herd (head of cows, does not include calves)

  Outputs:
  rev.rentpast = Change in revenue due to mortality and weight loss from trucking to rented pasture
  "
  # Number of calves sold after accounting for calf mortality in transport
  calf.sales.num <- herd * calf.sell - calf.loss

  # Selling weight after accounting for weight loss due to transport stress
  sell.wt <- expected.wn.wt * (1 + calf.wt.adj)

  # Expected calf sale revenues
  rev.rentpast <- calf.sales.num * sell.wt * p.wn
  rev.rentpast
}

# Option 3: Sell Pairs & Replace
CalculateSellPrsCost <- function(op.cost.adj, herd, sell.cost, base.op.cost, herdless.op.cost) {
  "
  Function: CalculateSellPrsCost
  Description: Calculates the operating costs to sell pairs in year 1 and replacing cows in year 3
  NOTE: It is assumed that cows are replaced on last day of the second year after they are sold.
  For example, cows sold in 2011 are replaced on 12/31/2013.

  Inputs:
  op.cost.adj = Change in operating costs in year 1 per cow ($/cow/year)
  sell.cost = Selling cost per cow ($/cow) NOTE: DO WE COUNT SELLING COSTS IN A NORMAL YEAR? ARE THESE ADDITIONAL?
  herd = Size of herd (head of cows, does not include calves)
  base.cost = Baseline annual cost of operating ranch with full herd ($/year)
  fixed.op.cost = Fixed operating costs for a year without a herd ($/year)

  Outputs:
  cost.sellprs = 5x1 vector of changes in operating costs for years 1 through 5 from selling pairs in year 1 and replacing them at the end of year 3
  "
  cost.sellprs <- NULL
  cost.sellprs[1] <- base.op.cost + op.cost.adj * herd + sell.cost * herd  # CORRECT CODE!!! # Yr 1 operating costs includes a reduction in operating cost from not having the herd and the additional cost to sell cows
  # cost.sellprs[1] <- base.op.cost + op.cost.adj * herd  # INCORRECT CODE (replicates excel's exclusion of herd selling costs)
  cost.sellprs[2] <- herdless.op.cost  # fixed 'herdless' operating costs
  cost.sellprs[3] <- herdless.op.cost  # fixed 'herdless' operating costs
  cost.sellprs[4:5] <- base.op.cost  # Yr 4 & 5 change in op costs are assumed to be normal

  cost.sellprs
}

CalculateSellPrsRev <- function(base.sales, herd, wn.succ, calf.wt, p.calf.t0) {
  "
  Function: CalculateSellPrsRev
  Description: Calculates calf sales revenues due to selling pairs and replacing cows for years 1 through 3
  NOTE: It is assumed that cows are replaced on last day of the second year after they are sold.
  For example, cows sold in 2011 are replaced on 12/31/2013.

  Inputs:
  base.sales = Calf sales in a normal year ($/year)
  p.wn.t0 = Current sale price calves ($/pound)
  herd = Size of herd (head of cows, does not include calves)
  wn.succ = Average percentage of cows that successfully wean calves (%)
  calf.wt = Average 'current' weight of calves (pounds)

  Outputs:
  rev.sellprs = 5x1 vector of calf revenues for years 1 through 5.
  "
  # Calf sales revenues
  calf.sales <- rep(NA,5)
  for (i in 1:5) {
    if(i == 1) {
      calf.sales[i] <- herd * wn.succ * calf.wt * p.calf.t0
    }
    if(i == 2 | i == 3) {
      calf.sales[i] <- 0
    }
    if(i > 3) {
      calf.sales[i] <- base.sales[i]
    }
  }
  calf.sales
}


# Assets Functions --------------------------------------------------------

# Calculating Assets and Total Net Worth

CalcCowAssets <- function(t, herd, p.cow, sell.year = NA, replace.year = NA) {
  # Function: CapitalAssets
  # Description: Caluclated the cow assets for each year.

  # Inputs:
  #  herd
  #  p.cow
  #  sell.year = Single numeric value. Equal to t where year 1 is t=1.
  #  replace.year = Single numeric value. Equal to t where year 1 is t=1.

  # Output:
  #  6x1 vector of cow assets for each year, including t=0

  cow.assets <- rep(NA,t)

  if(is.na(sell.year)) {
    cow.assets[1:t] <- herd * p.cow
    cow.assets <- c(herd * p.cow, cow.assets)  # Adding time 0 cow assets to make a 6x1 vector
    return(cow.assets)
  }

  if(sell.year > 0 & is.na(replace.year)) {
    cow.assets[1:sell.year] <- herd * p.cow  # Allows for sale of herd outside of year 1
    cow.assets[sell.year:t] <- 0  # Replaces sell year with 0, leaves prior years with herd, all subsequent years with no herd
    cow.assets <- c(herd * p.cow, cow.assets)  # Adding time 0 cow assets to make a 6x1 vector
    return(cow.assets)
  }

  if(sell.year > 0 & replace.year > 0 ) {
    cow.assets[1:sell.year] <- herd * p.cow  # Allows for sale of herd outside of year 1
    cow.assets[sell.year:(replace.year-1)] <- 0  # Replaces sell year with 0, leaves prior years with herd
    cow.assets[replace.year:5] <- herd * p.cow  # After replacing, assumes no additional sales
    cow.assets <- c(herd * p.cow, cow.assets)  # Adding time 0 cow assets to make a 6x1 vector
    return(cow.assets)
  }
}

CalcCapSalesPurch <- function(assets.cow, t, cull.num, p.cow) {
  # Description: Calculates vectors of capital sales and capital purchases from
  #  changes in assets.cow. Assumes sale/purchase of cows is only capital sales/purchase
  #
  # Inputs:
  #  assets.cow = tx1 vector of the value of cow assets each year.
  #
  # Outputs:
  #  cap.sales = tx1 vector of capital sales for each year
  #  cap.purch = tx1 vector of capital purchases for each year
  n <- length(assets.cow)
  cap.sales <- c(0, rep(NA, t))
  cap.purch <- c(0, rep(NA, t))
  for (i in 2:n) {
    # If cow assets increase and they were not 0 in the prior year, then cap sales equal to normal culling and
    if(assets.cow[i] > assets.cow[i-1] & assets.cow[i-1] != 0) {
      cap.sales[i] <- cull.num * p.cow  # Normal culling
      cap.purch[i] <- assets.cow[i] - assets.cow[i-1]
    }
    # If cow assets increase and they were 0 in the prior year, then no cap sales and cap purchases equal to change in assets
    if(assets.cow[i] > assets.cow[i-1] & assets.cow[i - 1] == 0) {
      cap.sales[i] <- 0
      cap.purch[i] <- assets.cow[i] - assets.cow[i-1]
    }
    # If cow assets decrease, then capital sales are equal to the change in cow assets, then cap sales equal to the change in assets
    if(assets.cow[i] < assets.cow[i - 1]) {
      cap.sales[i] <- assets.cow[i-1] - assets.cow[i]
      cap.purch[i] <- 0
    }
    # If cow assets are unchanged, then capital sales are equal to the normal culling
    if(assets.cow[i] == assets.cow[i - 1] & assets.cow[i] != 0) {
      cap.sales[i] <- cull.num * p.cow
      cap.purch[i] <- 0
    }
    # If cow assets are unchanged at 0, then capital sales and purch are 0
    if(assets.cow[i] == assets.cow[i - 1] & assets.cow[i] == 0) {
      cap.sales[i] <- 0
      cap.purch[i] <- 0
    }
  }
  list(cap.sales, cap.purch)
}

CalcCapTaxes <- function(herd, p.cow, cap.sales, cap.purch, cap.tax.rate, drought.emrg = 1)  {
  # Function: CalcCapTaxes
  # Description: Calculates capital taxes on herd sales. Tax treatment is
  # different depending on whether herd is sold and replaced by the end of the
  # third year or if the herd is sold and not replaced during a drought emergency.
  # Assumes that the entire herd is sold and replaced at the same rate.
  # Not sure how the tax code treats changes in prices. This abstracts away from that.
  # The price dynamics could matter here, but for now we are leaving them out.
  #
  # Inputs:
  #  cap.sales
  #  cap.purch
  #  cap.tax.rate
  #  drought.emrg = Binary variable to indicate whether drought emergency was in place when the herd was sold.
  #    currently set to a default of 1. This only matters if the herd is sold and not replaced.
  #
  # Outputs:
  #  cap.taxes <- 5x1 vector of capital taxes

  n <- length(cap.sales)
  cap.taxes <- cap.sales * cap.tax.rate  # default value is standard capital tax rate
  herd.value <- herd * p.cow

  # Special tax treatment for herd sales due to drought:
  for (i in 1:n) {
    if(cap.sales[i] == herd.value & cap.purch[i] == 0 & cap.purch[i+1] == 0 & cap.purch[i+2] == 0 & drought.emrg == 1) {  # if herd is sold and not replaced by the end of the 2nd year after the purchase and there is a drought emergency
      cap.taxes[i] <- 0
      cap.taxes[i+1] <- cap.sales[i] * cap.tax.rate  # then the capital taxes can be delayed by one year
    }
    if(cap.sales[i] == herd.value & cap.purch[i] == 0 & cap.purch[i+1] == 0 & cap.purch[i+2] == 0 & drought.emrg == 0) {  # if herd is sold and not replaced by the end of the 2nd year after the purchase and there is not a declared drought emergency
      cap.taxes[i] <- cap.sales[i] * cap.tax.rate  # then the capital taxes occur in the year of the sale
    }
    if(cap.sales[i] == herd.value & cap.purch[i+1] == herd.value | cap.sales[i] == herd.value & cap.purch[i+2] == herd.value ) {   # if herd is sold and replaced within 2 years, then there are no capital taxes
      cap.taxes[i] <- 0
    }
  }
  cap.taxes
}


# Simulation Run Functions -------------------------------------------------

OptionOutput <- function(t, opt, nodrought = FALSE, rev.calf, rev.oth = NULL,
                         cost.op, rma.ins, int.invst, int.loan, start.cash,
                         assets.cow, cap.sales, cap.purch, cap.taxes) {
  # Function: OptOutput
  # Desciption: Takes in cost and revenue variables and outputs data.frame with
  # all relevant outcome variables
  #
  # Inputs:
  # t = Number of years
  # opt = String to label option: "nodrght", "noadpt", etc.
  # nodrought = OPTIONAL value. default is set to false. set to true for no drought option
  # rev.calf = tx1 vector of revenue from actual calf sales in years 1 through t
  # rev.oth = OPTIONAL tx1 vector of non-calf revenues (created to account for interest on sale of cows in year 1)
  # int.invst = Interest rate on investments
  # int.loan = Interest rate on loans
  # rma.ins = tx3 matrix of insurance year, premium, and payouts
  # cost.op = tx1 vector of operating costs for years 1 through t, including any adaptation costs
  # int.invst = interest rate on positive cash assets (savings)
  # int.loan = interest rate on negative cash asssets (loans)
  # start.cash = starting cash assets at t=0
  # assets.cow = tx1 vector of the value of cow assets in each year
  #
  # Outputs:
  #  out = dataframe of all major variables of interest:
  #   opt
  #   yr,
  #   ins,
  #   rev.calf
  #   rev.ins
  #   rev.int
  #   rev.tot
  #   cost.op
  #   cost.ins
  #   cost.int
  #   cost.tot
  #   profit
  #   taxes
  #   aftax.inc
  #   cap.sales
  #   cap.purch
  #   cap.taxes
  #   assets.cow
  #   assets.cash
  #   net.wrth

  n <- (t + 1) * 2   # sets length as years plus 1 for initial year, times 2 for insurance and no insurance
  option <- rep(opt, n)
  yr <- c(0, 1:t, 0, 1:t)
  ins <- c(rep(1, t+1), rep(0, t+1))

  rev.calf <- c(0, rev.calf, 0, rev.calf)
  cost.op <- c(0, cost.op, 0, cost.op)

  cost.ins <- c(0, rma.ins[, 2], 0, rep(0, t))  # insurance for 2:6, no insurance for 8:12

  if(nodrought == FALSE) {
    rev.ins <- c(0, rma.ins[, 3], 0, rep(0, t))  # potential payout for 2:6, no payout for 8:12
  } else {
    rev.ins <- rep(0, n) # no drought, no payout
  }

  if(!is.null(rev.oth)) {  # if other revenues are passed through, then they are included in total revenues
    rev.oth <- c(0, rev.oth, 0, rev.oth)
    rev.tot.noint <- rev.calf + rev.ins + rev.oth
  } else {
    rev.tot.noint <- rev.calf + rev.ins
  }

  out <- data.frame(opt, yr, ins, rev.calf, rev.ins, rev.tot.noint, cost.op, cost.ins, cap.sales, cap.purch, cap.taxes, assets.cow)

  #WARNING: UGLY UGLY CODE AHEAD. Get a handle on dplyr and revisit.
  #Split into ins and non-insurance; calculate interest income, profits, and cash assets; then put back together
  out.ins <- out[out$ins==1, ]
  out.noins <- out[out$ins==0,]

  out.ins$rev.int <- c(0, rep(NA, t))
  out.ins$cost.int <- c(0, rep(NA, t))
  out.ins$rev.tot <- c(0, rep(NA, t))
  out.ins$cost.tot <- c(0, rep(NA, t))
  out.ins$profit <- c(0, rep(NA, t))
  out.ins$taxes <- c(0, rep(NA, t))
  out.ins$aftax.inc <- c(0, rep(NA, t))
  out.ins$assets.cash <- rep(start.cash, t+1)
  for (i in 2:(t+1)) {
    if(out.ins$assets.cash[i - 1] > 0) {
      out.ins$rev.int[i] <- out.ins$assets.cash[i - 1] * (invst.int)
    } else {
      out.ins$rev.int[i] <- 0
    }
    if(out.ins$assets.cash[i - 1] < 0) {
      out.ins$cost.int[i] <- -1 * (out.ins$assets.cash[i - 1] * (loan.int))
    } else {
      out.ins$cost.int[i] <- 0
    }
    out.ins$rev.tot[i] <- out.ins$rev.int[i] + out.ins$rev.tot.noint[i]
    out.ins$cost.tot[i] <- out.ins$cost.int[i] + out.ins$cost.op[i] + out.ins$cost.ins[i]
    out.ins$profit[i] <- out.ins$rev.tot[i] - out.ins$cost.tot[i]
    out.ins$taxes[i] <- ifelse(out.ins$profit[i] > 0, out.ins$profit[i] * (0.124+0.15+0.04), 0)  # taxes only if positive profits. i wonder if EITC applies here?
    out.ins$aftax.inc[i] <- out.ins$profit[i] - out.ins$taxes[i]
    out.ins$assets.cash[i] <- out.ins$assets.cash[i-1] + out.ins$aftax.inc[i] + out.ins$cap.sales[i] - out.ins$cap.purch[i] - out.ins$cap.taxes[i]
  }

  out.noins$rev.int <- c(0, rep(NA, t))
  out.noins$cost.int <- c(0, rep(NA, t))
  out.noins$rev.tot <- c(0, rep(NA, t))
  out.noins$cost.tot <- c(0, rep(NA, t))
  out.noins$profit <- c(0, rep(NA, t))
  out.noins$taxes <- c(0, rep(NA, t))
  out.noins$aftax.inc <- c(0, rep(NA, t))
  out.noins$assets.cash <- rep(start.cash, t+1)
  for (i in 2:(t+1)) {
    if(out.noins$assets.cash[i - 1] > 0) {
      out.noins$rev.int[i] <- out.noins$assets.cash[i - 1] * (invst.int)
    } else {
      out.noins$rev.int[i] <- 0
    }
    if(out.noins$assets.cash[i - 1] < 0) {
      out.noins$cost.int[i] <- -1 * (out.noins$assets.cash[i - 1] * (loan.int))
    } else {
      out.noins$cost.int[i] <- 0
    }
    out.noins$rev.tot[i] <- out.noins$rev.int[i] + out.noins$rev.tot.noint[i]
    out.noins$cost.tot[i] <- out.noins$cost.int[i] + out.noins$cost.op[i] + out.noins$cost.ins[i]
    out.noins$profit[i] <- out.noins$rev.tot[i] - out.noins$cost.tot[i]
    out.noins$taxes[i] <- ifelse(out.noins$profit[i] > 0, out.noins$profit[i] * (0.124+0.15+0.04), 0)  # taxes only if positive profits. i wonder if EITC applies here?
    out.noins$aftax.inc[i] <- out.noins$profit[i] - out.noins$taxes[i]
    out.noins$assets.cash[i] <- out.noins$assets.cash[i-1] + out.noins$aftax.inc[i] + out.noins$cap.sales[i] - out.noins$cap.purch[i] - out.noins$cap.taxes[i]
  }


  out <- rbind(out.ins, out.noins)  # Recombine ins and no ins dataframes
  out$rev.tot.noint <- NULL  # Remove total revenue without insurance variable
  out$net.wrth <- out$assets.cash + out$assets.cow  # Calculate total net worth

  # Reorder variables for output
  out <- out[c("opt", "yr", "ins", "rev.calf", "rev.ins", "rev.int", "rev.tot",
               "cost.op", "cost.ins", "cost.int", "cost.tot", "profit", "taxes",
               "aftax.inc", "cap.sales", "cap.purch", "cap.taxes", "assets.cow",
               "assets.cash", "net.wrth")]
  out
}

sim_run <- function(pars) {
  load("data/insurance_base.RData")
  stopifnot(is.list(pars))
  attach(pars)
  on.exit(detach(pars))

  # Calculate No-Drought Revenues from Calf Sales (aka base sales)
  base.sales <- unlist(lapply(1:t,function(i){
    CalculateExpSales(herd = herd, calf.sell = calf.sell, wn.wt = expected.wn.wt, p.wn = p.wn[i])
  }))

  # Calculate No-Drought Operating Costs
  base.op.cost = CalculateBaseOpCosts(herd = herd, cow.cost = cow.cost)

  # Compute insurance premiums and indemnities
  if (purchase.insurance == 1){
    rma.ins = insMat(tgrd = tgrd, yyr = yyr, clv = clv, acres = acres,
                     pfactor = pfactor, insPurchase  =  insp)
  }else{ # if purchase.insurance set to 0 (no insurance), simply set prem/indem = 0
    rma.ins = cbind(yyr[1]:yyr[1]+(t-1),matrix(0,t,2))
  }

  # Base Cow Assets: No sell/replace
  base.assets.cow <- CalcCowAssets(t = t, herd = herd, p.cow = p.cow)

  c(base.cap.sales, base.cap.purch) := CalcCapSalesPurch(assets.cow = base.assets.cow,
                                                         t=t,
                                                         cull.num = cull.num,
                                                         p.cow = p.cow)

  base.cap.taxes <- CalcCapTaxes(cap.sales = base.cap.sales,
                                 cap.purch = base.cap.purch,
                                 cap.tax.rate = cap.tax.rate,
                                 herd = herd,
                                 p.cow = p.cow)

  ####No Drought####

  out.nodrght <- OptionOutput(t = t,
                              opt = "nodrght",
                              nodrought = TRUE,
                              rev.calf = base.sales,
                              cost.op = rep(base.op.cost,t),
                              rma.ins = rma.ins,
                              int.invst = invst.int,
                              int.loan = loan.int,
                              start.cash = 0,
                              assets.cow = base.assets.cow,
                              cap.sales = base.cap.sales,
                              cap.purch = base.cap.purch,
                              cap.taxes = base.cap.taxes)

  ####Drought Occurs####
  # For each option, we calculate the **CHANGE** in costs
  # and the **CHANGE** in revenues relative to the no drought baseline.

  # Calculate vector of days of drought adaptation action for each year
  forage.potential <- sapply(yyr, foragePWt, stgg = stgg, zonewt = zonewt, stzone = stzone)
  drght.act.adj <- CalculateAdaptationIntensity(forage.potential)
  days.act <- CalculateDaysAction(act.st.yr, act.st.m, act.end.yr, act.end.m, drought.action) * drght.act.adj  # adjusts the days of action by the severity of drought

  ## Option 0: No adaptation ##
  # drought revenues
  noadpt.rev.calf <- unlist(lapply(1:t, function(i){
    CalculateExpSales(herd = herd, calf.sell = calf.sell, wn.wt = wn.wt[i], p.wn = p.wn[i])
  }))

  out.noadpt <- OptionOutput(t = t,
                             opt = "noadpt",
                             rev.calf = noadpt.rev.calf,
                             cost.op = rep(base.op.cost,t),
                             rma.ins = rma.ins,
                             int.invst = invst.int,
                             int.loan = loan.int,
                             start.cash = 0,
                             assets.cow = base.assets.cow,
                             cap.sales = base.cap.sales,
                             cap.purch = base.cap.purch,
                             cap.taxes = base.cap.taxes)


  ## Option 1: Buy additional feed
  days.feed <- days.act  # Assumes that feeding days are equivalent to drought adaptation action days

  # Calculate operating costs including costs to buy feed
  feed.cost <- CalculateFeedCost(kHayLbs, kOthLbs, p.hay, p.oth, days.feed, herd) + base.op.cost

  out.feed <- OptionOutput(t = t,
                           opt = "feed",
                           rev.calf = base.sales,
                           cost.op = feed.cost,
                           rma.ins = rma.ins,
                           int.invst = invst.int,
                           int.loan = loan.int,
                           start.cash = 0,
                           assets.cow = base.assets.cow,
                           cap.sales = base.cap.sales,
                           cap.purch = base.cap.purch,
                           cap.taxes = base.cap.taxes)

  ## Option 2: Truck pairs to rented pasture
  days.rent <- days.act # Assumes that pasture rental days are equivalent to drought adaptation action days

  # Calculate calf revenues in drought after trucking pairs to rented pasture
  calf.rev.rentpast <- CalculateRentPastRevenue(expected.wn.wt = expected.wn.wt,
                                                calf.loss = calf.loss,
                                                calf.wt.adj = calf.wt.adj,
                                                calf.sell = calf.sell,
                                                herd = herd,
                                                p.wn = p.wn)

  # Calculate operating costs to truck pairs to rented pasture. Assumes base operating cost is unchanged.
  cost.op.rentpast <- CalculateRentPastCost(n.miles = n.miles,
                                            truck.cost = truck.cost,
                                            past.rent = past.rent,
                                            days.rent = days.rent,
                                            oth.cost = oth.cost,
                                            max.wt = max.wt,
                                            cow.wt = cow.wt,
                                            calf.wt = calf.wt,
                                            herd = herd) + base.op.cost

  out.rentpast <- OptionOutput(t = t,
                               opt = "rentpast",
                               rev.calf = calf.rev.rentpast,
                               cost.op = cost.op.rentpast,
                               rma.ins = rma.ins,
                               int.invst = invst.int,
                               int.loan = loan.int,
                               start.cash = 0,
                               assets.cow = base.assets.cow,
                               cap.sales = base.cap.sales,
                               cap.purch = base.cap.purch,
                               cap.taxes = base.cap.taxes)

  ## Option 3: Sell pairs and replace cows

  calf.rev.sellprs <- CalculateSellPrsRev(base.sales = base.sales,
                                          herd = herd,
                                          wn.succ = wn.succ,
                                          calf.wt = calf.wt,
                                          p.calf.t0 = p.calf.t0)

  cost.op.sellprs <- CalculateSellPrsCost(op.cost.adj = op.cost.adj,
                                          herd = herd,
                                          sell.cost = sell.cost,
                                          base.op.cost = base.op.cost,
                                          herdless.op.cost = herdless.op.cost)

  assets.cow.sellprs <- CalcCowAssets(herd = herd,
                                      p.cow = p.cow,
                                      sell.year = 1,
                                      replace.year = 3)

  c(cap.sales, cap.purch) := CalcCapSalesPurch(assets.cow = assets.cow.sellprs,
                                               t=t,
                                               cull.num = cull.num,
                                               p.cow = p.cow)

  cap.taxes <- CalcCapTaxes(cap.sales = cap.sales,
                            cap.purch = cap.purch,
                            cap.tax.rate = cap.tax.rate,
                            herd = herd,
                            p.cow = p.cow)

  out.sellprs <- OptionOutput(t = t,
                              opt = "sellprs",
                              rev.calf = calf.rev.sellprs,
                              cost.op = cost.op.sellprs,
                              rma.ins = rma.ins,
                              int.invst = invst.int,
                              int.loan = loan.int,
                              start.cash = 0,
                              assets.cow = assets.cow.sellprs,
                              cap.sales = cap.sales,
                              cap.purch = cap.purch,
                              cap.taxes = cap.taxes)

  ## Option 4: Sell pairs and don't replace

  calf.rev.sellprs.norepl <- c(calf.rev.sellprs[1],rep(0,(t-1)))

  cost.op.sellprs.norepl <- c(cost.op.sellprs[1],rep(herdless.op.cost,(t-1)))

  assets.cow.sellprs.norepl <- CalcCowAssets(t = t,
                                             herd = herd,
                                             p.cow = p.cow,
                                             sell.year = 1)

  c(cap.sales, cap.purch) := CalcCapSalesPurch(assets.cow = assets.cow.sellprs.norepl,
                                               t=t,
                                               cull.num = cull.num,
                                               p.cow = p.cow)

  cap.taxes <- CalcCapTaxes(cap.sales = cap.sales,
                            cap.purch = cap.purch,
                            cap.tax.rate = cap.tax.rate,
                            herd = herd,
                            p.cow = p.cow)

  out.sellprs.norepl <- OptionOutput(t = t,
                                     opt = "sellprs.norepl",
                                     rev.calf = calf.rev.sellprs.norepl,
                                     cost.op = cost.op.sellprs.norepl,
                                     rma.ins = rma.ins,
                                     int.invst = invst.int,
                                     int.loan = loan.int,
                                     start.cash = 0,
                                     assets.cow = assets.cow.sellprs.norepl,
                                     cap.sales = cap.sales,
                                     cap.purch = cap.purch,
                                     cap.taxes = cap.taxes)

  ## Bringing outcome df's from each option together
  outcomes <- rbind(out.nodrght, out.noadpt, out.feed, out.rentpast, out.sellprs, out.sellprs.norepl)
  outcomes$opt=as.character(outcomes$opt)
  outcomes$sim.index <- rep(sim.index, nrow(outcomes))
  outcomes
}



# Utility Functions -------------------------------------------------------

':=' <- function(lhs, rhs) {
  # Description: Magical function that allows you to return more than one variable
  #  from other functions.
  # Code from http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value

  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL))
}

 
act.rainfall.adjustment <- function(f) {
  forage.potential <- foragePWt(stgg, zonewt, stzone, styear)
}

