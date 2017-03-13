
# Forage Functions -------------------------------------------------------

foragePWt <- function(station.gauge, styear, herd, carryingCap,
                      decision = FALSE, decisionMonth = 5, farmYearStart = 11){
  
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
  
  yprecip <- station.gauge$stgg[Year %in% (styear-1):styear, ]  # monthly precip amounts for start year
  zonewt <- station.gauge$zonewt
  yprecip <- cbind((yprecip[Year == styear - 1, c("NOV", "DEC")]), 
                   (yprecip[Year == styear, -c("NOV", "DEC", "Year")]))
  ave <- station.gauge$avg
  yearAvg <- rbindlist(list(yprecip, ave), use.names = T)
  
  
  
  ##****This is an interesting approach does it work? CV results needed
  # Also the year we're working with should be removed
  if(decision){ #"decision making under uncertainty" mode
    
    ## Group years in period of record by monthly precip
    #****Do we want to change the way year is done  here?
    #****This method gives very high forage potentials I might have messed something up
    
    # cper <- station.gauge$stgg[1:which(Year == 2015),] # subset by period 1948-2015
    # cper_clust <- pamk(cper[, -1]) # Find optimal groups of years, k = 2-10
    # yy_group <- cper_clust[[1]]$clustering[which(cper$Year == styear)] # Group membership for target year
    # yy_ave <- colMeans(cper[which(cper_clust[[1]]$clustering == yy_group), ][, -1]) # Group mean vector
    # yy_ave[c(1:decisionMonth - 1)] <- yprecip[c(1:decisionMonth-1)]
    # yidx <- yy_ave/ave # Expected index values for year (group mean vector / long-term average)
    # Generate forage potential weights
    # not sure why the rows are subsetting as lists!
    # foragewt <- unlist(c((zonewt * pidx), (zonewt * yidx)))
    
    # Replace Months forage in unknown months with the average
    
    yearAvg[1, (names(ave)[decisionMonth:(farmYearStart -1)]) := ave[,decisionMonth:(farmYearStart -1)]] 
  }
  
  # Monthly precip "index"
  pidx  <- yearAvg[1,] / yearAvg[2,] 
  
  #Compute Forage Weight Potentials
  foragewt = zonewt * pidx[, names(ave), with = F]
    
  
  # Compute annual forage potential weight for zone
  forage.potential <- sum(foragewt)
  
  ## Adjust for carying Capacity
  forage.potential <-forage.potential/carryingCap
  
  return(forage.potential)
  
}

whatIfForage <- function(station.gauge, zonewt, styear, herd, carryingCap,
                         currentMonth, farmYearStart = 11, expectedFuture){
  yprecip <- station.gauge$stgg[Year %in% (styear-1):styear, ]  # monthly precip amounts for start year
  yprecip <- cbind((yprecip[Year == styear - 1, c("NOV", "DEC")]), 
                   (yprecip[Year == styear, -c("NOV", "DEC", "Year")]))
  ave <- station.gauge$avg
  yearAvg <- rbindlist(list(yprecip, ave), use.names = T)
  
  if(expectedFuture == "normal"){
    yearAvg[1, (names(ave)[currentMonth:(farmYearStart -1)]) := ave[,currentMonth:(farmYearStart -1)]]
  }
  if(expectedFuture == "low"){
    yearAvg[1, (names(ave)[currentMonth:(farmYearStart -1)]) := ave[,currentMonth:(farmYearStart -1)] * .5]
  }
  if(expectedFuture == "high"){
    yearAvg[1, (names(ave)[currentMonth:(farmYearStart -1)]) := ave[,currentMonth:(farmYearStart -1)] * 1.5]
  }
  
  # Monthly precip "index"
  pidx  <- yearAvg[1,] / yearAvg[2,] 
  
  #Compute Forage Weight Potentials
  foragewt = zonewt * pidx[, names(ave), with = F]
  
  
  # Compute annual forage potential weight for zone
  forage.potential <- sum(foragewt)
  
  ## Adjust for carying Capacity
  forage.potential <-forage.potential/carryingCap
  
  return(forage.potential)
  
  
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

