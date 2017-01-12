

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



dcInfo <- function(dc, tgrd){
  
  "
  Author: Joe
  
  Extracts drought calculator info from a 
  grid cell. 

  dc: 'droughtCalculator' output (class LIST)

  tgrd: target grid cell ID
  "
  
  dcinf = lapply(dc, function(X){
    
    extract(X, rastPt[rastPt$layer == tgrd,])
    
  })
  
  return(dcinf)
  
}




foragePWt<-function(stgg,zonewt,stzone,styear,decision=F){
  
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
  zonewt=zonewt[stzone,] # subset weights by station/grid zone
  yprecip=stgg[stgg[,1]==styear,][,-1] # monthly precip amounts for start year
  # ave=stgg[stgg$Year=="AVE",][,-1] # average precip since 1948
  ave=stgg[nrow(stgg),][,-1] # average precip since 1948
  pidx=yprecip/ave # Monthly precip "index"
    
  if(decision){ #"decision making under uncertainty" mode
  
    ## Group years in period of record by monthly precip
    cper=stgg[1:which(stgg$Year==2015),] # subset by period 1948-2015
    cper_clust=pamk(cper[,-1]) # Find optimal groups of years, k = 2-10
    yy_group=cper_clust[[1]]$clustering[which(cper$Year==styear)] # Group membership for target year
    yy_ave=colMeans(cper[which(cper_clust[[1]]$clustering==yy_group),][,-1]) # Group mean vector
    yidx=yy_ave/ave # Expected index values for year (group mean vector / long-term average)
    
    # Generate forage potential weights 
    # not sure why the rows are subsetting as lists!
    foragewt=unlist(c((zonewt*pidx)[1:dr_start],(zonewt*yidx)[(dr_start+1):12]))
  
  }else{ #default: long-term precip & zone weights
    
    foragewt=zonewt*pidx
    
  }
  
  # Compute annual forage potential weight for zone
  stfwt=sum(foragewt) 
  
  return(stfwt)
  
}

calfDroughtWeight<-function(expected.wn.wt,calf.wt,stfwt){
  return(calf.wt+(stfwt*(expected.wn.wt-calf.wt)))
}

calfWeanWeight<-function(styr){
  
  "
  Compute calf weights based on station/grid cell 
  forage potential for a five-year period.
  "
  
  if(!exists("station.gauge",envir=globalenv())){
    stop("Station gauge information is required.")
  }
  
  if(!exists("constvars",envir=globalenv())){
    stop("Constant variable information is required.")
  }
  
  attach(station.gauge)
  attach(constvars)
  forage.weights=unlist(lapply(seq(styr,styr+4),function(i){
    foragePWt(stgg,zonewt,stzone,i)
  }))
  calf_weights_ann=unlist(lapply(forage.weights,function(i){ # annual calf weights
    calfDroughtWeight(expected.wn.wt,calf.wt,i)
  }))
  detach(station.gauge)
  detach(constvars)
  
  calf_weights_ann
}


':=' <- function(lhs, rhs) {
  # Decription: Magical function that allows you to return more than one variable
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
  
  coop.pt=SpatialPoints(t(rev(coop$loc)),proj4string=CRS(proj4string(mlra)))
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


getStationGauge<-function(target.loc="CPER"){
  
  # clear station gauge environment if previously written
  if(exists("station.gauge",envir = globalenv())){
    rm("station.gauge",envir=globalenv())
  }
  
  if(target.loc=="CPER"){ # Use COOP sites or CPER: Default to CPER
    
    ## Zone Weights
    stzone=3 # state forage zone
    # multiple operations since reading from
    # external file that may be replaced
    zonewt=read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx",sheet="Drought Calculator",skip = 5)[5:8,]
    zonewt=sapply(data.frame(zonewt[,which(names(zonewt)=="Jan"):which(names(zonewt)=="Dec")]),as.numeric)
    
    ## Station precip gauge 
    stgg=data.frame(read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx","CPER Precip",skip = 1))
    stgg=stgg[,-which(names(stgg) %in% c("TOTAL","Var.15"))]
    stgg=stgg[stgg$Year %in% c(1948:2016,"AVE"),]
    
    ## Target grid cell
    tgrd = 25002  # target grid cell - CPER default 
    
  }else{ #Custom location specified (COOP site and MLRA forage potential weights)
    
    ## Fetch data
    wrc.state="co" # For pulling COOP sites & mlra forage weights
    load("data/coops.RData") # Shortcut for sourcing 'R/coop_scraper.R'
    # source("R/coop_scraper.R") # the long way
    mlra=readOGR("data","mlra_v42") # load MLRA zone data
    target.coop=coops[[which(names(coops)==target.loc)]]
    
    ## Zone weights
    mlra.idx=COOP_in_MRLA(target.coop) # MLRA index
    zonewt=getMRLAWeights(wrc.state) # zone weights
    stzone=which(zonewt[,1]==mlra.idx) # not a great workaround...should fix 'foragePwt' function instead
    zonewt=zonewt[,-1] # not a great workaround...should fix 'foragePwt' function instead
    
    ## Station precip gauge
    stgg=target.coop$precip
    stgg=rbind(stgg,rep(NA,ncol(stgg)))
    stgg[nrow(stgg),][,1]="AVE"
    stgg[nrow(stgg),][,-1]=colMeans(stgg[-nrow(stgg),][,-1],na.rm=T)
    
    ## Target grid cell
    tgrd = target.coop$grid  # target grid cell - custom site
    
  }  
  
  # Write vars to new env
  station.gauge<<-new.env()
  assign("zonewt",zonewt,envir=station.gauge)
  assign("stzone",stzone,envir=station.gauge)
  assign("stgg",stgg,envir=station.gauge)
  assign("tgrd",tgrd,envir=station.gauge)
  
  # SpatialPoints representation of target gridcell 
  # for fetching insurance results
  assign("tgrd_pt",rastPt[rastPt@data$layer == tgrd, ],envir=station.gauge)  
  
}