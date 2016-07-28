# Load Libraries
library(pracma)
library("tseries")
require(lubridate)
require(dplyr)
library(raster)
library("ncdf4")
library("rgdal")
library("fields")
library(data.table)
library(spdep)
library(ggplot2)
library("rasterVis")
library("RSelenium")
library("trend")
library(rgeos)
library("maptools")
library(rgdal)
library(readxl)
library(fpc)

load("RanchDrought_base.RData") # Load base grid and insurance data

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

droughtCalculator<-function(yy,clv,acres,pfactor,insPurchase,mask=NULL){
  
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
  if(!yy %in% 2000:2015){
    stop("Input year must occur in the range 2000-2015.")
  }
  # if(!clv %in% round(seq(0.7,0.9,by=0.05),1)){
  #   stop("Invalid coverage level. Accepted coverage levels are 0.7, 0.75, 0.8, 0.85, 0.9.")
  # }
  # if(!pfactor %in% round(seq(0.6:1.5,by=0.01),1)){
  #   stop("Productivity factor must range from 0.6-1.5, in increments of 0.01.")
  # }
  if(min(dist(insPurchase[,1]))==1){
    stop("Insurance allocation for consecutive intervals is not permitted.")
  }
  if(nrow(insPurchase)<=1){
    stop("Insurance must be allocated for at least two intervals.")
  }
  if(prod(insPurchase[,1] %in% 1:11)==0){
    stop("Insurance allocation intervals must range from 1-11.")
  }
  if(max(insPurchase[,2])>0.6){
    stop("Insurance allocation may not exceed 60% per interval.")
  }
  if(sum(insPurchase[,2])!=1){
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
  sbdy=covsub[coverage.trigger==clv,subsidy.rate]
  
  ##Set up insurance purchase vector
  ip=rep(0,11)
  ip[insPurchase[,1]]=insPurchase[,2]
  insPurchase=ip
  
  ##Calculate policy rate
  plrt=prod(clv,acres,pfactor)*basePrice
  
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

dcInfo<-function(dc,tgrd){
  
  "
  Author: Joe
  
  Extracts drought calculator info from a 
  grid cell. 

  dc: 'droughtCalculator' output (class LIST)

  tgrd: target grid cell ID
  "
  
  dcinf=lapply(dc,function(X){
    
    extract(X,rastPt[rastPt$layer==tgrd,])
    
  })
  
  return(dcinf)
  
}
