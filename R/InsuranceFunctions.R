
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

#***Ohhh boy this needs work... 

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
  fiveYears = data.table(matrix(0, length(yyr), 3))  # empty matrix - year, indemnity, producer premium x number years
  setnames(fiveYears, c("year", "producer_prem", "indemnity"))
  fiveYears[,year := seq(yyr[1], yyr[length(yyr)])]  # populate years in first column
  #**PARALLELIZE THIS??**#
  #Probably not
  for(yy in yyr){
    
    ins_info <-  c(yy,
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
    fiveYears[year == yy, c("producer_prem", "indemnity") := as.list(ins_info[2:3])]
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

