
# Insurance Functions -----------------------------------------------------

insMat <- function(yy, clv, acres, pfactor, insPurchase, tgrd){
  "
  Author: Adam (based loosely on Joe's work)
  
  Calculates premium and indemification for a specific year and
  grid cell. Currently returns are summed bu this could be done
  on a index interval basis instead.
  
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
  Returns a data.table of outputs:
  
  year: year of calculations
  prod_prem: producer premium
  indemnity: total indemnity
  full_prem: premium without subsidy
  
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

  ##Get subsidy rate based on coverage level
  sbdy <- covsub[coverage.trigger == clv, subsidy.rate]
  
  ##Set up insurance purchase vector
  ip = rep(0, 11)
  ip[insPurchase[, 1]] = insPurchase[, 2]  # replaces 0's with interval allocations
  insPurchase = ip
  
  ## Make sure interval selecitons are eligible for insurance
  if(min(as.numeric(grid_elig[grid == tgrd, -"grid"]) - insPurchase) < 0){
    stop("Ineligible index intervals for this grid cell selected")
  }
  
  ##Calculate policy rate
  plrt = prod(clv, acres, pfactor) * basePrice[grid == tgrd, value]  # why use prod() instead of multiplying?
  
  ## Calculate the protection for each index interval
  monthProtec <- plrt * insPurchase
  
  ## Calculate Premium for each Month
  monthPrem <- (monthProtec * .01) * prem100[grid == tgrd, -c("grid", "state", "county")]
  
  ## Calculate Premium subsidy for each month
  subSidy <- round(monthPrem * sbdy, 2)
  
  ## Calculate subsidised premium for each month
  subPrem <- round(monthPrem * (1-sbdy), 2)
  
  ## Fetch index intervals
  intervalIndicies <- data.table(t(intervalNOAA[Year == yy & grid == tgrd, value]))
  setnames(intervalIndicies, names(subPrem))
  
  ## Calcualte the percent coverage for each month
  ## Rounding can make a significant difference here RMA uses 3 decimals
  coverageAmount <- round(clv - (intervalIndicies * .01), 3)
  
  ## Calculate Indemnities
  indem <- unlist(ifelse(coverageAmount > 0, coverageAmount/clv * monthProtec, 0))

  returnTable = data.table(matrix(nrow = 1, ncol = 4, data = c(yy, sum(subPrem), sum(indem), sum(monthPrem))))  # empty matrix - year, indemnity, producer premium x number years
  setnames(returnTable, c("year", "producer_prem", "indemnity", "full_prem"))
  
  return(returnTable)
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

