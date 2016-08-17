"
Scratchpad for development of 'optimal'
range insurance purchase allocation. Ranchers
are assumed to choose the interval featuring the
months with highest combined weight, followed by
the next-highest weighted non-consecutive 
interval. 

For now 50% protection will be allocated
to each interval. This should be refined so that
protection amounts are proportional to each weight.

Support for choosing up to 5 intervals should 
also be incorporated here.

Should be merged with the insurance prep 
section in 'vars.R' when developed further.
"

target.loc="BOULDER, COLORADO"
source("R/support_functions.R")
source("R/vars.R")

fpwt=as.numeric(zonewt[stzone,]) # our forage potential weight (e.g. for MLRA 49)

# Bin forage potential weights into two-month intervals by mean
fpwt_iv=c()
for(m in 1:11){
  
  fpwt_iv=c(fpwt_iv,(sum(fpwt[m],fpwt[m+1])/2)) # need to calc mean manually - not sure why
  names(fpwt_iv[m])=paste0("i",m)
  
}

####Insurance Purchase Options####

##OPTION 1 - RANKING##
# This one is very simple to implement when just two intervals are insured. 
# Ideally, the interval with the 2nd highest weights *does not* overlap
# with our top-weighted interval. If it does, the rancher chooses the 
# 3rd most highly-ranked interval instead -- unless that interval *also*
# overlaps our top-ranked interval, in which case the rancher settles on
# interval ranked 4 (this occurs in our Boulder example).
# 
# This could be extended to a simple algorithm that performs this process
# n-1 times with a rank shift for each secondary interval chosen. Eventually
# this might converge when additional choices are exhausted 

fpwt_iv_rank=rank(-fpwt_iv) # rank interval weights descending
names(fpwt_iv_rank)=paste0("i",1:11)
top_iv=which.min(fpwt_iv_rank) # index of top-ranked interval (i6, Jun-Jul)
adj_iv_rank=fpwt_iv_rank[c((top_iv-1),(top_iv+1))] # adjacent intervals to top rank

# is next rank (2) in either interval adjacent to 'top_iv'?
# if not, we can simply choose wherever interval rank 2 occurs 
2 %in% adj_iv_rank

# in this case, rank two is adjacent (in interval 7), 
# so now we need to check whether rank 3 also occurs in
# a consecutive interval
3 %in% adj_iv_rank

# Rank 3 is also in an interval adjacent to rank 1 (i5), 
# so now we should look for wherever rank 4 occurs as
# our second interval
sec_iv=which(fpwt_iv_rank==4) # i4, Apr-May

# alternatively...
if(min(adj_iv_rank)==2){
  if(3 %in% adj_iv_rank){
    sec_iv=which(fpwt_iv_rank==4)
  }else{
    sec_iv=which(fpwt_iv_rank==3)
  }
}else{
  sec_iv=which(fpwt_iv_rank==2)
}

# even better!
cand_iv=fpwt_iv_rank[-c((top_iv-1):(top_iv+1))] # candidate secondary intervals
sec_iv=as.numeric(substr(names(cand_iv[which.min(cand_iv)]),2,2)) # grab interval index from iv name

fpwt_iv[c(top_iv,sec_iv)] # inspect the weights

# we can even repeat that last technique
# for additional allocation intervals
cand_iv=fpwt_iv_rank[-unique(c((top_iv-1):(top_iv+1),(sec_iv-1):(sec_iv+1)))] # candidate tertiary intervals
thr_iv=as.numeric(substr(names(cand_iv[which.min(cand_iv)]),2,2)) # grab interval index from iv name

wt_iv=c(top_iv,sec_iv,thr_iv) # Jun-Jul, Apr-May, Aug-Sep
wt_choice=fpwt_iv[wt_iv] # inspect the weights
wt_choice # inspect the weights

## Convert weights to allocation amounts
wt_alloc=wt_choice/sum(wt_choice)

# or, if we wanted (or needed) to scale
# to a maximum allocation amount...
max_alloc=0.6
blah=c(0.7,0.2,0.1) # some toy allocation amts not valid for the model
blah[1]=blah[1]*(max_alloc/blah[1])
blah[2:3]=(blah[2:3]/sum(blah[2:3]))*(1-max_alloc)
blah

# with the actual weights
wt_alloc_scaled=wt_alloc
wt_alloc_scaled[1]=wt_alloc_scaled[1]*(max_alloc/wt_alloc_scaled[1])
wt_alloc_scaled[2:3]=(wt_alloc_scaled[2:3]/sum(wt_alloc_scaled[2:3]))*(1-max_alloc)
wt_alloc_scaled

# Final insurance purchase input 
cbind(wt_iv,round(wt_alloc,2))
cbind(wt_iv,round(wt_alloc_scaled,2)) # scaled for max allocation


##OPTION 2 - HIGHEST WEIGHTED INTERVAL SETS##
# This approach finds sets of n 
# non-consecutive intervals with the highest 
# overall (mean) weights. 
#
# However, a rancher making these choices might fail to 
# insure the interval with highest mean forage potential. 
# In this case, the optimal choice for two intervals
# is to insure intervals 5 (May-Jun) and 7 (Jul-Aug), 
# which are both highly weighted, but not quite as high 
# as the interval they bound (Jun-July).

niv=3 # number of intervals to insure 

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
           (sum(fpwt_iv[iv_comb[,i]])/2))
  
}

# Select best intervals
# with 3 chosen this is identical to option 1
wt_iv=iv_comb[,which.max(combwt)]
wt_choice=fpwt_iv[wt_iv]
cat(wt_iv,wt_choice,sep="\n")

####Write Options to Functions####

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

## Option 1 - By Rank
insuranceSelect_opt1<-function(fpwt,niv=2){
  
  fpwt_iv=forageWeights2Intervals(fpwt) # bin forage potential weights into intervals
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
  
  wt_choice=fpwt_iv[wt_iv] # get weight values
  
  return(cbind(wt_iv,wt_choice))
  
}

insuranceSelect_opt1(fpwt,2) # seems to work


## Option 2 - By Combo
insuranceSelect_opt2<-function(fpwt,niv=2){
  
  fpwt_iv=forageWeights2Intervals(fpwt) # bin forage potential weights into intervals
  
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
             (sum(fpwt_iv[iv_comb[,i]])/2))
    
  }
  
  # Select best intervals
  wt_iv=iv_comb[,which.max(combwt)]
  wt_choice=fpwt_iv[wt_iv]
  
  return(cbind(wt_iv,wt_choice))
  
}

insuranceSelect_opt2(fpwt,2) # seems to work
