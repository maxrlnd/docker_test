# Collecting unused functions

noIns <- function(){
  purchaseInsurance <<- FALSE
  indem <<- lapply(indem, function(x){
    x[, c("producer_prem", "indemnity", "full_prem") := 0]
    return(x)
  })
  indemprac <<- lapply(indemprac, function(x){
    x[, c("producer_prem", "indemnity", "full_prem") := 0]
    return(x)
  })
}