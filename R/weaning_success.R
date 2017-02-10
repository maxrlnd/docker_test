AdjWeanSuccess <- function(forage.potential, noadpt = FALSE, normal.wn.succ, t) {
    # Description: Adusts weaning success downward for the year of the drought and the following year
    # NOTE: This equation is based on what I consider to be "reasonable" estimates
    #  of weaning success based on forage potential. We need to find a source
    #  that gives a better idea of the relationship
    
    wn.succ <- NULL
    
      if(noadpt == FALSE | forage.potential >= 1) {
          wn.succ <- rep(normal.wn.succ, t)
        }
      if(noadpt == TRUE & forage.potential < 1) {
        if(t > 1){
          wn.succ[1] <- normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential)*2))) 
          wn.succ[2] <- normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential))))
          wn.succ[3:t] <- normal.wn.succ                                
        }else{
          wn.succ <- normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential)*2))) 
        }
      }
    return(wn.succ)
  }  