AdjWeanSuccess <- function(stgg, zonewt, stzone, styear, noadpt = FALSE, normal.wn.succ, t) {
    # Description: Adusts weaning success downward for the year of the drought and the following year
    # NOTE: This equation is based on what I consider to be "reasonable" estimates
    #  of weaning success based on forage potential. We need to find a source
    #  that gives a better idea of the relationship
    
    forage.potential <- foragePWt(stgg, zonewt, stzone, styear)
    wn.succ <- NULL
    
      if(noadpt == FALSE) {
          wn.succ <- rep(normal.wn.succ, t)
        }
      if(noadpt == TRUE & forage.potential < 1) {
        wn.succ[1] <- normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential)*2))) 
        wn.succ[2] <- normal.wn.succ * (1 / (1 + exp(-(1 + forage.potential))))
        wn.succ[3:t] <- normal.wn.succ                                
      }
    wn.succ
    }  