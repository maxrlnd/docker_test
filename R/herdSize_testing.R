# Testing Herd Size
# Final funtion is in calfCowFunctions 

getHerdSize <- function(results_1ya, results_2ya, deathRate){
  currentHerd <- (results_1ya$herd * (1 - deathRate) * 
                    (1 - results_1ya$cows.culled) + 
                    (results_2ya$herd * results_2ya$wn.succ) *
                    (1 - results_2ya$calves.sold) * (1 - deathRate))
  return(currentHerd)
}

results_1ya <- data.frame(herd = 600, cows.culled = .16, wn.succ = 0.88, calves.sold = .77)
results_2ya <- data.frame(herd = 600, cows.culled = .16, wn.succ = 0.88, calves.sold = .77)

getHerdSize(results_1ya, results_2ya, deathRate = .04)
# Death loss source: http://usda.mannlib.cornell.edu/usda/current/CattDeath/CattDeath-05-12-2011.pdf
