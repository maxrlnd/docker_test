library(data.table)
library(ggplot2)

newHerd <- function(){
  herdFrame <- data.table(matrix(0, ncol = 6, nrow = 30))
  setnames(herdFrame, c("Year", "Herd", "Culled_Cows", 
                        "Calves_Weaned", "Calves_Sold", "Cows_Dying"))
  return(herdFrame)
}

getCowStats <- function(herd){
  cull <- herd * input$cull
  wean <- herd * input$wean
  dead <- herd * input$deathRate
  sold <- wean * input$calveSold
  return(list(cull, wean, sold, dead))
}