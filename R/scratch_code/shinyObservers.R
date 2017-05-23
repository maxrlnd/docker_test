loadObservers <- function(input, output, session, i, rv){
  # Reactive to disable start simulation button after they're clicked
  observeEvent(input[[paste0("year", i, "Start")]], {
    shinyjs::disable(paste0("year", i, "Start"))
    delay(100,session$sendCustomMessage(type = "scrollCallbackRain", paste0("rainGraph", i)))
  })
  
  ## Disable cow and calf sliders after sell button
  ## Disable sell button
  ## update myOuts based on forage and the year's decisions
  observeEvent(input[[paste0("sell", i)]], {
    disable(paste0("sell", i))
    disable(paste0("calves", i, "Sale"))
    disable(paste0("cow", i, "Sale"))
    myOuts <<- updateOuts(wean = AdjWeanSuccess(get(paste0("totalForage", i))(), T, simRuns$normal.wn.succ, 1), 
                          totalForage = get(paste0("totalForage", i))(), calfSale = input[[paste0("calves", i, "Sale")]],
                          indem = indem[[i]], adaptExpend = input[[paste0("d", i, "adaptExpend")]], cowSales = input[[paste0("cow", i, "Sale")]], 
                          newHerd = get(paste0("herdSize", i))(), zones = get(paste0("currentZones", i))(), 
                          currentYear = i, ID = ID, time = startTime)
  })
  
  
  # Disable continue button and adaptation slider after clicking
  observeEvent(input[[paste0("year", i, "Summer")]], {
    shinyjs::disable(paste0("year", i, "Summer"))
    shinyjs::disable(paste0("d", i, "
                                "))
    delay(100,session$sendCustomMessage(type = "scrollCallbackIns", paste0("rainGraphSep", i)))
  })
  
  observeEvent(input[[paste0("insCont", i)]], {
    shinyjs::disable(paste0("insCont", i))
    delay(100,session$sendCustomMessage(type = "scrollCallbackCow", paste0("cowSell", i)))
  })
}