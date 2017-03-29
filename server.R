
function(input, output, session) {
  toggleClass(class = "disabled",
              selector = "#navBar li a[data-value=Demographics]")

  ## Dynamic UI for Demo graphics
  output$exp <- renderUI({
    if(input$experience == "Yes"){
      textInput("expExplain", "Please explain your previous ranch experience")
    }
  })
  
  #####Year Tab Functions#####################
  
  ## This loop Creates the necessary output functions for each year tab 
  lapply(1:simLength, function(i){
    
    ## Reactive taglist for the first set of winter info at the start of each year, updates when
    ## myOuts updates
    assign(paste0("reactiveWinter", i), reactive({
      input[[paste0("sell", i-1)]]
      tagList(
        h4("Winter Finance Assessment"),
        p(paste0("Your Current Herd has ", round(myOuts[i, herd], 0), " cows, not including calves or yearlings.")),
        p(paste0("Your Bank Balance is: $", round(myOuts[i, assets.cash], 0))),
        p(paste0("Your current net worth, including cows and your bank balance, is $", round(myOuts[i, net.wrth], 0), ".")),
        p(paste0("Your range is currently at: ", round(myOuts[i, forage.potential] * 100, 0), "%")),
        p(paste0("You paid: $", round(myOuts[i, cost.ins], 0), " for insurance"))
      )
    }))
    
    ## Creates a reactive to track the current zone weights for each year
    assign(paste0("currentZones", i), reactive({
      zones <- station.gauge$zonewt
      
      ## Code for the first year when the previous zones/GT haven't been determined
      if(i == 1){
        zones <- zones * (1 - (0)/simRuns$forage.constant)
        
      ## Code for all subsequent years
      }else{
        zones <- myOuts[i, zone.change] * zones * 
          (1 - (myOuts[i, Gt])/simRuns$forage.constant)
      }
      return(zones)
    }))
    
    ## Reactive to track forage fore ach year
    assign(paste0("effectiveForage", i), reactive({
      
      ## Establish current state
      myYear <- startYear + i - 1
      herd <- myOuts[i, herd]
      zones <- get(paste0("currentZones", i))()
      
      ## Calcualte available forage using Nov-Nov as a year
      forage <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 10, 11, "normal")
     
      ## Calcualte adaptation intensity based on forage
      adaptationInten <- CalculateAdaptationIntensity(forage)
      
      ## Calculate adaptation cost
      adaptationCost <-getAdaptCost(adpt_choice = "feed", pars = simRuns, 
                                    days.act = 180, current_herd = herd, intens.adj = adaptationInten)
      
      ## Calculate how much of the needed adaptation is being done
      adaptationPercent <- ifelse(adaptationCost == 0, 0, input[[paste0("d", i, "AdaptSpent")]]/adaptationCost)
      
      ## Output new forage
      forage <- (1 - forage) * adaptationPercent + forage 
    }))
    
    ## Reactive to track herd size for each year
    assign(paste0("herdSize", i), reactive({
      
      ## Get cows being sold based on slide position
      cows <- input[[paste0("cow", i, "Sale")]]
      
      ## Calculate herd size for the first year
      if(i == 1){
        herd <- myOuts[i, herd]
        shinyHerd(herd1 = herd, cull1 = cows, herd2 = herd, 
                  calves2 = herd * simRuns$normal.wn.succ * (1 - simRuns$calf.sell),
                  deathRate = simRuns$death.rate)
        
      ## Herd size for second year
      }else{
        herd <- myOuts[i, herd]
        herd2 <- myOuts[i - 1, herd]
        wean2 <- myOuts[i - 1, wn.succ]
        calvesSold <- myOuts[i - 1, calves.sold]
        
        shinyHerd(herd1 = herd, cull1 = cows, herd2 = herd2, 
                  calves2 = herd2 * wean2 * (1 - calvesSold),
                  deathRate = simRuns$death.rate)
      }
    }))
    
    #################UI Functions of Year Tabs###################
    
    ## UI for winter Info
    output[[paste0("winterInfo", i)]] <- renderUI({
         get(paste0("reactiveWinter", i))()
    })
    
    ## Display rain info up to July and allow user to choose adaptation level
    output[[paste0("decision", i)]] <- renderUI({
      if(input[[paste0("year", i, "Start")]] == 1){
        getJulyInfo(i)
      }
    })
    
    ## Display Update for insurance info
    output[[paste0("insuranceUpdate", i)]] <- renderUI({
      if(!is.null(input[[paste0("year", i, "Summer")]])){
        if(input[[paste0("year", i, "Summer")]]){
          currentIndem <- round(indem[[i]]$indemnity, 0)
          tagList(
            h4("Insurance Payout"),
            if(currentIndem > 0){
              p(paste0("You have received a check for $", currentIndem, " from your rain insurance policy."))
            }else{
              p("You did not recieve a check for your rain insurance policy")
            }
          )
        }
      }
    })
    
    
    ## Present options to sell cows
    output[[paste0("cowSell", i)]] <- renderUI({
      if(!is.null(input[[paste0("year", i, "Summer")]])){
        if(input[[paste0("year", i, "Summer")]] == 1){
          tagList(
            getCowSell(get(paste0("effectiveForage", i))(), AdjWeanSuccess(get(paste0("effectiveForage", i))(), T, simRuns$normal.wn.succ, 1), i),
            plotOutput(paste0("cowPlot", i))
          )
        }
      }
    })
    
    ## Create a button to continue after selecting adaptation level
    output[[paste0("continue", i)]] <- renderUI({
      if(!is.null(input[[paste0("year", i, "Start")]])){
        if(input[[paste0("year", i, "Start")]] == 1){
          actionButton(paste0("year", i, "Summer"), "Continue")
        }
      }
    })
    
    ## Create button to sell calves and cows once decisions are made
    ## Additionally moves simualation to the next year
    output[[paste0("sellButton", i)]] <- renderUI({
      if(!is.null(input[[paste0("year", i, "Summer")]])){
        if(input[[paste0("year", i, "Summer")]] == 1){
          actionButton(paste0("sell", i), "Sell Calves and Cows")
        }
      }
    })
    
    ## Table of rain for each July
    output[[paste0("julyRain", i)]] <- renderTable({
      julyRain <- station.gauge$stgg[Year == (startYear + i - 1),-1]/station.gauge$avg * 100
      julyRain[, 7:12 := "?"]
    })
    
    ## Bar graphs for herd size
    output[[paste0("cowPlot", i)]] <- renderPlot({
      if(!is.null(input[[paste0("year", i, "Summer")]])){
        if(input[[paste0("year", i, "Summer")]] == 1){
          cows <- input[[paste0("cow", i, "Sale")]]
          calves <- input[[paste0("calves", i, "Sale")]]
          herd <- myOuts[i, herd]
          herdy1 <- shinyHerd(herd1 = get(paste0("herdSize", i))(), cull1 = cows, herd2 = herd,
                              calves2 = (herd - calves), deathRate = simRuns$death.rate)
          years <- (startYear + i - 1):(startYear + i + 1)
          cows <- data.table("Year" = years, "Herd Size" = c(myOuts[i, herd],
                                                             get(paste0("herdSize", i))(), herdy1))
          ggplot(cows, aes(x = Year, y = `Herd Size`)) + geom_bar(stat = "identity")
        }
      }
    })
    
    ## Reactive to disable start simualation button after they're clicked
    observeEvent(input[[paste0("year", i, "Start")]], {
      shinyjs::disable(paste0("year", i, "Start"))
    })
    
    ## Disable cow and calf sliders after sell button
    ## Disable sell button
    ## update myOuts based on forage and the year's decisions
    observeEvent(input[[paste0("sell", i)]], {
      disable(paste0("sell", i))
      disable(paste0("calves", i, "Sale"))
      disable(paste0("cow", i, "Sale"))
      myOuts <<- updateOuts(wean = AdjWeanSuccess(get(paste0("effectiveForage", i))(), T, simRuns$normal.wn.succ, 1), 
                            forage = get(paste0("effectiveForage", i))(), calfSale = input$calves1Sale,
                            indem = indem[[i]], adaptCost = input$d1AdaptSpent, cowSales = input$cow1Sale, 
                            newHerd = get(paste0("herdSize", i))(), zones = get(paste0("currentZones", i))(), 
                            adaptInten = CalculateAdaptationIntensity(get(paste0("effectiveForage", i))()),
                            currentYear = i)
      print(myOuts)
      values$currentYear <- values$currentYear + 1
      addTabToTabset(createNewYr(values$currentYear), "mainPanels")
      session$sendCustomMessage("myCallbackHandler", as.character(values$currentYear))
    })
    
    ## Disable continue button and adaptation slider after clicking
    observeEvent(input[[paste0("year", i, "Summer")]], {
      shinyjs::disable(paste0("year", i, "Summer"))
      shinyjs::disable(paste0("d", i, "AdaptSpent"))
      
    })
    
    
  }) ##End of lapply
  
  
  ## Observer for begin button in demographis panel
  observeEvent(input$begin, {
    # addTabToTabset(createNewYr(1), "mainPanels")
    disable("begin")
    session$sendCustomMessage("myCallbackHandler", "1")
  })

  ########## Functions to Print out state information
  
  ## Code to disable demographic tab at start
  observe({
    toggleClass(selector = "#navbar li a[data-value=Demographics]")
  })

  ## Disable agree button on instructions after it has been clicked and move to
  ## Demographics tab
  observeEvent(input$agree, {
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value=Demographics]")
    session$sendCustomMessage("myCallbackHandler", "6")
  })
  
  ## Reactive value for current year
  values <- reactiveValues("currentYear" = 1)
  
  ## Code to dynamically add new tabs
  output$creationPool <- renderUI({})
  outputOptions(output, "creationPool", suspendWhenHidden = FALSE)
  addTabToTabset <- function(Panels, tabsetName){
    titles <- lapply(Panels, function(Panel){return(Panel$attribs$title)})
    Panels <- lapply(Panels, function(Panel){Panel$attribs$title <- NULL; return(Panel)})
    
    output$creationPool <- renderUI({Panels})
    session$sendCustomMessage(type = "addTabToTabset", message = list(titles = titles, tabsetName = tabsetName))
  }
  
  ## Commented code to add all tab panels at once currently not used as tab panels are added dynamically

  # yearTabs <-  
  #   lapply(1:5, function(z){
  #     tabPanel(paste("Year", z),
  #              fluidRow(
  #                column(8,
  #                       uiOutput(paste0("winterInfo", z)),
  #                       fluidRow(column(12, style = "background-color:white;", div(style = "height:50px;"))),
  #                       uiOutput(paste0("decision", z)),
  #                       uiOutput(paste0("insuranceUpdate", z)),
  #                       uiOutput(paste0("cowSell", z))
  #                ),
  #                column(2,
  #                       fluidRow(column(12, style = "background-color:white;", div(style = "height:170px;"))),
  #                       actionButton(paste0("year", z, "Start"), "Begin Simulation"),
  #                       fluidRow(column(12, style = "background-color:white;", div(style = "height:500px;"))),
  #                       uiOutput(paste0("continue", z)),
  #                       fluidRow(column(12, style = "background-color:white;", div(style = "height:700px;"))),
  #                       uiOutput(paste0("sellButton", z))
  #                )
  #              )
  #     )
  #   })
  # addTabToTabset(yearTabs, "mainPanels")
  # 
  addTabToTabset(createNewYr(1), "mainPanels")
}
