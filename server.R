`%then%` <- shiny:::`%OR%`
genericWrong <- "This is incorrect please try again"
function(input, output, session) {
  
  observeEvent(input$runCode, {
    eval(parse(text = input$code))
  })
  
  toggleClass(class = "disabled",
              selector = "#navBar li a[data-value=Quiz]")
  toggleClass(class = "disabled",
              selector = "#navBar li a[data-value='Background Info']")
  # toggleClass(class = "disabled",
  #             selector = "#navBar li a[data-value='Ranch Simulation']")
  # lapply(1:simLength, function(x)toggleClass(class = "disabled", selector = paste0("#navBar li a[data-value=Year ", x, "]")))

  output$weanVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
          need(input$weanQ, genericWrong) %then%
          need(all(input$weanQ == c("grow", "wean", "land")), genericWrong)
        )
      )
  })
  
  output$ranchVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$ranchSizeQ == 3000, "This is the incorrect size try again")
      )
    )
  })
  
  output$cullVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$cullQ == "To sell a cow", genericWrong)
      )
    )
  })
  
  output$herdSizeVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$herdSizeQ == 600, genericWrong)
      )
    )
  })
  
  output$lHerdVal <- renderText(({
    req(input$quizSub)
    isolate(
      validate(
        need(input$lHerdQ == 600, genericWrong)
      )
    )
  }))
  
  output$bigHerdVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
          need(input$bigHerdQ, genericWrong) %then%
          need(all(input$bigHerdQ == c("moreCalves", "damage")), genericWrong)
      )
    )
  })
  
  output$priceVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$priceQ == "same", genericWrong)
      )
    )
  })
  
  output$adaptVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$adaptQ, genericWrong) %then%
        need(all(input$adaptQ == c("underweight", "fewerBirths", "fewerCalves")), genericWrong)
      )
    )
  })
  
  output$earningsVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$earningsQ == "$3", genericWrong)
      )
    )
  })
  
  output$practiceVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$practiceQ == "True", genericWrong)
      )
    )
  })
  
  output$bonusVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$bonusQ == "No", genericWrong)
      )
    )
  })
  
  output$premiumVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$premiumQ == "something reasonable", genericWrong)
      )
    )
  })
  
  output$rainmonthsVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$rainmonthsQ == "May-June, July-August", genericWrong)
      )
    )
  })
  
  output$payoutVal <- renderText({
    req(input$quizSub)
    isolate(
      validate(
        need(input$payoutQ == "2 inches", genericWrong)
      )
    )
  })
  # output$insuranceQuiz <- renderUI({
  #   if(purchaseInsurance){
  #     tagList(
  #       selectInput("premiumQ", "How much does your rain-index insurance cost each year?",
  #                   choices = c("", "$0", "$100", "something reasonable")),
  #       textOutput("premiumVal"),
  #       radioButtons("rainmonthsQ", "Your insurance payouts depend on rain in which months?",
  #                    choices = c("May-June, July-August", "May-June, June-July", 
  #                                "February-March, May-June", "July-August, October-November")),
  #       textOutput("rainmonthsVal"),
  #       selectInput("payoutQ", "Would you get a larger insurance payout if you get 5 inches of rain or 2 inches of rain during 
  #                   a month that is insured?", choices = c("", "5 inches", "2 inches")),
  #       textOutput("payoutVal")
  #   )}
  # })
  
  #####Year Tab Functions#####################
  
  ## This loop Creates the necessary output functions for each year tab
  lapply(1:simLength, function(i){
    # lapply(c("p", "r"), function(j){})
    ## Reactive taglist for the first set of winter info at the start of each year, updates when
    ## myOuts updates
    assign(paste0("reactiveWinter", i), reactive({
      input[[paste0("sell", i-1)]]
      if(myOuts[i, herd] == 0){
        myOuts[i, cost.ins] <<- 0
      }
      tagList(
        br(),
        h4("Winter Finance Assessment"),
        p(paste0("Your Current Herd has ", round(myOuts[i, herd], 0), " cows, not including calves or yearlings.")),
        p(paste0("Your Bank Balance is $", round(myOuts[i, assets.cash], 0))),
        p(paste0("Your current net worth, including cows and your bank balance, is $", round(myOuts[i, net.wrth], 0), ".")),
        p(paste0("Your range is currently at ", round(myOuts[i, forage.potential] * 100, 0), "%")),
        p(paste0("You paid: $", round(myOuts[i, cost.ins], 0), " for insurance")),
        plotOutput(paste0("worthPlot", i)),
        tags$hr(style="border-color: darkgray;")
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
      tagList(
         h3(paste0("Year ", i, " of ", simLength)),
         get(paste0("reactiveWinter", i))()
       )
    })
    
    ## Display rain info up to July and allow user to choose adaptation level
    output[[paste0("decision", i)]] <- renderUI({
      if(!is.null(input[[paste0("year", i, "Start")]])){  
        if(input[[paste0("year", i, "Start")]] == 1){
          tagList(
            getJulyInfo(i)
          )
        }
      }
    })
    
    ## Display Update for insurance info
    output[[paste0("insuranceUpdate", i)]] <- renderUI({
      if(!is.null(input[[paste0("year", i, "Summer")]])){
        if(input[[paste0("year", i, "Summer")]]){
          if(myOuts[i, herd] == 0){
            indem[[i]]$indemnity <<- 0
          }
          currentIndem <- round(indem[[i]]$indemnity, 0)
          tagList(
            br(),
            br(),
            h4("Insurance Payout"),
            if(currentIndem > 0){
              tagList(
                p("You didn't get much rain this summer! In the graph below you can see how much
                  it has rained since you decided whether or not to purchase hay (July and August)."),
                plotOutput(paste0("rainGraphSep", currentYear)),
                p("Since you have rainfall insurance, 
                  you get a check to help cover your losses and extra expenses.
                  (Your rainfall insurance pays out when the rain falls significantly below
                  normal in May, June, July, and August.)"),
                br(),
                h4(paste0("You have received a check for $", currentIndem, " from your rain insurance policy.")),
                textInput(paste0("insuranceDeposit", i), 
                          "Please type the amount of the check below and to add the money to your bank account and continue.",
                          width = "100%"),
                # actionButton(paste0("deposit", i), "Deposit"),
                uiOutput(paste0("postDeposit", i)),
                uiOutput(paste0("postDepositButt", i))
              )
            }else{
              tagList(
                p("You got sufficient rain this summer, so your grass should be in good shape for your cattle! 
                  In the graph below you can see how much
                  it has rained since you decided whether or not to purchase hay (July and August)."),
                plotOutput(paste0("rainGraphSep", currentYear)),
                p("Because rainfall was close to or above normal levels, you did not recieve a check for your rain insurance policy"),
                actionButton(paste0("insCont", i), "Continue")
              )
            }
          )
        }
      }
    })
    
    
    ## Present options to sell cows
    output[[paste0("cowSell", i)]] <- renderUI({
      # if(!is.null(input[[paste0("year", i, "Summer")]])){
      #   if(input[[paste0("year", i, "Summer")]] == 1){
      req(input[[paste0("insCont", i)]])
      print(get(paste0("effectiveForage", i))())
      print( AdjWeanSuccess(get(paste0("effectiveForage", i))(), T, simRuns$normal.wn.succ, 1))
          tagList(
            getCowSell(get(paste0("effectiveForage", i))(), AdjWeanSuccess(get(paste0("effectiveForage", i))(), T, simRuns$normal.wn.succ, 1), i),
            plotOutput(paste0("cowPlot", i))
          )
      #   }
      # }
    })
    
    ## Create a button to continue after selecting adaptation level
    output[[paste0("continue", i)]] <- renderUI({
      if(!is.null(input[[paste0("year", i, "Start")]])){
        if(input[[paste0("year", i, "Start")]] == 1){
          tagList(
            fluidRow(column(12, style = "background-color:white;", div(style = "height:900px;"))),
            actionButton(paste0("year", i, "Summer"), "Continue")
          )
        }
      }
    })
    
    output[[paste0("nextButton", i)]] <- renderUI({
      if(!is.null(input[[paste0("sell", i)]])){
        if(input[[paste0("sell", i)]] == 1){
          tagList(
            fluidRow(column(12, style = "background-color:white;", div(style = "height:50px;"))),
            actionButton("nextBtn", "Begin Next Year >")
          )
        }
      }

    })
    
    ## Create button to sell calves and cows once decisions are made
    ## Additionally moves simualation to the next year
    output[[paste0("sellButton", i)]] <- renderUI({
      # if(!is.null(input[[paste0("year", i, "Summer")]])){
      #   if(input[[paste0("year", i, "Summer")]] == 1){
      req(input[[paste0("insCont", i)]])    
      tagList(
            fluidRow(column(12, style = "background-color:white;", div(style = "height:500px;"))),
            actionButton(paste0("sell", i), "Sell Calves and Cows")
          )
      #   }
      # }
    })
    
    output[[paste0("postDeposit", i)]] <- renderUI({
      if(input[[paste0("insuranceDeposit", i)]] != ""){
        validate(
          need(input[[paste0("insuranceDeposit", i)]] == round(indem[[i]]$indemnity, 0), genericWrong)
        )
        fluidRow(
          h4(paste0("After your expenditures on hay and your insurance check, your new bank balance is $", round(myOuts[i, assets.cash] + indem[[i]]$indemnity, 0)))
        )
      }
    })
    
    output[[paste0("insSpace", i)]] <- renderUI({
      req(input[[paste0("year", i, "Summer")]])
      fluidRow(column(12, style = "background-color:white;", div(style = "height:1050px;")))
    })
    
    output[[paste0("postDepositButt", i)]] <- renderUI({
      
      if(!is.null(input[[paste0("year", i, "Summer")]])){
        print("hello")
      if(indem[[i]]$indemnity == 0){
        tagList(
          actionButton(paste0("insCont", i), "Continue")
        )
      }else{
      if(input[[paste0("insuranceDeposit", i)]] != ""){
        req(input[[paste0("insuranceDeposit", i)]] == round(indem[[i]]$indemnity, 0))
        tagList(
          actionButton(paste0("insCont", i), "Continue")
        )
      }
      }
      }
    })
    
    ## Table of rain for each July
    output[[paste0("julyRain", i)]] <- renderTable({
      currentYear <- (startYear + i - 1)
      yprecip <- station.gauge$stgg[Year %in% (currentYear - 1):currentYear, ]  # monthly precip amounts for start year
      yprecip <- cbind((yprecip[Year == currentYear - 1, c("NOV", "DEC")]), 
                       (yprecip[Year == currentYear, -c("NOV", "DEC", "Year")]))
      yprecip[, 9:12 := 0]
      ave <- station.gauge$avg
      yearAvg <- rbindlist(list(yprecip, ave), use.names = T)
      julyRain <- round((yearAvg[1,]/yearAvg[2,]) * 100, 2)
      julyRain[, 9:12 := "?"]
      # julyRain <- station.gauge$stgg[Year == (startYear + i - 1),-1]/station.gauge$avg * 100
      # julyRain[, 7:12 := "?"]
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
          print(cows)
          cows$`Herd Size` = round(cows$`Herd Size`, 0)
          ggplot(cows, aes(x = Year, y = `Herd Size`, )) + geom_bar(stat = "identity", width = .3, fill = "#8b4513") +
            geom_text(aes(label = `Herd Size`), size = 10, position = position_stack(vjust = .5), color = "#ffffff") +
            theme(text = element_text(size = 20))
          #Fix Font Size
          #Fix Fatness of bar graphs
          
        }
      }
    })
    ## Bar graph to display net worth
    output[[paste0("worthPlot", i)]] <- renderPlot({
      plotOuts <- myOuts[, c("yr", "assets.cow", "assets.cash"), with = F]
      setnames(plotOuts, c("Year", "Value of Cows", "Cash"))
      plotOuts[, Year := startYear:(startYear + nrow(plotOuts) - 1)]
      plotOuts <- melt(plotOuts, id.vars = "Year")
      setnames(plotOuts, c("Year", "Area", "Value in $"))
      plotOuts$Area <- factor(plotOuts$Area)
      library(scales)
      #Rounding PlotOut dataframe table
      
      
      ggplot(plotOuts, aes(x = Year, y = `Value in $`, fill = Area)) + geom_bar(stat = "identity") + 
        ggtitle("Net Worth") + theme(legend.title = element_blank()) +
        scale_y_continuous(labels = comma) +
        #geom_text(aes(label = dollar(`Value in $`),), size = 5, position = position_stack(vjust = 0.3), angle = 90) +
        geom_text(aes(label = dollar(`Value in $`)), size = 5, position = position_stack(vjust = 0.3)) +
        theme(text = element_text(size = 20)) +
        scale_fill_manual(values = c("#f4a460", "#85bb65"))
      #change default color schemes
      #Green for cash, light peach for cows
      #X AXis - Year 1, Year2, Year 3, etc. 
      #X Label, Character Vector
    })
    
    ## Bar graph to display rainfall
    output[[paste0("rainGraph", i)]] <- renderPlot({
      currentYear <- (startYear + i - 1)
      yprecip <- station.gauge$stgg[Year %in% (currentYear - 1):currentYear, ]  # monthly precip amounts for start year
      yprecip <- cbind((yprecip[Year == currentYear - 1, c("NOV", "DEC")]), 
                       (yprecip[Year == currentYear, -c("NOV", "DEC", "Year")]))
      yprecip[, 9:12 := 0]
      ave <- station.gauge$avg
      yearAvg <- rbindlist(list(yprecip, ave), use.names = T)
      yearAvg[, "id" := c("Actual Rain", "Average Rain")]
      yearAvg <- melt(yearAvg, id.vars = "id")
      setnames(yearAvg, c("id", "Month", "Rainfall"))
      ggplot(yearAvg, aes(x = Month, y = Rainfall, fill = id, label = Rainfall)) + 
        geom_bar(width = .9, stat = "identity", position = 'dodge') + 
        #facet_wrap(~ id) +
        #geom_text(size = 3, position = position_stack(vjust = 0.5)) +
        theme(legend.title = element_blank(), text = element_text(size = 15)) + ggtitle("Rainfall") +
        scale_fill_manual(values = c("#00008b", "#1e90ff"))
      #position_dodge(width=1.3)
      #Rain color it!
      
    })
    
    ## Bar graph to display rainfall with July and August added
    output[[paste0("rainGraphSep", i)]] <- renderPlot({
      currentYear <- (startYear + i - 1)
      yprecip <- station.gauge$stgg[Year %in% (currentYear - 1):currentYear, ]  # monthly precip amounts for start year
      yprecip <- cbind((yprecip[Year == currentYear - 1, c("NOV", "DEC")]), 
                       (yprecip[Year == currentYear, -c("NOV", "DEC", "Year")]))
      yprecip[, 11:12 := 0]
      ave <- station.gauge$avg
      yearAvg <- rbindlist(list(yprecip, ave), use.names = T)
      yearAvg[, "id" := c("Actual Rain", "Average Rain")]
      yearAvg <- melt(yearAvg, id.vars = "id")
      setnames(yearAvg, c("id", "Month", "Rainfall"))
      ggplot(yearAvg, aes(x = Month, y = Rainfall, fill = id)) + 
        geom_bar(width = .6, stat = "identity", position = position_dodge(width=0.7)) + 
        theme(legend.title = element_blank()) + ggtitle("Rainfall")
    })
    
    # Reactive to disable start simualation button after they're clicked
    observeEvent(input[[paste0("year", i, "Start")]], {
      shinyjs::disable(paste0("year", i, "Start"))
      delay(100,session$sendCustomMessage(type = "scrollCallback", 1))
    })
    
    # observeEvent(input[[paste0("year", i, "Start")]], {
    #   session$sendCustomMessage(type = "scrollCallback", 1)
    # })
    
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
      values$currentYear <- values$currentYear + 1
    })
    
    
    # Disable continue button and adaptation slider after clicking
    observeEvent(input[[paste0("year", i, "Summer")]], {
      shinyjs::disable(paste0("year", i, "Summer"))
      shinyjs::disable(paste0("d", i, "AdaptSpent"))
      delay(100,session$sendCustomMessage(type = "scrollCallback", 1))
    })
    
    observeEvent(input[[paste0("insCont", i)]], {
      shinyjs::disable(paste0("insCont", i))
      delay(100,session$sendCustomMessage(type = "scrollCallback", 1))
    })
  }) ##End of lapply
  
  
  ## Observer for begin button in demographis panel
  observeEvent(input$demoSub, {
    # addTabToTabset(createNewYr(1), "mainPanels")
    disable("demoSub")
    session$sendCustomMessage("myCallbackHandler", "1")
  })
  
  ## Observer to track the number of quiz attempts
  observeEvent(input$quizSub, {
    quizCounter <<- quizCounter + 1
    if(quizCounter > 3){
      showModal(exitModal())
    }
  })
  
  observeEvent(input$Exit, {
    js$closewindow();
    stopApp()
  })
  
  exitModal <- function(){
    modalDialog(
      h4("Unfortunately, you've failed the quiz after three tries and cannot continue with the simulation."),
      h4("Thank you for participating in this survey."),
      footer = actionButton("Exit", "Exit")
    )
  }

  ########## Functions to Print out state information
  


  ## Disable agree button on instructions after it has been clicked and move to
  ## Demographics tab
  observeEvent(input$agree, {
    disable("agree")
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value='Background Info']")
    updateTabsetPanel(session, "mainPanels", selected = "Background Info")
  })
  
  observeEvent(input$simStart, {
    disable("simStart")
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value='Ranch Simulation']")
    updateTabsetPanel(session, "mainPanels", selected = "Ranch Simulation")
  })
  
  ## Reactive value for current year
  values <- reactiveValues("currentYear" = 1, "starting" = TRUE, "saveComplete" = FALSE, "beginSaving" = FALSE)
  
  ## Code to dynamically add new tabs
  output$creationPool <- renderUI({})
  outputOptions(output, "creationPool", suspendWhenHidden = FALSE)
  
  addTabToTabset <- function(Panels, tabsetName){
    titles <- lapply(Panels, function(Panel){return(Panel$attribs$title)})
    Panels <- lapply(Panels, function(Panel){Panel$attribs$title <- NULL; return(Panel)})
    
    output$creationPool <- renderUI({Panels})
    session$sendCustomMessage(type = "addTabToTabset", message = list(titles = titles, tabsetName = tabsetName))
  }
  

  observeEvent(input$update, {
    simRunsList <- as.list(simRuns)
    inputList <- reactiveValuesToList(input)
    overlap <- intersect(names(simRunsList), names(inputList))
    simRunsList[overlap] <- inputList[overlap]
    simRuns <<- simRunsList
    myOuts <<- createResultsFrame(simRuns)
    startYear <<- input$act.st.yr
  })
  rv <- reactiveValues(page = 1)
  output$page <- renderText(rv$page)
  
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < simLength + 1)
    hide(selector = ".page")
    show(sprintf("step%s", rv$page))
  })

  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  
  output$pageOut <- renderUI({
  
  if(rv$page <= simLength){  
    fluidRow(
       column(9,
              uiOutput(paste0("winterInfo", rv$page)),
              fluidRow(column(12, style = "background-color:white;", div(style = "height:50px;"))),
              uiOutput(paste0("decision", rv$page)),
              uiOutput(paste0("insuranceUpdate", rv$page)),
              uiOutput(paste0("cowSell", rv$page))
       ),
       column(2,
              fluidRow(column(12, style = "background-color:white;", div(style = "height:470px;"))),
              actionButton(paste0("year", rv$page, "Start"), "Begin Simulation"),
              uiOutput(paste0("continue", rv$page)),
              uiOutput(paste0("insSpace", rv$page)),
              uiOutput(paste0("sellButton", rv$page)),
              uiOutput(paste0("nextButton", rv$page))
       )
     )
  }else{
    fluidRow(
      column(width = 10,
      h4(paste0("Congratulations! You've completed ", simLength, " years of ranching.")),
      p(paste0("At the end of the simulation your ranch had ", myOuts$herd[simLength], " cows")),
      p(paste0("Through ranching you accumulated $", round(myOuts$assets.cash[simLength], 0), " in cash" )),
      actionButton("saveInputs", "Save results and recieve completion code"),
      uiOutput("complete"),
      offset = .5)
    )
  }
  })

  output$complete <- renderUI({
    req(values$saveComplete)
    h4("Your Data has been saved, complete code...")
  })
  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, navPage(1))
  observeEvent(input$saveInputs, {
    shinyjs::disable("saveInputs")
    # myDir <- "results"
    # if(!dir.exists(myDir)){
    #   dir.create(myDir)
    # }
    files <- gs_ls()$sheet_title
    # files <- files[order(files)]
    
    if(length(files) == 0){
      lastFile <- 0
    }else{
      # lastFile <- files[length(files)]
      # lastFile <- as.numeric(substr(lastFile, nchar(lastFile), nchar(lastFile)))
      lastFile <- regmatches(files, gregexpr('[0-9]+',files))
      lapply(lastFile, as.numeric) %>% unlist() %>% max() -> lastFile
    }
    saveData <- reactiveValuesToList(input)
    # save(saveData, file = "newSave.RData")
    saveData <- inputToDF(saveData)
    withProgress(message = "Saving Data", value = 1/3, {
    gs_new(title =  paste0("input", lastFile + 1), 
           input = saveData, trim = TRUE, verbose = TRUE)
    incProgress(1/3)
    gs_new(title =  paste0("output", lastFile + 1), 
           input = myOuts, trim = TRUE, verbose = TRUE)
    })
    values$saveComplete <- TRUE
    # write.csv(saveData, file = paste0("results/input", lastFile + 1, ".csv"), row.names = F)
    # write.csv(myOuts, file = paste0("results/output", lastFile + 1, ".csv"), row.names = F)
  })
  
  observeEvent(input$applyInsChange, {
    req(input$insChange)
    if(input$insChange){
      purchaseInsurance <<- T
      indem <<- lapply(startYear:(startYear + simLength - 1), function(x){
        with(simRuns, shinyInsMat(yy = x, clv = clv, acres = acres,
                                  pfactor = pfactor, insPurchase  =  insp, tgrd = tgrd))
      })
    }else{
      purchaseInsurance <<- FALSE
      indem <<- lapply(indem, function(x){
        x[, c("producer_prem", "indemnity", "full_prem") := 0]
        return(x)
      }) 
    }
      
  })
}
