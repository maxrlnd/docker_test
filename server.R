`%then%` <- shiny:::`%OR%`
genericWrong <- "This is incorrect please try again"
function(input, output, session) {
  

  observeEvent(input$runCode, {
    eval(parse(text = input$code))
  })
  
  if(!debugMode){
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value=Quiz]")
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value='Background Info']")
    toggleClass(class = "disabled",
                 selector = "#navBar li a[data-value='Ranch Simulation']")
  }
  
  
  
  #####Year Tab Functions#####################
  
  ## This loop Creates the necessary output functions for each year tab
  lapply(1:simLength, function(i){
    # lapply(c("p", "r"), function(j){})
    ## Reactive taglist for the first set of winter info at the start of each year, updates when
    ## myOuts updates
    assign(paste0("reactiveWinter", i), reactive({
      input[[paste0("sell", i-1)]]
      if(myOuts[i, herd] == 0){
        myOuts[i, cost.ins := 0]
      }
      delay(10,session$sendCustomMessage(type = "scrollCallbackTop", 0))
      tagList(
        br(),
        h3(paste0("Year ", i,": Winter Finance Assessment")),
        p("Before calving season begins, it is time to take account of your herd, range, and financial health."),
        br(),
        plotOutput(paste0("worthPlot", i)),
        tags$li(p("Your herd has ", 
                 span(prettyNum(myOuts[i, herd], digits = 0, big.mark=",", scientific=FALSE),style="font-weight:bold;font-size:large"), 
                 " cows, not including calves or yearlings (cows that are weaned, but not yet reproducing).")),
        if(prettyNum(myOuts[i, assets.cash], digits = 0)<0){
        tags$li(p("Your bank balance is $", span(prettyNum(myOuts[i, assets.cash], digits = 0,
                                                     big.mark=",", scientific=FALSE),style="font-weight:bold;font-size:large;color:red")))
        }else{
          tags$li(p("Your bank balance is $", span(prettyNum(myOuts[i, assets.cash], digits = 0,
                                                             big.mark=",", scientific=FALSE),style="font-weight:bold;font-size:large;color:green")))
          }
        ,
        if((prettyNum(myOuts[i, net.wrth], digits = 0)>0)){
        tags$li(p("Your current net worth, including cows and your bank balance, is $", 
                 span(prettyNum(myOuts[i, net.wrth], digits = 0, big.mark=",", scientific=FALSE),style="font-weight:bold;font-size:large;color:green"), "."))
          }else{
            tags$li(p("Your current net worth, including cows and your bank balance, is $", 
                      span(prettyNum(myOuts[i, net.wrth], digits = 0, big.mark=",", scientific=FALSE),style="font-weight:bold;font-size:large;color:red"), "."))
          },
        br(),
        h4("Range Condition"),
        if(ifelse(round(sum(get(paste0("currentZones", i))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", i))()) * 100, 0))<100){
        p("Your range is currently at ", span(ifelse(round(sum(get(paste0("currentZones", i))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", i))()) * 100, 0)),style="font-weight:bold;font-size:large;color:red"), "%")
          }else{
            p("Your range is currently at ", span(ifelse(round(sum(get(paste0("currentZones", i))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", i))()) * 100, 0)),style="font-weight:bold;font-size:large;color:green"), "%")
          },
        br(),
        h4("Bills Due"),
        p(p("Your rainfall-index insurance premium is due. You owe $", 
                 span(prettyNum(myOuts[i, cost.ins], digits = 0, big.mark=",",scientific=FALSE),style="font-weight:bold;font-size:large;color:red"), ". Please enter this amount below to pay your insurance bill.")),
        textInput(paste0("insurancePremium", i), 
                  "Please type the amount of the insurance premium below and to pay your bill and continue.",
                  width = "100%"),
        uiOutput(paste0("premCheck", i)),
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
    
    ## Reactive to track forage for each year
    assign(paste0("effectiveForage", i), reactive({
      
      ## Establish current state
      myYear <- startYear + i - 1
      herd <- myOuts[i, herd]
      zones <- get(paste0("currentZones", i))()
      
      ## Calculate available forage using Nov-Nov as a year
      forage <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 10, 11, "normal")
     
      ## Calculate adaptation intensity based on forage
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
      
      ## Calculate herd size for the first year in the simulation (i = 1)
      if(i == 1){
        herd <- myOuts[i, herd]
        shinyHerd(herd_1 = herd, cull_1 = cows, herd_2 = herd,  # Assumes that herd size has been stable for previous two years
                  calves_2 = herd * simRuns$normal.wn.succ * (1 - simRuns$calf.sell),
                  deathRate = simRuns$death.rate)
        
        
      ## Herd size for all subsequent years (i > 1)
      }else{
        herd <- myOuts[i, herd]
        herd_2 <- myOuts[i - 1, herd]
        wean_2 <- myOuts[i - 1, wn.succ]
        calvesSold <- myOuts[i - 1, calves.sold]
        
        shinyHerd(herd_1 = herd, cull_1 = cows, herd_2 = herd_2, 
                  calves_2 = herd_2 * wean_2 * (1 - calvesSold),
                  deathRate = simRuns$death.rate)
      }
    }))
    
    #################UI Functions of Year Tabs###################
    
    ## UI for winter Info
    output[[paste0("winterInfo", i)]] <- renderUI({
      tagList(
         h3(paste0("Year ", i, " of ", simLength, ": Ranching Simulation")),
         p("Remember, at the end of the simulation, you'll convert your net worth to a real MTurk bonus. Read the information carefully to make the best decisions."),
         get(paste0("reactiveWinter", i))()
       )
    })
    
    ## Start Button
    output[[paste0("start", i)]] <- renderUI({
      userPay <- gsub(",", "", input[[paste0("insurancePremium", i)]])
      userPay <- tryCatch(as.numeric(gsub("\\$", "", userPay)),
                          warning = function(war)return(0))
      if(!debugMode){
        req(userPay == round(indem[[i]]$producer_prem, 0), genericWrong)
      }
      actionButton(paste0("year", i, "Start"), "Next")
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
    
    ## Error message for incorrect prem deposit
    output[[paste0("premCheck", i)]] <- renderUI({
      userPay <- gsub(",", "", input[[paste0("insurancePremium", i)]])
      userPay <- tryCatch(as.numeric(gsub("\\$", "", userPay)),
                          warning = function(war)return(0))
      req(userPay)
      validate(
        need(userPay == round(indem[[i]]$producer_prem, 0), genericWrong)
      )
    })
    
    ## Display Update for insurance info
    output[[paste0("insuranceUpdate", i)]] <- renderUI({
      if(!is.null(input[[paste0("year", i, "Summer")]])){
        if(input[[paste0("year", i, "Summer")]]){
          if(myOuts[i, herd] == 0){
            indem[[i]]$indemnity <<- 0
          }
          currentIndem <- prettyNum(indem[[i]]$indemnity, digits = 0, big.mark=",",scientific=FALSE)
          tagList(
            br(),
            br(),
            h3(paste0("Year ", i, ": End of Growing Season")),
            if(currentIndem > 0){
              tagList(
                p("You didn't get much rain this summer! In the graph below you can see how much
                  it has rained since you decided whether or not to purchase hay (July and August)."),
                plotOutput(paste0("rainGraphSep", i)),
                p("Since you have rainfall insurance, 
                  you get a check to help cover your losses and extra expenses.
                  (Your rainfall insurance pays out when the rain falls significantly below
                  normal in May, June, July, and August.)"),
                br(),
                h4(p("You have received a check for $", span((currentIndem),style="font-weight:bold;font-size:large;color:green"), " from your rain insurance policy.")),
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
                plotOutput(paste0("rainGraphSep", i)),
                p("Because rainfall was close to or above normal levels, you did not recieve a check for your rain insurance policy"),
                h4(paste0("After your expenditures on hay and insurance, your new bank balance is: $", 
                          prettyNum(myOuts[i, assets.cash] - 
                                      indem[[i]]$producer_prem - input[[paste0("d", i, "AdaptSpent")]], 
                                    digits = 0, big.mark=",",scientific=FALSE))),
                actionButton(paste0("insCont", i), "Next")
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
      # print(get(paste0("effectiveForage", i))())
      # print( AdjWeanSuccess(get(paste0("effectiveForage", i))(), T, simRuns$normal.wn.succ, 1))
          tagList(
            getCowSell(get(paste0("effectiveForage", i))(), AdjWeanSuccess(get(paste0("effectiveForage", i))(), T, simRuns$normal.wn.succ, 1), i),
            plotOutput(paste0("cowPlot", i)),
            p("Keep in mind that yearlings (weaned calves that are not yet producing calves) 
              aren't counted in these herd size numbers. You also do not have the option to sell them in this game.")
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
            actionButton(paste0("year", i, "Summer"), "Purchase Hay")
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
            fluidRow(column(12, style = "background-color:white;", div(style = "height:750px;"))),
            actionButton(paste0("sell", i), "Sell Calves and Cows")
          )
      #   }
      # }
    })
    
    
    output[[paste0("postDeposit", i)]] <- renderUI({
      if(input[[paste0("insuranceDeposit", i)]] != ""){
        userIns <- gsub(",", "", input[[paste0("insuranceDeposit", i)]])
        userIns <- tryCatch(as.numeric(gsub("\\$", "", userIns)),
                            warning = function(war)return(0))
        if(!debugMode){
          validate(
            need(userIns == round(indem[[i]]$indemnity, 0), genericWrong)
          )
        }
        fluidRow(
          
          if(myOuts[i, assets.cash] + indem[[i]]$indemnity - 
             indem[[i]]$producer_prem - input[[paste0("d", i, "AdaptSpent")]] > 0){
            h4(p("After your expenditures on hay and your insurance check, your new bank balance is: $", 
                      span(prettyNum(myOuts[i, assets.cash] + indem[[i]]$indemnity - 
                                  indem[[i]]$producer_prem - input[[paste0("d", i, "AdaptSpent")]], 
                                digits = 0, big.mark=",",scientific=FALSE), style = "font-weight:bold:font-size:Xlarge;color:green")))
          }
          else{
            h4(p("After your expenditures on hay and your insurance check, your new bank balance is: $", 
                 span(prettyNum(myOuts[i, assets.cash] + indem[[i]]$indemnity - 
                                  indem[[i]]$producer_prem - input[[paste0("d", i, "AdaptSpent")]], 
                                digits = 0, big.mark=",",scientific=FALSE), style = "font-weight:bold:font-size:Xlarge;color:red")))
            
          }
          
        )
      }
    })
    
    output[[paste0("insSpace", i)]] <- renderUI({
      req(input[[paste0("year", i, "Summer")]])
      fluidRow(column(12, style = "background-color:white;", div(style = "height:1050px;")))
    })
    
    output[[paste0("postDepositButt", i)]] <- renderUI({
      
      if(!is.null(input[[paste0("year", i, "Summer")]])){
      if(indem[[i]]$indemnity == 0){
        tagList(
          actionButton(paste0("insCont", i), "Next")
        )
      }else{
      if(debugMode & input[[paste0("insuranceDeposit", i)]] == ""){
        actionButton(paste0("insCont", i), "Continue")
      }else if(input[[paste0("insuranceDeposit", i)]] != ""){
        userIns <- gsub(",", "", input[[paste0("insuranceDeposit", i)]])
        userIns <- tryCatch(as.numeric(gsub("\\$", "", userIns)),
                            warning = function(war)return(0))
        
        if(!debugMode){req(userIns == round(indem[[i]]$indemnity, 0))}
        tagList(
          actionButton(paste0("insCont", i), "Next")
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
          
          # Current herd size (determined by last years choices)
          herdy0 <- myOuts[i, herd]  
          
          # Next year's herd size
          herdy1 <- get(paste0("herdSize", i))()  
          
          # Herd size for the year after next
          herdy2 <- shinyHerd(herd_1 = herdy1,  # t-1 for year 2 is next years herd size
                              cull_1 = myOuts[1, cows.culled] * herdy1,  # we don't know how many cows they will cull next year. assume stability/default of 16% (draw from )
                              herd_2 = herdy0,  # t-2 for year 2 is this year
                              calves_2 = (floor(herdy0 * AdjWeanSuccess(get(paste0("effectiveForage", i))(), T, simRuns$normal.wn.succ, 1)) - calves),  # Calves in the herd this year minus those that are sold via the slider input
                              deathRate = simRuns$death.rate)  
          
          years <- c("This Year","Next Year","In Two Years")
          herd.projection <- data.table("Year" = years, "Herd Size" = c(herdy0, herdy1, herdy2))
          herd.projection$Year <- factor(herd.projection$Year, levels = c("This Year", "Next Year", "In Two Years"))
          herd.projection$`Herd Size` = round(herd.projection$`Herd Size`, 0)
          ggplot(herd.projection, aes(x = Year, y = `Herd Size`)) + geom_bar(stat = "identity", width = .3, fill = "#8b4513") +
            geom_text(aes(label = `Herd Size`), size = 10, position = position_stack( vjust = .5), color = "#ffffff") +
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
      #Rounding PlotOut dataframe table
      
      
      ggplot(plotOuts, aes(x = Year, y = `Value in $`, fill = Area)) + geom_bar(stat = "identity") + 
        ggtitle("Net Worth") + theme(legend.title = element_blank()) +
        scale_y_continuous(labels = comma) +
        #geom_text(aes(label = dollar(`Value in $`),), size = 5, position = position_stack(vjust = 0.3), angle = 90) +
        geom_text(aes(label = dollar(`Value in $`)), size = 5, position = position_stack(vjust = 0.3), angle = 90) +
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
        scale_fill_manual(values = c("#00008b", "#1e90ff")) +
        ylab("Rainfall (Inches)")
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
        geom_bar(width = .9, stat = "identity", position = 'dodge') + 
        theme(legend.title = element_blank(), text = element_text(size = 15)) + ggtitle("Rainfall") +
        scale_fill_manual(values = c("#00008b", "#1e90ff")) +
        ylab("Rainfall (Inches)")
    })
    
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
      myOuts <<- updateOuts(wean = AdjWeanSuccess(get(paste0("effectiveForage", i))(), T, simRuns$normal.wn.succ, 1), 
                            forage = get(paste0("effectiveForage", i))(), calfSale = input[[paste0("calves", i, "Sale")]],
                            indem = indem[[i]], adaptCost = input[[paste0("d", i, "AdaptSpent")]], cowSales = input[[paste0("cow", i, "Sale")]], 
                            newHerd = get(paste0("herdSize", i))(), zones = get(paste0("currentZones", i))(), 
                            currentYear = i)
      values$currentYear <- values$currentYear + 1
    })
    
    
    # Disable continue button and adaptation slider after clicking
    observeEvent(input[[paste0("year", i, "Summer")]], {
      shinyjs::disable(paste0("year", i, "Summer"))
      shinyjs::disable(paste0("d", i, "AdaptSpent"))
      delay(100,session$sendCustomMessage(type = "scrollCallbackIns", paste0("rainGraphSep", i)))
    })
    
    observeEvent(input[[paste0("insCont", i)]], {
      shinyjs::disable(paste0("insCont", i))
      delay(100,session$sendCustomMessage(type = "scrollCallbackCow", paste0("cowSell", i)))
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

  
  
  # Create an output for the sidebar widget on overall ranch status
  output$infoPane <- renderUI({
    fixedPanel(
      draggable = FALSE, top = 100, left = "auto", right = 20, bottom = "auto",
      width = 200, height = "auto",
      wellPanel(
        p(h3("Ranch Overview")), 
        br(), 
        # if(ifelse(round(sum(get(paste0("currentZones", i))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", i))()) * 100, 0))<100){
        #   p("Your range is currently at", span(ifelse(round(sum(get(paste0("currentZones", i))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", i))()) * 100, 0)),style="color:red"), "%")

        p(h4("Cattle Status:")), 
        p("You have:",prettyNum(myOuts[rv$page, herd], digits = 0, big.mark=",", scientific=FALSE)), 
        bsButton("cows", label = "", icon = icon("questions"), style = "info", size = "extra-small"),
        bsPopover(id = "cows", title = "Tidy data",
                  content = paste0("You should read the ", 
                                   a("tidy data paper", 
                                     href = "http://vita.had.co.nz/papers/tidy-data.pdf",
                                     target="_blank")),
                  placement = "left", 
                  trigger = "hover", 
                  options = list(container = "body")
        ),
        br(),
        if(round(sum(myOuts[rv$page, forage.potential])* 100, 0) >= 100){ 
        p("Range health:", span(round(sum(myOuts[rv$page, forage.potential])* 100, 0), style="color:green" ), "%")
        }else{
          p("Range health:", span(round(myOuts[ rv$page, forage.potential]* 100, 0), style="color:red" ), "%")
      }
        , 
          
        p(h4("Ranch Finances")),
        if((prettyNum(myOuts[rv$page, assets.cash], digits = 0,big.mark=",", scientific=FALSE))>0){
        p("Bank balance: $",span(prettyNum(myOuts[rv$page, assets.cash], digits = 0,big.mark=",", scientific=FALSE), style="color:green"))
        }else{p("Bank balance: $",span(prettyNum(myOuts[rv$page, assets.cash], digits = 0,big.mark=",", scientific=FALSE), style="color:red"))
          }
        , 
        if((prettyNum(myOuts[rv$page, net.wrth], digits = 0, big.mark=",", scientific=FALSE))>0){
        p("Net worth: $", span(prettyNum(myOuts[rv$page, net.wrth], digits = 0, big.mark=",", scientific=FALSE), style="color:green"))
        }else{p("Net worth: $", span(prettyNum(myOuts[rv$page, net.wrth], digits = 0, big.mark=",", scientific=FALSE), style="color:red"))
          
        }
      )
    )
  })
  
  # Create rollover tooltip for sidebar widget items
  addTooltip(session = session, id = "infoPane", title="test tooltip for sidebar widget", placement = "left", trigger = "hover")
  
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
              fluidRow(column(12, style = "background-color:white;", div(style = "height:1000px;"))),
              uiOutput(paste0("start", rv$page)),
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
      br(),
      p(paste0("Through ranching you accumulated $", round(myOuts$assets.cash[simLength + 1], 0), " in cash" )),
      p(paste0("You also have a herd worth $", round(myOuts$assets.cow[simLength + 1], 0), ".")),
      p(paste0("Your total net worth is $", round(myOuts$net.wrth[simLength + 1], 0), ". With a conversation rate of $200,000
               of simulation money to $1 of MTurk bonus money, you've earned $", round(myOuts$net.wrth[simLength + 1]/200000, 2),".")),
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
  observeEvent(input$reset_button, {
    createOutputs(practiceRuns, simRuns, indem)
    js$reset()
  })
  session$onSessionEnded(function() {
    createOutputs(practiceRuns, simRuns, indem)
    js$reset()
    stopApp()
  })
  
}
