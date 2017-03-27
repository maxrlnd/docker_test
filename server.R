function(input, output, session) {

  ## So this is supposed to update the constant variables which works
  ## but it does it through a << side effect...ugly
  ## So it needs to be fixed
  observeEvent(input$update, {
    conVarList <- as.list(constvars)
    inputList <- as.list(input)
    overlap <- intersect(names(conVarList), names(inputList))
    conVarList[overlap] <- inputList[overlap]
    constvars <<- as.environment(conVarList)
  })
  
  outs <- eventReactive(input$gen.res, {
    ## This function is going to need some work, we need to be able to pass
    ## the insurance information instead of having it assigned in the function
    # set.seed(1)
    getSimVars(random.starts = TRUE,
               use.forage = FALSE,
               random.acres=FALSE,
               random.productivity=TRUE,
               acres = input$acres.param) # with simulated vars
    simRuns <- append(append(as.list(station.gauge), as.list(constvars)), as.list(simvars))
    simRuns$sim.index <- 1
    outs <- data.table(sim_run(simRuns))
    numberCols <- names(outs)[-1]
    outs[, (numberCols) := lapply(.SD, round, digits = 2), .SDcols = numberCols]
    outs[, opt := mapvalues(opt, c("feed", "noadpt", "nodrght", "rentpast", "sellprs", "sellprs.norepl"),
                               c("Buy Feed", "Drought and No Adaptation", "No Drought", "Rent Pasture", "Sell Pairs",
                                 "Sell Cows/Don't Replace"))]
    outs[, ins := factor(ifelse(ins==1,"With Insurance","No Insurance"))]
    
  })
  
  observe({
    if(!is.null(input$map_zoom)){
      if(input$map_zoom >= 10 & !shapeDrawn){
        # north <- input$map_bounds[[1]] + 10
        # south <- input$map_bounds[[2]] - 10
        # east <- input$map_bounds[[3]] + 10
        # west <- input$map_bounds[[4]] -10
        # print(north)
        leafletProxy("map") %>% addPolygons(data=myShape, weight = 2, fillOpacity = 0)
        # leafletProxy("map") %>% addRasterImage(data=myShape, weight = 2, fillOpacity = .5)
        shapeDrawn <<- T
      }else if(input$map_zoom == 9){
        leafletProxy("map") %>% clearShapes()
        shapeDrawn <<- F
      }
    }
  })
  
  output$text1 <- renderText({
    print(input$agree[[1]])
    # print(input$map_zoom)
    # input$map_zoom
    })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- outs()
    data <- data[, input$show_outs, with = F]
    if(input$filter.strat != "All"){
      data <- data[opt == input$filter.strat]
    }
    if(input$filter.ins != "All"){
      data <- data[ins == input$filter.ins]
    }
    data
  },
  rownames = F, options = list(dom = "tlp")))
  
  output$plot1 <- renderPlot({
    outs.plot <- outs()
    start.networth <- outs.plot[yr == 0,]$net.wrth[1]/1000
    outs.plot=outs.plot[outs.plot$yr>0,]
    outs.plot$opt <- factor(outs.plot$opt)
    # outs.plot$opt=factor(outs.plot$opt,
    #                      levels=c("No Drought","Drought and No Adaptation","Buy Feed",
    #                               "Rent Pasture","Sell Cows/Replace","Sell Cows/Don't Replace"))
    # outs.plot$yr=outs.plot$yr+styear-1 # convert yr to actual years
    outs.plot$aftax.inc=round(outs.plot$aftax.inc/1000,2) # truncate income text for readability
    outs.plot$net.wrth=round(outs.plot$net.wrth/1000,2) # truncate net worth text for readability
    if(FALSE){
      print("Run Simulation First")
    } else if(input$graph.type == "Income"){
      ggplot(data=outs.plot,aes(x=yr,y=aftax.inc,colour=opt))+
        geom_hline(yintercept=0,linetype=2)+
        geom_line(size=1.2,alpha=0.8)+
        scale_colour_manual(values=brewer.pal(6,"Dark2"))+
        facet_grid(ins~opt)+
        xlab("Year")+
        ylab("After Tax Income ($1000 USD)")+
        theme(axis.text.x=element_text(angle=45))+
        labs(colour="Scenario")+
        ggtitle("Annual Profit")
    }else if(input$graph.type == "Net Worth"){
      ggplot(data=outs.plot,aes(x=yr,y=net.wrth,colour=opt))+
        geom_hline(yintercept=start.networth,linetype=2)+ # assumed starting net worth
        geom_line(size=1.2,alpha=0.8)+
        scale_colour_manual(values=brewer.pal(6,"Dark2"))+
        facet_grid(ins~opt)+
        xlab("Year")+
        ylab("Net Worth ($1000 USD)")+
        theme(axis.text.x=element_text(angle=45))+
        labs(colour="Scenario")+
        ggtitle("Year-End Net Worth")
    }
  })
  output$exp <- renderUI({
    if(input$experience == "Yes"){
      textInput("expExplain", "Please explain your previous ranch experience")
    }
  })
  
  #####Dynamic UI Functions#####################
  
  ## Display Winter info for year x
  # output[[paste0("winterInfo", 1)]] <- renderUI({
  #   getWinterInfo(currentYear)
  # })
  
  lapply(1:5, function(i){
    assign(paste0("reactiveWinter", i), reactive({
      input[[paste0("sell", i-1)]]
      tagList(
        h4("Winter Finance Assessment"),
        p(paste0("Your Current Net Worth is: $", round(myOuts[i, net.wrth], 0))),
        p(paste0("Your Current Herd is: ", round(myOuts[i, herd], 0))),
        p(paste0("Your Bank Balance is: $", round(myOuts[i, assets.cash], 0))),
        p(paste0("Your range is currently at: ", round(myOuts[i, forage.potential] * 100, 0), "%")),
        p(paste0("You paid: $", round(myOuts[i, cost.ins], 0), " for insurance"))
      )
    }))
    
    assign(paste0("currentZones", i), reactive({
      zones <- station.gauge$zonewt
      if(i %in% c(1,2)){
        zones <- zones * (1 - (0)/simRuns$forage.constant)
      }else{
        zones <- myOuts[i, zone.change] * zones * 
          (1 - (myOuts[i, Gt])/simRuns$forage.constant)
      }
      return(zones)
    }))
    
    assign(paste0("effectiveForage", i), reactive({
      myYear <- startYear + i - 1
      herd <- myOuts[i, herd]
      zones <- get(paste0("currentZones", i))()
      
      ## Calcualte available forage
      
      forage <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 10, 11, "normal")
     
      adaptationInten <- CalculateAdaptationIntensity(forage)
      adaptationCost <-getAdaptCost(adpt_choice = "feed", pars = simRuns, 
                                    days.act = 180, current_herd = herd, intens.adj = adaptationInten)
      adaptationPercent <- ifelse(adaptationCost == 0, 0, input[[paste0("d", i, "AdaptSpent")]]/adaptationCost)
      # adaptationPercent <- eval(parse(text = paste0("input$d", currentYear, "AdaptSpent")))/adaptationCost
      ## Adjust Forage based on adaptation
      forage <- (1 - forage) * adaptationPercent + forage 
    }))
    
    assign(paste0("herdSize", i), reactive({
      cows <- input[[paste0("cow", i, "Sale")]]
      if(i == 1){
        herd <- myOuts[i, herd]
        shinyHerd(herd1 = herd, cull1 = cows, herd2 = herd, 
                  calves2 = herd * simRuns$normal.wn.succ * (1 - simRuns$calf.sell),
                  deathRate = simRuns$death.rate)
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
    
    
    output[[paste0("winterInfo", i)]] <- renderUI({
     # if(!is.null(input[[paste0("sell",i)]])){
     #   print("hello")
     #   if(input[[paste0("sell",i)]] == 1){
         get(paste0("reactiveWinter", i))()
     #   }
     # }
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
    
    output[[paste0("continue", i)]] <- renderUI({
      if(!is.null(input[[paste0("year", i, "Start")]])){
        if(input[[paste0("year", i, "Start")]] == 1){
          actionButton(paste0("year", i, "Summer"), "Continue")
        }
      }
    })
    
    
    output[[paste0("sellButton", i)]] <- renderUI({
      if(!is.null(input[[paste0("year", i, "Summer")]])){
        if(input[[paste0("year", i, "Summer")]] == 1){
          actionButton(paste0("sell", i), "Sell Calves and Cows")
        }
      }
    })
    
    output[[paste0("julyRain", i)]] <- renderTable({
      julyRain <- station.gauge$stgg[Year == (startYear + i - 1),-1]/station.gauge$avg * 100
      julyRain[, 7:12 := "?"]
    })
    
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
    
    observeEvent(input[[paste0("year", i, "Start")]], {
      shinyjs::disable(paste0("year", i, "Start"))
    })
    
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
    })
    
    observeEvent(input[[paste0("year", i, "Summer")]], {
      shinyjs::disable(paste0("year", i, "Summer"))
      shinyjs::disable(paste0("d", i, "AdaptSpent"))
      
    })
    
    
  }) ##End of lapply
  
  

  

  
  
 
  

  
  
  

  
  ########## Functions to Print out state information
  

 

  observe({
    toggleClass(selector = "#navbar li a[data-value=Demographics]")
  })
  
  observeEvent(input$agree, {
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value=Demographics]")
    session$sendCustomMessage("myCallbackHandler", "1")
  })

  values <- reactiveValues("currentYear" = 1)
  
  # Important! : creationPool should be hidden to avoid elements flashing before they are moved.
  #              But hidden elements are ignored by shiny, unless this option below is set.
  output$creationPool <- renderUI({})
  outputOptions(output, "creationPool", suspendWhenHidden = FALSE)
  # End Important
  
  # Important! : This is the make-easy wrapper for adding new tabPanels.
  addTabToTabset <- function(Panels, tabsetName){
    titles <- lapply(Panels, function(Panel){return(Panel$attribs$title)})
    Panels <- lapply(Panels, function(Panel){Panel$attribs$title <- NULL; return(Panel)})
    
    output$creationPool <- renderUI({Panels})
    session$sendCustomMessage(type = "addTabToTabset", message = list(titles = titles, tabsetName = tabsetName))
  }
  # End Important 

  yearTabs <-  
    lapply(1:5, function(z){
      tabPanel(paste("Year", z),
               fluidRow(
                 column(8,
                        uiOutput(paste0("winterInfo", z)),
                        fluidRow(column(12, style = "background-color:white;", div(style = "height:50px;"))),
                        uiOutput(paste0("decision", z)),
                        uiOutput(paste0("insuranceUpdate", z)),
                        uiOutput(paste0("cowSell", z))
                 ),
                 column(2,
                        fluidRow(column(12, style = "background-color:white;", div(style = "height:170px;"))),
                        actionButton(paste0("year", z, "Start"), "Begin Simulation"),
                        fluidRow(column(12, style = "background-color:white;", div(style = "height:500px;"))),
                        uiOutput(paste0("continue", z)),
                        fluidRow(column(12, style = "background-color:white;", div(style = "height:700px;"))),
                        uiOutput(paste0("sellButton", z))
                 )
               )
      )
    })
  addTabToTabset(yearTabs, "mainPanels")
    

}
