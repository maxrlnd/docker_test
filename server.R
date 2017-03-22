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
  
  #####Dynamic UI Functions#####################
  
  output$exp <- renderUI({
    if(input$experience == "Yes"){
      textInput("expExplain", "Please explain your previous ranch experience")
    }
  })
  

  output$decision1 <- renderUI({
    if(input$year1Start == 1){
      getJulyInfo()
    }
  })
  
  output$continue1 <- renderUI({
    if(input$year1Start == 1){
      actionButton("year1Summer", "Continue")
    }
  })
  
  output$winterInfo <- renderUI({
    getWinterInfo()
  })
  
  output$sellButton1 <- renderUI({
    if(!is.null(input$year1Summer)){
      if(input$year1Summer == 1){
        actionButton("sell1", "Sell Calves and Cows")
      }
    }
  })
  
  output$insuranceUpdate <- renderUI({
    if(!is.null(input$year1Summer)){
      if(input$year1Summer == 1){
        currentIndem <- round(indem()$indemnity, 0)
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
    
    output$cowSell <- renderUI({
      if(!is.null(input$year1Summer)){
        if(input$year1Summer == 1){
          tagList(
            getCowSell(effectiveForage(), wean()),
            plotOutput("cowPlot")
          )
        }
      }
    })

   output$finalAccount <- renderUI({
     
   })  

  
  ########## Functions to Print out state information
  
  output$julyRain <- renderTable({
    julyRain <- station.gauge$stgg[Year == (startYear + currentYear - 1),-1]/station.gauge$avg * 100
    julyRain[, 7:12 := "?"]
  })
 
  observe({
    toggleClass(condition = input$foo,
                class = "disabled",
                selector = "#navBar li a[data-value=Demographics]")
  })
  
  observe({
    toggleClass(selector = "#navbar li a[data-value=Demographics]")
  })
  
  observeEvent(input$year1Start, {
    shinyjs::disable("year1Start")
  })
  
  observeEvent(input$sell1, {
    disable("sell1")
    disable(("calves1Sale"))
    disable(paste0("cow1Sale"))
    myOuts <<- updateOuts(wean = wean(), forage = effectiveForage(), calfSale = input$calves1Sale,
                          indem = indem(), adaptCost = input$d1AdaptSpent, cowSales = input$cow1Sale, 
                          newHerd = herdSize(), zones = currentZones(), adaptInten = adaptInten()
                          )
    print(myOuts)
  })

  observeEvent(input$year1Summer, {
    shinyjs::disable("year1Summer")
    shinyjs::disable("d1AdaptSpent")
    
  })
  
  observeEvent(input$agree, {
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value=Demographics]")
    session$sendCustomMessage("myCallbackHandler", "1")
  })
  
  output$cowPlot <- renderPlot({
    years <- (startYear + currentYear - 1):(startYear + currentYear + 1)
    cows <- data.table("Year" = years, "Herd Size" = c(myOuts[currentYear, herd], 
                                                       herdSize(), herdSizey1()))
    ggplot(cows, aes(x = Year, y = `Herd Size`)) + geom_bar(stat = "identity")
  })
  
  currentZones <- reactive({
    zones <- station.gauge$zonewt
    if(currentYear == 1){
      zones <- zones * (1 - (0)/simRuns$forage.constant)
    }else{
      zones <- myOuts[currentYear - 1, zone.change] * zones * 
        (1 - (myOuts[currentYear - 1, Gt])/simRuns$forage.constant)
    }
    return(zones)
  })
  
  
  adaptInten <- reactive({
    myYear <- startYear + currentYear - 1
    herd <- myOuts[currentYear, herd]
    zones <- currentZones()
    
    ## Calcualte available forage
    
    forage <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 10, 11, "normal")
    
    ## Calculate Necessary Adaptation
    adaptationInten <- CalculateAdaptationIntensity(forage)
    return(adaptationInten)
  })
  
  effectiveForage <- reactive({
    myYear <- startYear + currentYear - 1
    herd <- myOuts[currentYear, herd]
    zones <- currentZones()
    
    ## Calcualte available forage
    
    forage <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 10, 11, "normal")
    
    ## Calculate Necessary Adaptation
    adaptationInten <- adaptInten()
    adaptationCost <-getAdaptCost(adpt_choice = "feed", pars = simRuns, 
                                  days.act = 180, current_herd = herd, intens.adj = adaptationInten)
    
    adaptationPercent <- input[[paste0("d", currentYear, "AdaptSpent")]]/adaptationCost
    # adaptationPercent <- eval(parse(text = paste0("input$d", currentYear, "AdaptSpent")))/adaptationCost
    
    ## Adjust Forage based on adaptation
    forage <- (1 - forage) * adaptationPercent + forage 
  })
  
  herdSize <- reactive({
    cows <- input[[paste0("cow", currentYear, "Sale")]]
    if(currentYear == 1){
      herd <- myOuts[currentYear, herd]
      shinyHerd(herd1 = herd, cull1 = cows, herd2 = herd, 
                calves2 = herd * simRuns$normal.wn.succ * (1 - simRuns$calf.sell),
                deathRate = simRuns$death.rate)
    }else{
      herd <- myOuts[currentYear, herd]
      herd2 <- myOuts[currentYear - 1, herd]
      wean2 <- myOuts[currentYear - 1, wn.succ]
      calvesSold <- myOuts[currentYear - 1, calves.sold]
      
      shinyHerd(herd1 = herd, cull1 = cows, herd2 = herd2, 
                calves2 = herd2 * wean2 * (1 - calvesSold),
                deathRate = simRuns$death.rate)
    }
  })
  
  herdSizey1 <- reactive({
    cows <- input[[paste0("cow", currentYear, "Sale")]]
    calves <- input[[paste0("calves", currentYear, "Sale")]]
    herd <- myOuts[currentYear, herd]
    herdy1 <- shinyHerd(herd1 = herdSize(), cull1 = cows, herd2 = herd,
              calves2 = (herd - calves), deathRate = simRuns$death.rate)
    return(herdy1)
  })
  
  wean <- reactive({
    AdjWeanSuccess(effectiveForage(), T, simRuns$normal.wn.succ, 1)
  })
  
  indem <- eventReactive(input[[paste0("year", currentYear, "Summer")]], {
    rma.ins = with(simRuns, insMat(yy = styr + currentYear - 1, clv = clv, acres = acres,
                                   pfactor = pfactor, insPurchase  =  insp, tgrd = tgrd))
    return(rma.ins)
  })
  

}
