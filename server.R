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
    print(input$map_zoom)
    input$map_zoom
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
  
  observe({
    toggleClass(condition = input$foo,
                class = "disabled",
                selector = "#navBar li a[data-value=Demographics]")
  })
  
  observe({
    toggleClass(selector = "#navbar li a[data-value=Demographics]")
  })
  
  observeEvent(input$agree, {
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value=Demographics]")
    session$sendCustomMessage("myCallbackHandler", "1")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
}
