tabPanel("Insurance",
         div(class="outer",
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css")
               # includeScript("gomap.js")
             ),
             leafletOutput("map", width = "100%", height = "100%"),
             
             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                           width = 330, height = "auto",
                           
                           h3("Insurance"),
                           # Purchase Insurance?
                           ## Change all further options to be dynamic based on this input
                           selectInput("purchase.insurance", h4("Purchase Insurance"),
                                       c("Yes" = 1, "No" = 0)),
                           selectInput("pfactor",label=h4("Productivity Factor"),choices=60:150,selected=100),
                           selectInput("clv",label=h4("Coverage Level"),choices=seq(70,90,by=5),selected=90),
                           numericInput("acres.param", label = h4("Acres"), 2000, min = 0, step = 100),
                           h4("Insurance Allocation"),
                           column(4,
                                  selectInput("i1",label="Jan-Feb",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i4",label="Apr-May",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i7",label="Jul-Aug",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i10",label="Oct-Nov",
                                              choices=seq(0,1,by=0.1))
                           ),
                           column(4,
                                  selectInput("i2",label="Feb-Mar",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i5",label="May-Jun",
                                              choices=seq(0,1,by=0.1),selected=0.5),
                                  selectInput("i8",label="Aug-Sep",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i11",label="Nov-Dec",
                                              choices=seq(0,1,by=0.1))
                           ),
                           column(4,
                                  selectInput("i3",label="Mar-Apr",
                                              choices=seq(0,1,by=0.1),selected=0.5),
                                  selectInput("i6",label="Jun-Jul",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i9",label="Sep-Oct",
                                              choices=seq(0,1,by=0.1))
                           ),
                           actionButton("updateIns", "Update Insurance"),
                           h4(textOutput("text1"))
             )
         )
),

tabPanel("Input",
         fluidRow(
           column(2,
                  h4("Drought Facts"),
                  
                  # Start of the drought year
                  numericInput("act.st.yr", "Year", 2002, min = 1900, step = 1),
                  
                  # Month the drought starts
                  numericInput("act.st.m", "Start Month (Drought)", 6, min = 1, max = 12, step = 1),
                  
                  # End of drought month
                  ## Idealy the min and default value of this would be dynamic
                  ## based on the inputs to the above same with year end
                  numericInput("act.end.m", "Predicted End Month (Drought)", 6, min = 1, max = 12, step = 1),
                  
                  # End of drought year
                  numericInput("act.end.yr", "End Decision Year", 2002, min = 1900, step = 1)
           ),
           column(2, offset = 1,
                  h4("Cows"),
                  
                  # Set Herd size
                  ## The defaults should probably be changed here based on domain knowledge
                  numericInput("herd", "Herd", 600, min = 0, step = 1),
                  
                  # Average weight of a cow
                  ## I'd like to add lbs to the end of this statement
                  numericInput("cow.wt", "Average Cow Weight", 1200, min = 1, step = 1),
                  
                  # Current value of a cos
                  numericInput("p.cow", "Current Value of Cows (per cow)", 850, min = 1),
                  
                  # Cows Culled per year
                  numericInput("cull.num", "Number of Cows Culled in a Normal Year", 15, min = 0, step = 1),
                  
                  # Cow Cost
                  numericInput("cow.cost", 'Annual "Cow" Cost', 500, min = 1)
           ),
           column(2, offset = 1,
                  
                  h4("Other Information"),
                  
                  # Price of hay
                  ## This should really have a dollar sign infront of it but not sure how to do it right now
                  numericInput("p.hay", "Price of Hay ($/Ton)", 100, min = 1),
                  
                  # Cost of Pasture
                  numericInput("past.rent", "Pasture (per AUM)", 16.49, min = 1),
                  
                  # Interest rate on Borrow Money
                  numericInput("loan.int", "Interest Rate for Borrowed Money", .065, min = 0, step = .001),
                  
                  # Interest RAte for Invested Money
                  numericInput("invst.int", "Interest Rate for Invested Money", .0125, min = 0, step = .001),
                  
                  ## Excel model has average tax basis per cow but R model doesn't have this
                  
                  # Total (fed + state) on income
                  numericInput("cap.tax.rate", "Total Income Tax Rate (US & State)", .19, min = 0, max = 1)
           ),
           column(2, offset = 1,
                  h4("Calves"),
                  
                  # Average weaning percent
                  ## This really needs to be changed to a text box with a 
                  ## percent symbol after it
                  numericInput("wn.succ", "Average Weaning Percentage", .94, min = 0, max = 1),
                  
                  # Percent of calfs sold
                  numericInput("calf.sell", "Average Percent of Calves Sold", .75, min = 0, max = 1),
                  
                  # Current Weight of calves
                  numericInput("calf.wt", "Average Weight Currently (lbs)", 375, min = 1), 
                  
                  # Calf weight at weaning
                  numericInput("expected.wn.wt", "Average Weight at Weaning (lbs)", 600, min = 1),
                  
                  # Current Price per pound
                  ## Probably need to input further years...better yet this should be dynamic based
                  ## on the length of the drought action and simulation
                  numericInput("p.calf.t0", "Current Prices Recieved (per pound)", 1.45, min = 0, step = .01),
                  
                  # Button to update information
                  actionButton("update", "Update Information")
                  
           )
         )
),

output$map <- renderLeaflet({
  leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    setView(lng = -93.85, lat = 37.45, zoom = 4)
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

tabPanel("Results",
         fluidPage(
           titlePanel("Simulation Results"),
           sidebarLayout(
             sidebarPanel(width = 3,
                          checkboxGroupInput("show_outs", "Columns to Display",
                                             filter.titles$cols, selected = filter.titles$cols
                          ),
                          actionButton("gen.res", "Generate Results")
             ),
             mainPanel(
               fluidRow(
                 column(4,
                        selectInput("filter.strat", "Adaptation Strategy",
                                    c("All", filter.titles$opt))
                 ),
                 column(4,
                        selectInput("filter.year", "Year", 
                                    c("All", filter.titles$yr))),
                 column(4,
                        selectInput("filter.ins", "Insurance",
                                    c("All", filter.titles$ins)))
               ),
               fluidRow(
                 DT::dataTableOutput("table")
               )
             )
           )
         )
),
tabPanel("Graphs",
         sidebarLayout(
           sidebarPanel(
             radioButtons("graph.type", "Select Graph", choices = c("Income", "Net Worth"))
           ),
           mainPanel(
             plotOutput("plot1")
           )
         )
)


),

filter.titles <- list("opt" = c("Buy Feed", "Drought and No Adaptation", "No Drought", "Rent Pasture", "Sell Pairs"), 
                      "yr" = c(1950:2017), "ins" = c("With Insurance","No Insurance"),
                      "cols" = c("opt", "yr", "ins", "rev.calf", "rev.ins", "rev.int", "rev.tot", 
                                 "cost.op", "cost.ins", "cost.int", "cost.tot", "profit", "taxes", 
                                 "aftax.inc", "cap.sales", "cap.purch", "cap.taxes", "assets.cow", 
                                 "assets.cash", "net.wrth", "sim.index"))
shapeDrawn <- F


advanceCurrentYear <- function(){
  currentYear <<- currentYear + 1
}

getWinterInfo <- function(currentYear){
  tagList(
    h4("Winter Finance Assessment"),
    p(paste0("Your Current Net Worth is: $", myOuts[currentYear, net.wrth])),
    p(paste0("Your Current Herd is: ", myOuts[currentYear, herd])),
    p(paste0("Your Bank Balance is: $", myOuts[currentYear, assets.cash])),
    p(paste0("Your range is currently at: ", myOuts[currentYear, forage.potential] * 100, "%")),
    p(paste0("You paid: $", myOuts[currentYear, cost.ins], " for insurance"))
  )
}