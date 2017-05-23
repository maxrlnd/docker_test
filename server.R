`%then%` <- shiny:::`%OR%`
genericWrong <- "This is incorrect please try again"
function(input, output, session) {
  
  ## Reactive value for current year
  values <- reactiveValues("currentYear" = 1, "starting" = TRUE, "saveComplete" = FALSE, "beginSaving" = FALSE)

  observeEvent(input$runCode, {
    eval(parse(text = input$code))
  })
  rv <- reactiveValues(page = 1)
  rvPrac <- reactiveValues(page = 1)
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
    simCreator(input, output, session, i, rv, simLength, startYear)

  }) ##End of lapply
  
  lapply(1:practiceLength, function(i){
    simCreator(input, output, session,i, rvPrac, practiceLength, startYearprac, name = "prac")
  })
  
  
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
  
  observeEvent(input$pracStart, {
    disable("pracStart")
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value='Practice Simulation']")
    updateTabsetPanel(session, "mainPanels", selected = "Practice Simulation")
  })
  
  observeEvent(input$simStart, {
    createOutputs(practiceRuns, simRuns, indem)
    disable("simStart")
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value='Ranch Simulation']")
    updateTabsetPanel(session, "mainPanels", selected = "Ranch Simulation")
  })
  
  
  
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
  
  # output$page <- renderText(rv$page)
  
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < simLength + 1)
    hide(selector = ".page")
    show(sprintf("step%s", rv$page))
  })
  
  observe({
    toggleState(id = "prevBtnprac", condition = rvPrac$page > 1)
    toggleState(id = "nextBtnprac", condition = rvPrac$page < practiceLength + 1)
    hide(selector = ".page")
    show(sprintf("step%s", rvPrac$page))
  })

  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  
  navPagePrac <- function(direction) {
    rvPrac$page <- rvPrac$page + direction
  }
  

  
  
  output$practiceOut <- renderUI({
    if(rvPrac$page <= practiceLength){
      simPageOutput(rvPrac, "prac")  
    }else{
      
      fluidRow(
        hide("infoPanePrac"),
        column(width = 10,
          h4("Done with Practice"),
          actionButton("simStart", "Begin Ranch Game")
        )
      )
    }
  })
  
  output$pageOut <- renderUI({
  
  if(rv$page <= simLength){  
    simPageOutput(rv, name = "")
    

  }else{
    fluidRow(
      hide("infoPane"),
      column(width = 10,
      h4(paste0("Congratulations! You've completed ", simLength, " years of ranching.")),
      br(),
      p(paste0("Through ranching you accumulated $", round(myOuts$assets.cash[simLength + 1], 0), " in cash" )),
      p(paste0("You also have a herd worth $", round(myOuts$assets.cow[simLength + 1], 0), ".")),
      p(paste0("Your total net worth is $", round(myOuts$net.wrth[simLength + 1], 0), ". With a conversation rate of $200,000
               of simulation money to $1 of MTurk bonus money, you've earned $", round(myOuts$net.wrth[simLength + 1]/200000, 2),".")),
      actionButton("saveInputs", "Save results and recieve completion code"),
      span((endTime <<- Sys.time()), style = "color:white"),
      span((simTime <<- endTime - startTime), style = "color:white"),
      uiOutput("complete"),
      offset = .5)
    )
  }
  })

  output$complete <- renderUI({
    req(values$saveComplete)
    h4(p("Your Data has been saved, your completion code is: ", span(sample(10000:99999,1), style="color:green")), p("Please write down your completion code and close this window to finish the last portion of the game."))
  })
  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, navPage(1))
  observeEvent(input$prevBtnprac, navPagePrac(-1))
  observeEvent(input$nextBtnprac, navPagePrac(1))
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
    #saveData$names <- NULL
    # Pivot save data to horizontal
    #saveData <- t(saveData)
    # Remove first row of variable names
    withProgress(message = "Saving Data", value = 1/3, {
    gs_new(title =  ID, 
           input = saveData, trim = TRUE, verbose = TRUE)
    ## These are used to check the output in testing
     #inputsheet <- gs_title(ID)
     #insheet <- gs_read(inputsheet)
    incProgress(1/3)
    
    outputSheet <- gs_title("cowGameInputs")
    gs_add_row(outputSheet, ws=1, input = myOuts)
    ## This is used to validate in testing
     outsheet <- outputSheet %>% gs_read(ws = "Sheet1")
    
    # gs_new(title =  paste0("output", lastFile + 1), 
    #        input = myOuts, trim = TRUE, verbose = TRUE)
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


