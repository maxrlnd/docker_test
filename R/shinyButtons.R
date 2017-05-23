
loadButtons <- function(input, output, session, i, rv){
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

}