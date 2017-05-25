simCreator <- function(input, output, session, i, rv, simLength, startYear, name = ""){
  # lapply(c("p", "r"), function(j){})
  ## Reactive taglist for the first set of winter info at the start of each year, updates when
  ## myOuts updates
  orgName <- name
  name <- paste0(name, i)
  
  assign(paste0("reactiveWinter", name), reactive({
    input[[paste0("sell", i-1)]]
    if(myOuts[i, herd] == 0){
      myOuts[i, cost.ins := 0]
    }
    
    ID<<- input$user.ID
    myOuts[1, mTurkID := ID]
    
    # Append range health value to a list 
    {appendRangeHealth(ifelse(round(sum(get(paste0("currentZones", name))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", name))()) * 100, 0)), rangeHealthList)}
    # Compute health info for sidebar display
    span(rangeHealth(i),style = "color:white")
    delay(10,session$sendCustomMessage(type = "scrollCallbackTop", 0))
    
    tagList(
      tags$head(tags$style(HTML(
        # CSS formating for the rollover buttons
        ".inTextTips{
        color:rgb(0, 0, 0);
        text-align: left;
        border-color: rgb(255,255,255);
        background-color: rgb(255, 255, 255);
  }
        .inTextTips:hover{
        color:rgb(0, 0, 0);
        text-align: left;
        border-color: rgb(255,255,255);
        background-color: rgb(255, 255, 255);"))),
      br(),
      h3(paste0("Year ", i,": Winter Finance Assessment")),
      p("Before calving season begins, it is time to take account of your herd, range, and financial health."),
      br(),
      plotOutput(paste0("worthPlot", name)),
      tags$li(p("Your herd has ", 
                span(prettyNum(myOuts[i, herd], digits = 0, big.mark=",", scientific=FALSE),style="font-weight:bold;font-size:large"), 
                
                
                " cows, not including calves ",bsButton("calfdesc", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"),bsPopover(id = "calfdesc", title = "Calf Description",content = paste0("Calves are born in early spring and are raised on milk from their mother until they reach a weight of about 600 pounds.Once the calves stop taking milk from their mothers they arecalled weaned calves."))," or yearlings.",bsButton("yearlingdesc", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"),bsPopover(id = "yearlingdesc", title = "Yearling Description",content = paste0("These are cows that are weaned, but not yet reproducing")),"")),
      
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
      if(ifelse(round(sum(get(paste0("currentZones", name))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", name))()) * 100, 0))<100){
        p("Your range is currently at ", span(ifelse(round(sum(get(paste0("currentZones", name))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", name))()) * 100, 0)),style="font-weight:bold;font-size:large;color:red"), "%")
      }else{
        p("Your range is currently at ", span(ifelse(round(sum(get(paste0("currentZones", name))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", name))()) * 100, 0)),style="font-weight:bold;font-size:large;color:green"), "%")
      },
      plotOutput(paste0("RangeHealthPlot", name)),
      br(),
      if(purchaseInsurance == TRUE) {
        h4("Bills Due")
      },
      if(purchaseInsurance == TRUE) {
        p("Your rainfall-index insurance premium is due. You owe $", 
          span(prettyNum(myOuts[i, cost.ins], digits = 0, big.mark=",",scientific=FALSE),style="font-weight:bold;font-size:large;color:red"), ". Please
          enter this amount below to pay your insurance bill.",
          bsButton("insurance", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"),
          bsPopover(id = "insurance", title = "Insurance",
                    content = paste0("The rainfall each year is unpredictable, but it can have a big impact on your bottom line. To help protect income, ranchers purchase insurance that will result in a payment if growing season rainfall is below normal. The only months that matter for your payout are May, June, July and August. The worse the drought, the bigger the check. Each year, any payouts are received at the end of August."),
                    placement = "auto", 
                    trigger = "hover", 
                    options = list(container = "body")))
      },
      if(purchaseInsurance == TRUE) {
        textInput(paste0("insurancePremium", name), 
                  "Please type the amount of the insurance premium below and to pay your bill and continue.",
                  width = "100%")   
      },
      if(purchaseInsurance == TRUE) {
        uiOutput(paste0("premCheck", name))   
      },
      tags$hr(style="border-color: darkgray;"),
      span(rangeHealth(i),style = "color:white"),
      
      # Create an output for the sidebar widget on overall ranch status
      output[[paste0("infoPane", orgName)]] <- renderUI({
        fixedPanel(
          draggable = FALSE, top = 70, left = "auto", right = 20, bottom = "auto",
          
          width = 220, height = "auto",
          wellPanel(
            p(h3("Ranch Overview")), 
            br(), 
            p(h4("Cattle Status:")), 
            p("Cattle in herd:",prettyNum(myOuts[rv$page, herd], digits = 0, big.mark=",", scientific=FALSE), 
              # Tooltip creation, a button with an icon and the popover for the "tip"
              bsButton("infocows", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small")),
            bsPopover(id = "infocows", title = "Cattle in herd",
                      content = paste0("The carrying capacity of your range is about 600 cows. Your herd can grow or shrink depending on how many calves your cows produce and how many cows and calves you sell in the fall. But be careful: if your herd is too large, you will have less grass per cow and you may reduce your range health. If your herd is too small, you may lose out on profits."),
                      placement = "bottom", 
                      trigger = "hover", 
                      options = list(container = "body")
            ),
            
            p("Calves in herd:", prettyNum(get(paste0("calvesAvailable", name))(), digits = 0, big.mark=",", scientific=FALSE),
              
              
              bsButton("infocalves", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small")),
            bsPopover(id = "infocalves", title = "Calves in herd",
                      content = paste0("Your revenues will primarily depend on how many calves you sell and how much each calf weighs."),
                      placement = "bottom", 
                      trigger = "hover", 
                      options = list(container = "body")),
            br(),
            p(h4("Ranch Status:")),
            if("bankBalance">0){
              p("Bank Balance: $", span(prettyNum(get(paste0("bankBalance", name))(), digits = 0, big.mark=",", scientific=FALSE),style="color:black"),             bsButton("infocash", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"))
            }else{
              p("Bank Balance: $", span(prettyNum(get(paste0("bankBalance", name))(), digits = 0, big.mark=",", scientific=FALSE),style="color:black"), 
                bsButton("infocash", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"))
            },
            bsPopover(id = "infocash", title = "Cash Assets",
                      content = paste0("If your balance falls below zero, you will automatically borrow money at 6.5% interest."),
                      placement = "bottom", 
                      trigger = "hover", 
                      options = list(container = "body")),
            if(ifelse(round(sum(get(paste0("currentZones", name))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", name))()) * 100, 0))<100){
              
              p("Range health(%):", span(ifelse(round(sum(get(paste0("currentZones", name))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", name))()) * 100, 0)),style="color:red"), 
                bsButton("infohealth", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"))
            }else{
              p("Range health(%):", span(ifelse(round(sum(get(paste0("currentZones", name))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", name))()) * 100, 0)),style="color:green"),
                bsButton("infohealth", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"))
              
            },
            bsPopover(id = "infohealth", title = "Range Health",
                      content = paste0("There is a delicate balance between the size of a ranch and the number of cattle that graze it. Overgrazing will lead to many problems that reduce the health and productivity of your rangeland. Without a healthy rangeland, you will incur increasingly higher hay costs and see lower cattle weights at sale. Also, these problems are exacerbated under dry conditions and drought, so be especially careful when this occurs and adjust your herd size with the weather."),
                      placement = "left", 
                      trigger = "hover", 
                      options = list(container = "body")),
            
            #if((prettyNum(myOuts[i, assets.cash], digits = 0,
                          #big.mark=",", scientific=FALSE))>=0){
              #p("Bank balance: $", span(prettyNum((myOuts[rv$page, assets.cash]), digits = 0, big.mark=",", scientific=FALSE), style="color:green"),
                #bsButton("infocash", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"))
            #}else{p("Bank balance: $", networth, 
                    #bsButton("infocash", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"))
            #},
            #bsPopover(id = "infocash", title = "Cash Assets",
                      #content = paste0("If your balance falls below zero, you will automatically borrow money at 6.5% interest."),
                      #placement = "bottom", 
                      #trigger = "hover", 
                      #options = list(container = "body")),
            if((prettyNum((myOuts[rv$page, net.wrth] - myOuts[rv$page, assets.cash]), digits = 0,big.mark=",", scientific=FALSE)) > 0){
              p("Value of herd: $", span(prettyNum((myOuts[rv$page, net.wrth] - myOuts[rv$page, assets.cash]), digits = 0,big.mark=",", scientific=FALSE), style="color:green"), 
                bsButton("herdval", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"))
            }else{
              p("Value of herd: $", span(prettyNum((myOuts[rv$page, net.wrth] - myOuts[rv$page, assets.cash]), digits = 0,big.mark=",", scientific=FALSE), style="color:red"), 
                bsButton("herdval", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"))
            },
            bsPopover(id = "herdval", title = "Value of herd",
                      content = paste0("This is the estimated value of your breeding cows at current market prices."),
                      placement = "bottom", 
                      trigger = "hover", 
                      options = list(container = "body")),
            
            if((prettyNum(myOuts[rv$page, net.wrth], digits = 0, big.mark=",", scientific=FALSE))>0){
              p("Net worth: $", span(prettyNum(myOuts[rv$page, net.wrth], digits = 0, big.mark=",", scientific=FALSE), style="color:green"), 
                bsButton("infonet", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"))
            }else{p("Net worth: $", span(prettyNum(myOuts[rv$page, net.wrth], digits = 0, big.mark=",", scientific=FALSE), style="color:red"), 
                    bsButton("infonet", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small")
            )
              
            },
            bsPopover(id = "infonet", title = "Net Assets",
                      content = paste0("This is the current market value of your herd combined with your bank balance."),
                      placement = "bottom", 
                      trigger = "hover", 
                      options = list(container = "body")) 
            
          )
        )
      })
      

      
      )
        }))
  
  assign(paste0("calvesAvailable", name), reactive({
    if(!is.null(input[[paste0("insCont", name)]])){  
      if(input[[paste0("insCont", name)]] == 1){
        calvesAvailable <- 
          myOuts[i, herd] * AdjWeanSuccess(get(paste0("totalForage", name))(), T, simRuns$normal.wn.succ, 1)
      }else{
        calvesAvailable <- myOuts[i, herd] * simRuns$normal.wn.succ
      }
    }else{
      calvesAvailable <- myOuts[i, herd] * simRuns$normal.wn.succ
    }
    return(calvesAvailable)
  }))
  
  assign(paste0("bankBalance", name), reactive({
    balance <- myOuts[i, assets.cash]
    if(!is.null(input[[paste0("year", name, "Start")]])){  
      if(input[[paste0("year", name, "Start")]] == 1){
        balance <- balance - get(paste0("indem", orgName))[[i]]$producer_prem
      }
    }
    if(!is.null(input[[paste0("year", name, "Summer")]])){  
      if(input[[paste0("year", name, "Summer")]] == 1){
        balance <- balance - input[[paste0("d", name, "adaptExpend")]]
      }
    }
    if(!is.null(input[[paste0("insCont", name)]])){  
      if(input[[paste0("insCont", name)]] == 1){
        balance <- balance + get(paste0("indem", orgName))[[i]]$indemnity
      }
    }
    if(!is.null(input[[paste0("sell", name)]])){  
      if(input[[paste0("sell", name)]] == 1){
        balance <- myOuts[i + 1, assets.cash]
      }
    }
    return(balance)
    
        
  }))
  
  ## Creates a reactive to track the current zone weights for each year
  assign(paste0("currentZones", name), reactive({
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
  assign(paste0("totalForage", name), reactive({
    
    ## Establish current state
    myYear <- startYear + i - 1
    herd <- myOuts[i, herd]
    zones <- get(paste0("currentZones", name))()
    
    ## Calculate available forage produced on the land using Nov-Nov as a year
    ## forageProduction = 1 is full feed for a cow-calf pair
    forage.production <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 10, 11, "normal")
    # print(paste("forage production", forage.production))
    
    ## Calculate adaptation intensity based on forage production
    adaptInten <- CalculateAdaptationIntensity(forage.production)
    
    ## Calculate adaptation cost
    fullAdaptCost <-getAdaptCost(adpt_choice = "feed", pars = simRuns, 
                                 days.act = 180, current_herd = herd, intens.adj = adaptInten)
    
    ## Calculate how much of the needed adaptation is being done
    adaptPercent <- ifelse(fullAdaptCost == 0, 0, input[[paste0("d", name, "adaptExpend")]]/fullAdaptCost * (1 - forage.production))
    # print(paste("adaptPercent", adaptPercent))
    
    ## Output new forage that includes forage and adaptation feed
    totalForage <- forage.production + adaptPercent
  }))
  
  ## Reactive to track herd size for each year
  assign(paste0("herdSize", name), reactive({
    
    ## Get cows being sold based on slide position
    cows <- input[[paste0("cow", name, "Sale")]]
    
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
  
  ## Reactive to track revenues for calf and cow sales
  assign(paste0("revenues", name), reactive({
    
    ## Get cows being sold based on slide position
    cows <- input[[paste0("cow", name, "Sale")]]
    calves <- input[[paste0("calves", name, "Sale")]]
    totalForage <- get(paste0("totalForage", name))()
    weanWeight <- round(calfDroughtWeight(simRuns$normal.wn.wt, totalForage), 0)
    
    ## Calculate revenues for the current year based on slider position
    revenues <- cows * simRuns$p.cow + calves *  weanWeight * simRuns$p.wn[1]
    
  }))
  
  #################UI Functions of Year Tabs###################
  
  ## UI for winter Info
  output[[paste0("winterInfo", name)]] <- renderUI({
    tagList(
      h3(paste0("Year ", i, " of ", simLength, ": Ranching Simulation")),
      p("Remember, at the end of the simulation, you'll convert your net worth to a real MTurk bonus. Read the information carefully to make the best decisions."),
      get(paste0("reactiveWinter", name))()
    )
  })
  
  ## Start Button
  output[[paste0("start", name)]] <- renderUI({
    userPay <- gsub(",", "", input[[paste0("insurancePremium", name)]])
    userPay <- tryCatch(as.numeric(gsub("\\$", "", userPay)),
                        warning = function(war)return(0))
    if(!debugMode){
      req(userPay == round(get(paste0("indem", orgName))[[i]]$producer_prem, 0), genericWrong)
    }
    actionButton(paste0("year", name, "Start"), "Next")
  })
  
  ## Display rain info up to July and allow user to choose adaptation level
  output[[paste0("decision", name)]] <- renderUI({
    if(!is.null(input[[paste0("year", name, "Start")]])){  
      if(input[[paste0("year", name, "Start")]] == 1){
        tagList(
          getJulyInfo(i, name, startYear)
        )
      }
    }
  })
  
  ## Error message for incorrect prem deposit
  output[[paste0("premCheck", name)]] <- renderUI({
    userPay <- gsub(",", "", input[[paste0("insurancePremium", name)]])
    userPay <- tryCatch(as.numeric(gsub("\\$", "", userPay)),
                        warning = function(war)return(0))
    req(userPay)
    validate(
      need(userPay == round(get(paste0("indem", orgName))[[i]]$producer_prem, 0), genericWrong)
    )
  })
  
  ## Display Update for insurance info
  output[[paste0("insuranceUpdate", name)]] <- renderUI({
    if(!is.null(input[[paste0("year", name, "Summer")]])){
      if(input[[paste0("year", name, "Summer")]]){
        if(myOuts[i, herd] == 0){
          get(paste0("indem", orgName))[[i]]$indemnity <<- 0
        }
        currentIndem <- prettyNum(get(paste0("indem", orgName))[[i]]$indemnity, digits = 0, big.mark=",",scientific=FALSE)
        tagList(
          br(),
          br(),
          h3(paste0("Year ", i, ": End of Growing Season")),
          if(currentIndem > 0){
            tagList(
              p("You didn't get much rain this summer! In the graph below you can see how much
                it has rained since you decided whether or not to purchase hay (July and August)."),
              plotOutput(paste0("rainGraphSep", name)),
              if(purchaseInsurance == TRUE) {
                p("Since you have rainfall insurance, 
                you get a check to help cover your losses and extra expenses.
                  (Your rainfall insurance pays out when the rain falls significantly below
                  normal in May, June, July, and August.)")
              },
              br(),
              if(purchaseInsurance == TRUE) {
                h4(p("You have received a check for $", span((currentIndem),style="font-weight:bold;font-size:large;color:green"), " from your rain insurance policy."))
              },
              if(purchaseInsurance == TRUE) {
                textInput(paste0("insuranceDeposit", name), 
                          "Please type the amount of the check below and to add the money to your bank account and continue.",
                          width = "100%")
              },
              # actionButton(paste0("deposit", name), "Deposit"),
              uiOutput(paste0("postDeposit", name)),
              uiOutput(paste0("postDepositButt", name))
              )
          }else{
            tagList(
              tags$head(tags$style(HTML(
                # CSS formating for the rollover buttons
                ".inTextTips{
                color:rgb(0, 0, 0);
                text-align: left;
                border-color: rgb(255,255,255);
                background-color: rgb(255, 255, 255);
          }
                .inTextTips:hover{
                color:rgb(0, 0, 0);
                text-align: left;
                border-color: rgb(255,255,255);
                background-color: rgb(255, 255, 255);"))),
              p("You got sufficient rain this summer, so your grass should be in good shape for your cattle! 
                In the graph below you can see how much
                it has rained since you decided whether or not to purchase hay (July and August)."),
              plotOutput(paste0("rainGraphSep", name)),
              if(purchaseInsurance == TRUE) {
                h4("Because rainfall was close to or above normal levels during the growing season, you did not recieve a check for your rain insurance policy")
              },
              if(purchaseInsurance == FALSE) {
                h4("Rainfall was close to or above normal levels during the growing season.")
              },
              if(purchaseInsurance == TRUE) {
                h4(paste0("After your expenditures on hay and insurance, your new bank balance is: $", 
                          prettyNum(myOuts[i, assets.cash] - 
                                      get(paste0("indem", orgName))[[i]]$producer_prem - input[[paste0("d", name, "adaptExpend")]], 
                                    digits = 0, big.mark=",",scientific=FALSE)))  
              },
              if(purchaseInsurance == FALSE) {
                h4(paste0("After your expenditures on hay, your new bank balance is: $", 
                          prettyNum(myOuts[i, assets.cash] - 
                                      get(paste0("indem", orgName))[[i]]$producer_prem - input[[paste0("d", name, "adaptExpend")]], 
                                    digits = 0, big.mark=",",scientific=FALSE)))
              },
              actionButton(paste0("insCont", name), "Next")
              )
                }
              )
      }
    }
  })
  
  output[[paste0("safePlot", i)]] <- renderPlot({
    randomData <- data.table("Year" = c(1,2,3), "Herd" = c(600,500,400))
    qplot(data = randomData, x = as.factor(Year), y = Herd)
  })

  ## Present options to sell cows
  output[[paste0("cowSell", name)]] <- renderUI({
    # if(!is.null(input[[paste0("year", i, "Summer")]])){
    #   if(input[[paste0("year", i, "Summer")]] == 1){
    req(input[[paste0("insCont", name)]])
    # print(get(paste0("totalForage", name))())
    # print( AdjWeanSuccess(get(paste0("totalForage", name))(), T, simRuns$normal.wn.succ, 1))
    tagList(
      getCowSell(get(paste0("totalForage", name))(), AdjWeanSuccess(get(paste0("totalForage", name))(), T, simRuns$normal.wn.succ, 1), i, name),
      plotOutput(paste0("cowPlot", name)),
      br(),
      p("Herd prediction details",bsButton("herdetails", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"),bsPopover(id = "herdetails", title = "Herd Prediction",content = paste0("Keep in mind that yearlings (weaned calves that are not yet producing calves) are not counted in these herd size numbers. You do not have the option to sell yearlings in this game. These herd size predictions also assume that you go back to normal culling and calf sale rates next year. For these reasons, your herd may not go all the way to 0 if you sell off all of your cows and calves."), 
                                                                                                                                                                  placement = "auto", 
                                                                                                                                                                  trigger = "hover", 
                                                                                                                                                                  options = list(container = "body")))
      
    )
    
    #   }
    # }
  })
  
  output[[paste0("profits", name)]] <- renderUI({
    req(input[[paste0("insCont", name)]])
    profit <- get(paste0("revenues", name))() + get(paste0("indem", orgName))[[i]]$indemnity - myOuts[i, herd] * simRuns$cow.cost - input[[paste0("d", name, "adaptExpend")]] - get(paste0("indem", orgName))[[i]]$producer_prem
    print(paste0("Profits: $", profit))
     tagList(
       tags$head(tags$style(HTML(
    ".inTextTips{
    color:rgb(0, 0, 0);
    text-align: left;
    border-color: rgb(255,255,255);
    background-color: rgb(255, 255, 255);
    }
    .inTextTips:hover{
    color:rgb(0, 0, 0);
    text-align: left;
    border-color: rgb(255,255,255);
    background-color: rgb(255, 255, 255);"
       ))),
    
       br(),

    
    # p("Range health(%):", span(ifelse(round(sum(get(paste0("currentZones", name))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", name))()) * 100, 0)),style="color:green"),
    #   bsButton("infohealth", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"))
    
    
       h4(p("Based on your current selections for market sales, your cow-calf revenues for this year are as follows:")),
       h4(p("Cow-calf revenues: $",
           span(prettyNum(get(paste0("revenues", name))(), digits = 2, big.mark = ",", scientific = FALSE),
                style = "font-weight:bold:font-size:Xlarge;color:green"),
         bsButton("revenueFromCows", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"),
    bsPopover(id = "revenueFromCows", title = "Herd Prediction", placement= "top",content = paste0("Each cow sells for $850. Each calf sells for $1.30 per pound. Move the sliders to change your revenues for this year and your herd size for the next few years.")))),

    # Revenues from sales of cows and calves. Currently breaks the ability to use the sliders...
                
       br(),
       if(purchaseInsurance == TRUE){
         h4("Other revenues and costs that will affect your income for the year include:
            operating costs, additional feed costs, insurance payments and revenues, 
            interest earned or paid, and taxes.")
       },
       if(purchaseInsurance == FALSE){
         h4("Other revenues and costs that will affect your income for the year include:
            operating costs, additional feed costs, 
            interest earned or paid, and taxes.")
       },
      # h5(p("Rain-index insurance payouts: $",
      #      span(prettyNum(get(paste0("indem", orgName))[[i]]$indemnity, digits = 2, big.mark = ",", scientific = FALSE),
      #           style = "font-weight:bold:font-size:Xlarge;color:green"),
      #           bsButton("rainInsurance", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"))),
      #           bsPopover(id = "rainInsurance", title = "Rain-index insurance",
      #                     content = paste0("This is how much you were paid from your rain-index insurance this year."),
      #                     placement = "auto",
      #                     trigger = "hover",
      #                     options = list(container = "body")),
      # br(),
      # h5(p("Base operating costs: $",
      #      span(prettyNum(myOuts[i, herd] * simRuns$cow.cost,
      #                     digits = 0, big.mark=",",scientific=FALSE), style = "font-weight:bold:font-size:Xlarge;color:red"),
      #      bsButton("operatingCosts", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"))),  # Costs of operatiting
      #      bsPopover(id = "operatingCosts", title = "Ranch operating costs",
      #                 content = paste0("Your ranch operating costs are based on the size of your herd. Each cow in your herd costs $500 to maintain for the year. Yearlings and calves are not counted in your operating costs."),
      #                 placement = "auto",
      #                 trigger = "hover",
      #                 options = list(container = "body")),
      # h5(p("Extra feed costs: $",
      #     span(prettyNum(input[[paste0("d", name, "adaptExpend")]], digits = 2, big.mark = ",", scientific = FALSE),
      #           style = "font-weight:bold:font-size:Xlarge;color:red"),
      #     bsButton("extraFeedCost", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"))),
      #     bsPopover(id = "extraFeedCost", title = "Extra feed costs",
      #           content = paste0("This is how much you spent on feed earlier in the year."),
      #           placement = "auto",
      #           trigger = "hover",
      #           options = list(container = "body")),
      # h5(p("Rain-index insurance premium cost: $",
      #     span(prettyNum(get(paste0("indem", orgName))[[i]]$producer_prem, digits = 2, big.mark = ",", scientific = FALSE),
      #           style = "font-weight:bold:font-size:Xlarge;color:red"),
      #     bsButton("premiumCost", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"))),
      #     bsPopover(id = "premiumCost", title = "Rain-index costs",
      #           content = paste0("This is how much you spent on your rain-index insurance this year."),
      #           placement = "auto",
      #           trigger = "hover",
      #           options = list(container = "body")),
      # h5(p("Taxes: $",
      #      span(prettyNum(profit * (0.124 + 0.15 + 0.04), digits = 2, big.mark = ",", scientific = FALSE),
      #           style = "font-weight:bold:font-size:Xlarge;color:red"))),
      # 
      # br(),
      if(get(paste0("revenues", name))() + get(paste0("indem", orgName))[[i]]$indemnity
         - myOuts[i, herd] * simRuns$cow.cost - input[[paste0("d", name, "adaptExpend")]] - get(paste0("indem", orgName))[[i]]$producer_prem > 0){

        h4(p("",
             span(prettyNum(profit - profit * (0.124 + 0.15 + 0.04),
                            digits = 2, big.mark = ",", scientific = FALSE),
                  style = "font-weight:bold:font-size:Xlarge;color:white")
             # bsButton("totalProfits", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small")
             ))

      }
      else{
        h4(p("Total profits: $",
             span(prettyNum(get(paste0("revenues", name))() + get(paste0("indem", orgName))[[i]]$indemnity
                            - myOuts[i, herd] * simRuns$cow.cost - input[[paste0("d", name, "adaptExpend")]] - get(paste0("indem", orgName))[[i]]$producer_prem,
                            digits = 2, big.mark = ",", scientific = FALSE),

                  style = "font-weight:bold:font-size:Xlarge;color:white")
             # bsButton("totalProfits", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"))
             ))
      }
      # bsPopover(id = "totalProfits", title = "Total profits",
      #           content = paste0("Your profits are your revenues for the year minus your costs."),
      #           placement = "auto",
      #           trigger = "hover",
      #           options = list(container = "body"))


     )
  })

  
  ## Create a button to continue after selecting adaptation level
  output[[paste0("continue", name)]] <- renderUI({
    if(!is.null(input[[paste0("year", name, "Start")]])){
      if(input[[paste0("year", name, "Start")]] == 1){
        tagList(
          actionButton(paste0("year", name, "Summer"), "Purchase Hay")
        )
      }
    }
  })
  
  output[[paste0("nextButton", name)]] <- renderUI({
    if(!is.null(input[[paste0("sell", name)]])){
      if(input[[paste0("sell", name)]] == 1){
        tagList(
          actionButton(paste0("nextBtn", orgName), "Begin Next Year >")
        )
      }
    }
    
  })
  
  ## Create button to sell calves and cows once decisions are made
  ## Additionally moves simualation to the next year
  output[[paste0("sellButton", name)]] <- renderUI({
    # if(!is.null(input[[paste0("year", name, "Summer")]])){
    #   if(input[[paste0("year", name, "Summer")]] == 1){
    req(input[[paste0("insCont", name)]])    
    tagList(
      actionButton(paste0("sell", name), "Sell Calves and Cows")
    )
    #   }
    # }
  })
  
  
  output[[paste0("postDeposit", name)]] <- renderUI({
    if(input[[paste0("insuranceDeposit", name)]] != ""){
      userIns <- gsub(",", "", input[[paste0("insuranceDeposit", name)]])
      userIns <- tryCatch(as.numeric(gsub("\\$", "", userIns)),
                          warning = function(war)return(0))
      if(!debugMode){
        validate(
          need(userIns == round(get(paste0("indem", orgName))[[i]]$indemnity, 0), genericWrong)
        )
      }
      
      fluidRow(
        
        if(myOuts[i, assets.cash] + get(paste0("indem", orgName))[[i]]$indemnity - 
           get(paste0("indem", orgName))[[i]]$producer_prem - input[[paste0("d", name, "adaptExpend")]] > 0){
          h4(p("After your expenditures on hay and your insurance check, your new bank balance is: $", 
               span(prettyNum(myOuts[i, assets.cash] + get(paste0("indem", orgName))[[i]]$indemnity - 
                                get(paste0("indem", orgName))[[i]]$producer_prem - input[[paste0("d", name, "adaptExpend")]], 
                              digits = 0, big.mark=",",scientific=FALSE), style = "font-weight:bold:font-size:Xlarge;color:green")))
        }
        else{
          h4(p("After your expenditures on hay and your insurance check, your new bank balance is: $", 
               span(prettyNum(myOuts[i, assets.cash] + get(paste0("indem", orgName))[[i]]$indemnity - 
                                get(paste0("indem", orgName))[[i]]$producer_prem - input[[paste0("d", name, "adaptExpend")]], 
                              digits = 0, big.mark=",",scientific=FALSE), style = "font-weight:bold:font-size:Xlarge;color:red")))
          
        }
        
      )
    }
  })
  
  output[[paste0("insSpace", name)]] <- renderUI({
    req(input[[paste0("year", name, "Summer")]])
    fluidRow(column(12, style = "background-color:white;", div(style = "height:1050px;")))
  })
  
  output[[paste0("postDepositButt", name)]] <- renderUI({
    
    if(!is.null(input[[paste0("year", name, "Summer")]])){
      if(get(paste0("indem", orgName))[[i]]$indemnity == 0){
        tagList(
          actionButton(paste0("insCont", name), "Next")
        )
      }else{
        if(debugMode & input[[paste0("insuranceDeposit", name)]] == ""){
          actionButton(paste0("insCont", name), "Continue")
        }else if(input[[paste0("insuranceDeposit", name)]] != ""){
          userIns <- gsub(",", "", input[[paste0("insuranceDeposit", name)]])
          userIns <- tryCatch(as.numeric(gsub("\\$", "", userIns)),
                              warning = function(war)return(0))
          
          if(!debugMode){req(userIns == round(get(paste0("indem", orgName))[[i]]$indemnity, 0))}
          tagList(
            actionButton(paste0("insCont", name), "Next")
          )
        }
      }
    }
  })
  
  ## Table of rain for each July
  output[[paste0("julyRain", name)]] <- renderTable({
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
  
  
  output[[paste0("cowPlot", name)]] <- renderPlot({
    print("starting cow plot")
    if(!is.null(input[[paste0("year", name, "Summer")]])){
      if(input[[paste0("year", name, "Summer")]] == 1){

        cows <- input[[paste0("cow", name, "Sale")]]
        calves <- input[[paste0("calves", name, "Sale")]]
        
        # Current herd size (determined by last years choices)
        herdy0 <- myOuts[i, herd]  
        
        # Next year's herd size
        herdy1 <- get(paste0("herdSize", name))()  
        
        # Herd size for the year after next
        herdy2 <- shinyHerd(herd_1 = herdy1,  # t-1 for year 2 is next years herd size
                            cull_1 = myOuts[1, cows.culled] * herdy1,  # we don't know how many cows they will cull next year. assume stability/default of 16% (draw from )
                            herd_2 = herdy0,  # t-2 for year 2 is this year
                            calves_2 = (floor(herdy0 * AdjWeanSuccess(get(paste0("totalForage", name))(), T, simRuns$normal.wn.succ, 1)) - calves),  # Calves in the herd this year minus those that are sold via the slider input
                            deathRate = simRuns$death.rate)  
        
        years <- c("This Year","Next Year","In Two Years")
        herd.projection <- data.table("Year" = years, "Herd Size" = c(herdy0, herdy1, herdy2))
        herd.projection$Year <- factor(herd.projection$Year, levels = c("This Year", "Next Year", "In Two Years"))
        herd.projection$`Herd Size` = round(herd.projection$`Herd Size`, 0)
        ggplot(herd.projection, aes(x = Year, y = `Herd Size`)) + geom_bar(stat = "identity", width = .3, fill = "#8b4513") +
          geom_text(aes(label = `Herd Size`), size = 10, position = position_stack( vjust = .5), color = "#ffffff") +
          theme(text = element_text(size = 20), axis.title.x=element_blank())
        
      }
    }
  })
  
  ## Bar graph to display net worth
  output[[paste0("worthPlot", name)]] <- renderPlot({
    plotOuts <- myOuts[1:simLength, c("yr", "assets.cow", "assets.cash"), with = F]
    setnames(plotOuts, c("Year", "Value of Cows", "Cash"))
    plotOuts[, Year := startYear:(startYear + nrow(plotOuts) - 1)]
    plotOuts <- melt(plotOuts, id.vars = "Year")
    setnames(plotOuts, c("Year", "Area", "Value in $"))
    plotOuts$Area <- factor(plotOuts$Area)
    plotOuts$YearNumbers <-  paste("Yr", plotOuts$Year - min(plotOuts$Year) + 1)
    plotOuts$YearNumbers <- factor(plotOuts$YearNumbers, 
                                   levels = paste("Yr", seq_along(unique(plotOuts$Year))))
    
    ggplot(plotOuts, aes(x = YearNumbers, y = `Value in $`, fill = Area)) + geom_bar(stat = "identity") + 
      ggtitle("Net Worth") + 
      scale_y_continuous(labels = comma) +
      geom_text(data = subset(plotOuts, `Value in $` !=0), aes(label = dollar(`Value in $`)), 
                size = 5, position = position_stack(vjust = 0.3), angle = 90) +
      theme(legend.title = element_blank(), axis.title = element_text(size = 20), text = element_text(size = 20)) +
      scale_fill_manual(values = c("#f4a460", "#85bb65")) +
      labs(x="Year", y="Value in $")
    
    
  })
  
  output[[paste0("RangeHealthPlot", name)]] <- renderPlot({
    PlotYear <- myOuts[, "yr", with = F]
    print(PlotYear)
    setnames(PlotYear, c("Year"))
    PlotYear[, Year := startYear:(startYear + nrow(PlotYear) - 1)]
    PlotYear <- melt(PlotYear, id.vars = "Year")
    PlotYear$rangeHealthList <- rangeHealthList
    PlotYear$YearNumbers <- c(paste("Yr", seq(1, simLength, length.out = simLength)))
    PlotYear$YearNumbers <- factor(PlotYear$YearNumbers, 
                                   levels = paste("Yr", seq_along(unique(PlotYear$Year))))
    
    ggplot(PlotYear, aes(x = YearNumbers, y = rangeHealthList)) + 
      geom_bar(stat = "identity", fill = "olivedrab") + 
      ggtitle("Range Health") + 
      labs(x = "Year" ,y = "Range Condition (%)") +
      theme(text = element_text(size = 20)) +
      geom_text(aes(label = paste(rangeHealthList, sep = "", "%")), size = 8, color =  "#ffffff", vjust = 5)
    
    
  })
  
  ## Bar graph to display rainfall
  output[[paste0("rainGraph", name)]] <- renderPlot({
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
  output[[paste0("rainGraphSep", name)]] <- renderPlot({
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
    
    #Setting output to only highlight July and August
    yprecip1 = yprecip[, (1:12)[-seq(9,10)] :=0]
    ave1 <- station.gauge$avg
    yearAvg1 <- rbindlist(list(yprecip1, ave1), use.names = T)
    yearAvg1[, "id" := c("Actual Rain", "Average Rain")]
    yearAvg1 <- melt(yearAvg1, id.vars = "id")
    setnames(yearAvg1, c("id", "Month", "Rainfall"))
    
    #ggplot for July and August
    ggplot(yearAvg, aes(x = Month, y = Rainfall, area = id)) + 
      geom_bar(width = .9, stat = "identity", position = 'dodge', alpha = .5) + 
      theme(legend.title = element_blank(), text = element_text(size = 15)) + ggtitle("Rainfall") +
      scale_fill_manual(values = c("#00008b", "#1e90ff")) +
      scale_color_manual(values = c("grey50", "grey50")) +
      ylab("Rainfall (Inches)") +
      xlab("Months") + 
      #Geom__bar only highlighting July and August
      geom_bar(data = yearAvg1, aes(x = Month, y = Rainfall, fill = id), stat = "identity", position = 'dodge')
    
  })
  
  # Reactive to disable start simulation button after they're clicked
  observeEvent(input[[paste0("year", name, "Start")]], {
    shinyjs::disable(paste0("year", name, "Start"))
    delay(100,session$sendCustomMessage(type = "scrollCallbackRain", paste0("rainGraph", i)))
  })
  
  ## Disable cow and calf sliders after sell button
  ## Disable sell button
  ## update myOuts based on forage and the year's decisions
  observeEvent(input[[paste0("sell", name)]], {
    disable(paste0("sell", name))
    disable(paste0("calves", name, "Sale"))
    disable(paste0("cow", name, "Sale"))
    myOuts <<- updateOuts(wean = AdjWeanSuccess(get(paste0("totalForage", name))(), T, simRuns$normal.wn.succ, 1), 
                          totalForage = get(paste0("totalForage", name))(), calfSale = input[[paste0("calves", name, "Sale")]],
                          indem = get(paste0("indem", orgName))[[i]], adaptExpend = input[[paste0("d", name, "adaptExpend")]], cowSales = input[[paste0("cow", name, "Sale")]], 
                          newHerd = get(paste0("herdSize", name))(), zones = get(paste0("currentZones", name))(), 
                          currentYear = i, ID = ID, time = startTime)
  })
  
  
  # Disable continue button and adaptation slider after clicking
  observeEvent(input[[paste0("year", name, "Summer")]], {
    shinyjs::disable(paste0("year", name, "Summer"))
    shinyjs::disable(paste0("d", name, "adaptExpend"))
    delay(100,session$sendCustomMessage(type = "scrollCallbackIns", paste0("rainGraphSep", i)))
  })
  
  observeEvent(input[[paste0("insCont", name)]], {
    shinyjs::disable(paste0("insCont", name))
    delay(100,session$sendCustomMessage(type = "scrollCallbackCow", paste0("cowSell", i)))
  })
}

