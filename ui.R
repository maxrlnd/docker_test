# TODOs
  # Add dollar signs to all of the dollar inputs
  # Change some numeric inputs to text inputs and create a reactive
  #   statement in the server to change them to numerics (http://stackoverflow.com/questions/24960407/r-shiny-numeric-input-without-selectors)

 
## HTML and JS to dynamically create tabs and move between tabs
tagList(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "init"),
  extendShinyjs(text = "shinyjs.closewindow = function() { window.close(); }"),
  extendShinyjs(text = jsResetCode),
  tags$style(css),
  tags$head(
    tags$style(HTML("
    .shiny-output-error-validation {
    color: red;
    }
    ")),
    tags$script(paste0('Shiny.addCustomMessageHandler("myCallbackHandler",
                  function(typeMessage) {console.log(typeMessage)
                  if(typeMessage == 6){
                  console.log("got here");
                  $("a:contains(Background)").click();
                  }
                  if(typeMessage == 7){
                  console.log("got here");
                  $("a:contains(Quiz)").click();
                  }
                  if(typeMessage == 1){
                  console.log("got here");
                  $("a:contains(Year-1)").click();
                  }', yearHandler, '
                  });')
    ),
    tags$script(
      '
      Shiny.addCustomMessageHandler("scrollCallbackIns",
      function(msg) {
      console.log(msg)
      window.scrollTo(0, document.getElementById(msg).getBoundingClientRect().bottom + 680);
      }
      );'
  ),
  tags$script(
    '
    Shiny.addCustomMessageHandler("scrollCallbackCow",
    function(msg) {
    console.log(msg)
    window.scrollTo(0, document.getElementById(msg).getBoundingClientRect().bottom + 915);
    }
    );'
  ),
  tags$script(
    '
    Shiny.addCustomMessageHandler("scrollCallbackTop",
    function(msg) {
    console.log(msg)
    window.scrollTo(0, document.body.top);
    }
    );'
  ),
  tags$script(
    '
      Shiny.addCustomMessageHandler("scrollCallbackRain",
      function(msg) {
      console.log(msg)
      window.scrollTo(0, document.getElementById(msg).getBoundingClientRect().top + 200);
      }
      );'
  )),



fluidPage("Ranch Drought", id = "navBar",


tabsetPanel(id = "mainPanels",
  
 ## Instruction panel
 tabPanel("Welcome",
          fluidRow(
           # textInput("code", "Enter Code to be Run"),
           # actionButton("runCode", "Run Code"),
           # textInput("insChange", "Enter True or False to use insurance or not"),
           # actionButton("applyInsChange", "Change Insurance"),
           # actionButton("reset_button", "Reset Page"),
           # br(),
            textInput("user.ID", "Enter your mTurk code"),
            actionButton("simStart", "Begin Ranch Game"),
            br(),
            br(),
            span((startTime <<- Sys.time()), style="color:white")
            # actionButton("saveInputs", "Save all Input")
          )),
 # tabPanel("Input",
 #          fluidRow(
 #            column(2,
 #                   h4("Drought Facts"),
 #                   
 #                   # Start of the drought year
 #                   numericInput("act.st.yr", "Year", 2002, min = 1900, step = 1)
 #                   
 #                  
 #            ),
 #            column(2, offset = 1,
 #                   h4("Cows"),
 #                   
 #                   # Set Herd size
 #                   ## The defaults should probably be changed here based on domain knowledge
 #                   numericInput("herd", "Herd", 600, min = 0, step = 1),
 #                   
 #                   # Average weight of a cow
 #                   ## I'd like to add lbs to the end of this statement
 #                   numericInput("cow.wt", "Average Cow Weight", 1200, min = 1, step = 1),
 #                   
 #                   # Current value of a cos
 #                   numericInput("p.cow", "Current Value of Cows (per cow)", 850, min = 1),
 #                   
 #                   # Cows Culled per year
 #                   numericInput("cull.num", "Number of Cows Culled in a Normal Year", .16, min = 0, max = 1, step = 1),
 #                   
 #                   # Cow Cost
 #                   numericInput("cow.cost", 'Annual "Cow" Cost', 500, min = 1)
 #            ),
 #            column(2, offset = 1,
 #                   
 #                   h4("Other Information"),
 #                   
 #                   # Price of hay
 #                   ## This should really have a dollar sign infront of it but not sure how to do it right now
 #                   numericInput("p.hay", "Price of Hay ($/Ton)", 100, min = 1),
 #                   
 #                   # Cost of Pasture
 #                   numericInput("past.rent", "Pasture (per AUM)", 16.49, min = 1),
 #                   
 #                   # Interest rate on Borrow Money
 #                   numericInput("loan.int", "Interest Rate for Borrowed Money", .065, min = 0, step = .001),
 #                   
 #                   # Interest RAte for Invested Money
 #                   numericInput("invst.int", "Interest Rate for Invested Money", .0125, min = 0, step = .001),
 #                   
 #                   ## Excel model has average tax basis per cow but R model doesn't have this
 #                   
 #                   # Total (fed + state) on income
 #                   numericInput("cap.tax.rate", "Total Income Tax Rate (US & State)", .19, min = 0, max = 1)
 #            ),
 #            column(2, offset = 1,
 #                   h4("Calves"),
 #                   
 #                   # Average weaning percent
 #                   ## This really needs to be changed to a text box with a 
 #                   ## percent symbol after it
 #                   numericInput("normal.wn.succ", "Average Weaning Percentage", .88, min = 0, max = 1),
 #                   
 #                   # Percent of calfs sold
 #                   numericInput("calf.sell", "Average Percent of Calves Sold", .77, min = 0, max = 1),
 #                   
 #                   # Current Weight of calves
 #                   numericInput("calf.wt", "Average Weight Currently (lbs)", 375, min = 1), 
 #                   
 #                   # Calf weight at weaning
 #                   numericInput("expected.wn.wt", "Average Weight at Weaning (lbs)", 600, min = 1),
 #                   
 #                   # Current Price per pound
 #                   ## Probably need to input further years...better yet this should be dynamic based
 #                   ## on the length of the drought action and simulation
 #                   numericInput("p.calf.t0", "Current Prices Recieved (per pound)", 1.45, min = 0, step = .01),
 #                   
 #                   # Button to update information
 #                   actionButton("update", "Update Information")
 #                   
 #            )
 #          )
 # ),
 
 # tabPanel("Instructions",
 #    fluidRow(
 #      column(12,
 #             h4("Instructions"),
 #             p("Thank you for participating in our study. In this study, you 
 #                will be participating in a game where you act as the owner of a 
 #                cow-calf ranch. Please read these instructions carefully before 
 #                you proceed to the next screen."),
 #             p("First, you will read information you’ll need to “run” a ranch in 
 #               this game. Read this information carefully, but don’t worry about 
 #               memorizing everything. This information will be available to you 
 #               throughout the simulation by clicking back on the information 
 #               tabs."),
 #             p("Next, you will take a comprehension quiz. You can look back at 
 #                the information and instructions while you take the quiz, but 
 #                you must answer all questions correctly to continue the game. If 
 #                you do not answer them correctly the first time, you will have 
 #                two more chances to answer the questions you missed."),
 #             p("After you pass the comprehension quiz, you will do five practice 
 #               rounds of the ranching game. What you do during these practice 
 #               rounds will have NO impact on your game that counts. Feel free to 
 #               play around with your decisions to better understand how they 
 #               affect your outcomes."),
 #             p("Next, you will play ten rounds of the ranching game. The 
 #               rainfall will be different in each year and is drawn from a 
 #               historical distribution of rainfall in Colorado. The rainfall 
 #               will be different than in the practice rounds. Make the best 
 #               choices you can about buying hay and selling cows and calves. 
 #               These choices will affect your net worth at the end of the game."),
 #             p("At the end of the game, your net worth (your bank account plus 
 #              the value of the herd) will be translated into a real-world MTurk 
 #              bonus at a rate of $100,000 of simulation money to $1 of real 
 #              world money. You will receive your bonus within one month of 
 #              completing this study."),
 #             p("Next you will answer a series of basic demographic questions 
 #               that we will only use for control variables, you will not be 
 #               identified based on your answers."),
 #             p(" Finally, we will ask you to choose from a series of ten 
 #              lotteries. One person in this study will be chosen and one 
 #              randomly selected choice will be implemented. The winner will 
 #              receive a bonus according to the lottery that is chosen."),
 #             p("You must complete the full study to be eligible for the bonuses. 
 #              You may skip demographic questions that you do not feel 
 #              comfortable answering, but you must continue to the end of the 
 #              survey and complete the final round of questions."),
 #             br(),
 #             
 #             h4("Release/Agreeement"),
 #             p("You must agree to continue"),
 #             actionButton("agree", "I Agree")
 #      )
 #    )),
 # 
 # 
 # tabPanel("Background Info",
 #          fluidRow(
 #            column(12,
 #              h4("Ranching Information"),
 #              p("You run a ranch which breeds and raises calves to be sold at 
 #                auction."),
 #              p("Calves are born in early spring and are raised by drinking milk 
 #                from their mother until they reach a weight of about 600 pounds. 
 #                Once the calves stop taking milk from their mothers they are 
 #                called weaned calves. Because the calves gain weight throughout 
 #                the summer from their mothers’ milk, it is important that the 
 #                mother cows are well fed and healthy."),
 #              p("Each year, most weaned calves and some mother cows are sold at 
 #                auction in October. If a weaned calf is not sold it will stay on 
 #                the ranch for two years until it reaches maturity and can be 
 #                bred. If a mother cow is sold at auction it is called culling."),
 #              p("There is a delicate balance between the size of a ranch and the 
 #                number of mother cows and calves that can be kept there. If too 
 #                many cattle are grazing on the ranch, they will eat the grass 
 #                down to the dirt and it won’t grow back as well for the next 
 #                season. This means there is less food for next year’s herd. If 
 #                too few cattle are present on the land the ranch cannot be 
 #                profitable and stay in business."),
 #              p("The ranch operates by allowing the mother cows to graze on 
 #                pasture land. In years with less precipitation the ranch must 
 #                purchase extra hay because there is less grass available and 
 #                calves will not be able to reach their target weight. Skinny 
 #                calves mean less revenue when they go to market. Feeding the 
 #                mother cows more hay keeps the mother cows healthy and able to 
 #                produce milk for the calves. Also, if the mother cows aren’t 
 #                healthy, they won’t produce as many cows for the next season."),
 #              p("(Insurance treatment only)"),
 #              p("The rainfall each year is unpredictable, but it can have a big 
 #                impact on ranchers’ bottom lines. To help protect their income, 
 #                ranchers can purchase insurance that will send them a check when 
 #                droughts occur during months when they normally grow a lot of 
 #                grass. The worse the drought, the bigger the check. The 
 #                insurance payment is based off the total rainfall per month as 
 #                compared to historical normal: if the rainfall is below the 
 #                historical norms then you are entitled to an insurance payout."),
 #              br(),
 #              
 #              h4("Facts About Your Ranch"),
 #              tags$li("Your ranch is located about an hour northeast of Denver, 
 #                Colorado and includes 3000 acres of private land."),
 #              tags$li("Your herd has 600 cows and 528 calves."),
 #              tags$li("On your ranch, calves are born between February and 
 #                March, and are sold in October."),
 #              tags$li("Under normal conditions, your ranch has a calf birth and 
 #                weaning success rate of 88%."),
 #              tags$li("Your ranch usually sells 75% of calves and 15% of mother 
 #                cows each year."),
 #              tags$li("The expected weight for a calf when it is ready to sell 
 #                is 600lb, while a mother cow will be about 1,400lb when sold."),
 #              tags$li("The market price for calves is $1.45/lb. Mother cows are 
 #                sold for $850 per cow."),
 #              tags$li("Your ranch can support 600 cow-calf pairs on a total of 
 #                5000 acres without damaging the grass in a normal year."),
 #              tags$li("Each mother cow/calf have a normal yearly operating cost 
 #                of $500, and your ranch has an extra yearly base operating cost 
 #                of $20,000."),
 #              tags$li("(Insurance only) Each year, you buy $X of insurance for 
 #                your ranch. The biggest payout you could get is $Y."),
 #              br(),
 #              
 #              h5("In the next section, you will take a quiz to ensure that you
 #                 know the basic information you will need in this game."),
 #              actionButton("simStart", "Begin Ranch Game")
 #              
 #            )
 #          )),
  
 #  ## Panel for comprehension quiz
 # tabPanel("Quiz",
 #  fluidRow(
 #    column(5,
 #           h4("Comprehension Quiz"),
 #           h5("You may look back at the background info tab to find the 
 #              information you need to answer these questions."),
 #           numericInput("ranchSizeQ", "What is the size of your ranch (in acres)?", 
 #                        0, min = 0, step = 100),
 #           br(),
 #           textOutput("ranchVal"),
 #           numericInput("herdSizeQ", "How many cows do you have in your herd (not 
 #                        including calves or yearlings)?", 0, min = 0, step = 100),
 #           textOutput("herdSizeVal"),
 #           radioButtons("cullQ", "What does it mean to 'cull' a cow?", 
 #                        c("To keep a cow in the herd to breed in the following year",
 #                          "To sell a cow")),
 #           textOutput("cullVal"),
 #           checkboxGroupInput("weanQ", "What happens if you keep a weaned calf instead of selling it (select all that apply)?",
 #                              choices = c("You earn revenue from the sale" = "sale",
 #                                          "Your herd will grow" = "grow",
 #                                          "Your herd will shrink" = "shrink",
 #                                          "You will produce more calves in the year after next" = "wean",
 #                                          "You will create more grazing pressure on your land, eventually leading to possible damages to your forage production" = "land")),
 #           textOutput("weanVal"),
 #           textInput("lHerdQ", "What is the largest herd you can keep on your land without causing damage if rainfall is normal?"),
 #           textOutput("lHerdVal"),
 #           checkboxGroupInput("bigHerdQ", "What happens if you increase the size of your herd beyond the recommended maximum (Select all that apply)",
 #                              choices = c("You will produce more calves" = "moreCalves",
 #                                          "You will damage your land, especially if there is not enough rainfall" = "damage")),
 #           textOutput("bigHerdVal"),
 #           radioButtons("priceQ", "In this game, do calf prices change each year or stay the same?", 
 #                        choices = c("Change" = "change", "Stay the same" = "same")),
 #           textOutput("priceVal")
 #    ),
 #        column(5,
 #           br(),
 #           checkboxGroupInput("adaptQ", "What will likely happen if there is not enough rain and 
 #                        you do not buy sufficient hay?", 
 #                        choices = c("Your calves will be underweight and will generate less revenue at market." = "underweight", 
 #                                    "Your cows will produce fewer calves next year." = "fewerBirths",
 #                                    "Your calves will have higher rates of mortality which leaves you with fewer calves to sell at market." = "fewerCalves",
 #                                    "You will have to cull more cows." = "cullMore")),
 #           textOutput("adaptVal"),
 #           selectInput("earningsQ", "Your net worth at the end of the game will be translated into real-life bonus money 
 #                      at a rate of $100,000 game money to $1 real money. So if you have $300,000 in the net worth (bank 
 #                      account plus the value of your herd), how much bonus money will you get after the game ends?",
 #                       choices = c("","$0", "$3", "$6", "$10")),
 #           textOutput("earningsVal"),
 #           selectInput("practiceQ", "True or False, before starting the game, you will play five “practice rounds” that 
 #                      will not count towards your final net worth.", choices = c("", "True", "False")),
 #           textOutput("practiceVal"),
 #           selectInput("bonusQ", "If you do not complete the survey after the simulation, will you receive your bonus?",
 #                       choices = c("", "Yes", "No")),
 #           textOutput("bonusVal"),
 #           
 #           #INSURANCE TREATMENT ONLY #Randomize
 #           uiOutput("insuranceQuiz"),
 #           actionButton("quizSub", "Submit"),
 #           renderText("quizWarning")
 #           )
 #  )),
 #                   
 #  ## Panel for users to enter demographics         
 #  tabPanel("Demographics",
 #    fluidRow(
 #      br(),
 #      column(5,
 #        
 #        selectInput("gender", "Select Gender", c("", "Female", "Male", "Other")),
 #        textInput("age", "Enter Age"),
 #        checkboxGroupInput("race", "Please specify your race (select all that apply)", 
 #                           choices = c("American Indian or Alaskan Native", "Asian",
 #                                       "Black or African American", "Native Hawaiian or Other Pacific Islander",
 #                                       "White", "Other")),
 #        checkboxGroupInput("ethnicity", "Please specify your ethnicity (select all that apply)",
 #                           choices = c("Hispanic or Latino", "Not Hispanic or Latino")),
 #        radioButtons("attn", "Please select five if you are still reading this question",
 #                     choices = c("1", "2", "3", "4", "5")),
 #        textInput("country", "In which country do you reside?"),
 #        selectInput("state", "In which state do you reside?",
 #                  choices = c("", "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
 #                              "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
 #                              "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
 #                              "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
 #                              "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
 #                              "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
 #                              "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
 #                              "Wyoming", "None of the above"), selected = ""),
 #        conditionalPanel(condition = "input.state == 'None of the above'", 
 #                        textInput("state_specify", "You chose none of the above. Please specify the state in which you live:")),
 #        textInput("zip", "What is your five digit zip code?")
 #        ),
 #      column(5,
 #        selectInput("ranchKnowledge", "Before this experiment, how much did you know about cattle ranching?",
 #                    choices = c("", "Nothing", "I’ve read or talked about cattle ranching at least once before",
 #                                "I am familiar with cattle ranching", "I am very knowledgeable about cattle ranching")),
 #        selectInput("droughtKnowledge", "Before this experiment, how much did you know about drought?",
 #                    choices = c("", "Nothing", "I’ve read or talked about drought at least once before",
 #                                "I am familiar with the concept of drought", "I am very knowledgeable about the concept of drought")),
 #        selectInput("droughtImpact", "Does drought impact your life, and, if so, to what degree?",
 #                    choices = c("", "No, drought has no impact on my life", "No, drought has little impact on my life",
 #                                "Yes, drought has impacted my life a little bit", "Yes, to a high degree")),
 #        selectInput("veg", "Are you a vegetarian or vegan?", choices = c("", "Yes", "No")),
 #        selectInput("income", "What was your total household income before taxes from all sources in 2016?",
 #                    choices = c("", "Less than $15,000", "$15,000 to $29,999", "$30,000 to $44,999", "$45,000 to $59,999",
 #                                "$60,000 to $74,999", "$75,000 to $89,999", "$90,000 to $104,999", "$105,000 to $150,000",
 #                                "$150,000 and up")),
 #        selectInput("education", "What is the highest level of education you have completed?", 
 #                    choices = c("", "Less than high school", "High school diploma or equivalent", "Associate degree",
 #                                "Trade school degree or certificate", "Bachelors degree", 
 #                                "Graduate degree (Masters, PhD, MD, JD, etc.)")),
 #        selectInput("attn2", "Do you agree or disagree: please leave this question blank if you are still reading.",
 #                    choices = c("", "Strongly Agree", "Agree", "Disagree", "Strongly Disagree")),
 #        selectInput("vote", "When you vote, if there are only Democrats or Republicans on the ballot, which party do you tend to vote for?", 
 #                    choices = c("", "I nearly always vote for Democrats", "I vote for Democrats more often than I vote for Republicans",
 #                                "Half the time I vote for Democrats and half the time I vote for Republicans", 
 #                                "I vote for Republicans more often than I vote for Democrats", "I nearly always vote for Republicans",
 #                                "I am not eligible to vote.", "I am eligible, but I do not vote.")),
 #        selectInput("enviro", "Do you identify as an environmentalist?", 
 #                    choices = c("", "Yes", "No", "It depends on the issue")),
 #        
 #        actionButton("demoSub", "Submit")
 #        )
 #      
 #        
 # 
 #    )),
 # 
 # ## Panel for risk aversion measures
 # tabPanel("Lotteries",
 #          fluidRow(
 #            br(),
 #            column(12,
 #                   h4("Choose Your Lottery"),
 #                   p("In this section, you will have another chance to earn additional money."),
 #                   p("You will be presented with two different lotteries. Lottery A and Lottery B 
 #                    will have different payoffs with different odds. To have a chance to earn additional 
 #                    money, you will choose between lottery A and lottery B."),
 #                   p("You will make 10 different choices between lotteries."),
 #                   p("We will select ten participants in this study to win one of their lottery choices
 #                    in the form of an MTurk bonus."),
 #                   p("For the winners, one of these choices will be randomly drawn. For that lottery choice, one of the 
 #                    payoffs will be randomly drawn according to the probabilities given in the lottery. 
 #                    The winners will be paid the amount drawn in addition to their previous earnings in this survey.")
 #            ),
 #            column(5,
 #                   br(),
 #                   radioButtons("lottery10_90",
 #                                "Which lottery would you rather play?",
 #                                choices = c("Lottery A: 10% chance of winning $2.00, 90% chance of winning $1.60",
 #                                            "Lottery B: 10% chance of winning $3.85, 90% chance of winning $0.10")),
 #                   radioButtons("lottery20_80",
 #                                "Which lottery would you rather play?",
 #                                choices = c("Lottery A: 20% chance of winning $2.00, 80% chance of winning $1.60",
 #                                            "Lottery B: 20% chance of winning $3.85, 80% chance of winning $0.10")),
 #                   radioButtons("lottery30_70",
 #                                "Which lottery would you rather play?",
 #                                choices = c("Lottery A: 30% chance of winning $2.00, 70% chance of winning $1.60",
 #                                            "Lottery B: 30% chance of winning $3.85, 70% chance of winning $0.10")),
 #                   radioButtons("lottery40_60",
 #                                "Which lottery would you rather play?",
 #                                choices = c("Lottery A: 40% chance of winning $2.00, 60% chance of winning $1.60",
 #                                            "Lottery B: 40% chance of winning $3.85, 60% chance of winning $0.10")),
 #                   radioButtons("lottery50_50",
 #                                "Which lottery would you rather play?",
 #                                choices = c("Lottery A: 50% chance of winning $2.00, 50% chance of winning $1.60",
 #                                            "Lottery B: 50% chance of winning $3.85, 50% chance of winning $0.10"))
 #            ),
 #            column(5,
 #                   br(),
 #                   radioButtons("lottery60_40",
 #                                "Which lottery would you rather play?",
 #                                choices = c("Lottery A: 60% chance of winning $2.00, 40% chance of winning $1.60",
 #                                            "Lottery B: 60% chance of winning $3.85, 40% chance of winning $0.10")),
 #                   radioButtons("lottery70_30",
 #                                "Which lottery would you rather play?",
 #                                choices = c("Lottery A: 70% chance of winning $2.00, 30% chance of winning $1.60",
 #                                            "Lottery B: 70% chance of winning $3.85, 30% chance of winning $0.10")),
 #                   radioButtons("lottery80_20",
 #                                "Which lottery would you rather play?",
 #                                choices = c("Lottery A: 80% chance of winning $2.00, 20% chance of winning $1.60",
 #                                            "Lottery B: 80% chance of winning $3.85, 20% chance of winning $0.10")),
 #                   radioButtons("lottery90_10",
 #                                "Which lottery would you rather play?",
 #                                choices = c("Lottery A: 90% chance of winning $2.00, 10% chance of winning $1.60",
 #                                            "Lottery B: 90% chance of winning $3.85, 10% chance of winning $0.10"))
 #                   )
 #          )),
 
 tabPanel("Ranch Simulation", 
          # CSS tags to control the button colors, .btn is the default state, 
          # focus is what happens after the button is clicked, 
          # hover is the response to a rollover
          tags$head(tags$style(HTML("
                                .btn {
                                    color:rgb(0, 0, 0);
                                    text-align: left;
                                    border-color: rgb(255,255,255);
                                    background-color: rgb(43, 181, 52);}
                                    
                                .btn:focus{
                                    background-color:rgb(255,255,255);
                                    }
                                    
                                    
                                .btn:hover{
                                    #border-color: rgb(255,255,255);
                                    background-color: rgb(255,255,255)
                                    color: rgb(255,255,255);
                                    font-weight: bold;
                                }",
                      # CSS formating for the rollover buttons
                                ".quest{
                                    color:rgb(0, 0, 0);
                                    text-align: left;
                                    border-color: rgb(245,245,245);
                                    background-color: rgb(245, 245, 245);
                                }
                                  .quest:hover{
                                    color:rgb(0, 0, 0);
                                    text-align: left;
                                    border-color: rgb(245,245,245);
                                    background-color: rgb(245, 245, 245);",

                                  ".inTextTips{
                                    color:rgb(0, 0, 0);
                                    text-align: left;
                                    border-color: rgb(245,245,245);
                                    background-color: rgb(245, 245, 245);
                                }
                                  .inTextTips:hover{
                                    color:rgb(0, 0, 0);
                                    text-align: left;
                                    border-color: rgb(245,245,245);
                                    background-color: rgb(245, 245, 245);"
                      ))),
          
          
          
          fluidPage(uiOutput("pageOut"),
                    uiOutput("infoPane")
                    
          )
          
          
 )
 
  
)#, 
## Code to insert new tabs, these get inserted into the main panel tabset via the JS at top


# uiOutput("creationPool", style = "display: none;")

)
)

