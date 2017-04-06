# TODOs
  # Add dollar signs to all of the dollar inputs
  # Change some numeric inputs to text inputs and create a reactive
  #   statement in the server to change them to numerics (http://stackoverflow.com/questions/24960407/r-shiny-numeric-input-without-selectors)


## HTML and JS to dynamically create tabs and move between tabs
tagList(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "init"),
  tags$style(css),
  tags$head(
    # tags$script(paste0('Shiny.addCustomMessageHandler("myCallbackHandler",
    #               function(typeMessage) {console.log(typeMessage)
    #               if(typeMessage == 6){
    #               console.log("got here");
    #               $("a:contains(Demographics)").click();
    #               }', yearHandler, '});'
    #               )),
    
    tags$script('Shiny.addCustomMessageHandler("myCallbackHandler",
                  function(typeMessage) {console.log(typeMessage)
                  if(typeMessage == 6){
                  console.log("got here");
                  $("a:contains(Demographics)").click();
                  }
                  if(typeMessage == 1){
                  console.log("got here");
                  $("a:contains(Year 1)").click();
                  }
                  });'
    ),
    tags$script(HTML("
    /* In coherence with the original Shiny way, tab names are created with random numbers. 
                     To avoid duplicate IDs, we collect all generated IDs.  */
                     var hrefCollection = [];
                     
                     Shiny.addCustomMessageHandler('addTabToTabset', function(message){
                     var hrefCodes = [];
                     /* Getting the right tabsetPanel */
                     var tabsetTarget = document.getElementById(message.tabsetName);
                     
                     /* Iterating through all Panel elements */
                     for(var i = 0; i < message.titles.length; i++){
                     /* Creating 6-digit tab ID and check, whether it was already assigned. */
                     do {
                     hrefCodes[i] = Math.floor(Math.random()*100000);
                     } 
                     while(hrefCollection.indexOf(hrefCodes[i]) != -1);
                     hrefCollection = hrefCollection.concat(hrefCodes[i]);
                     
                     /* Creating node in the navigation bar */
                     var navNode = document.createElement('li');
                     var linkNode = document.createElement('a');
                     
                     linkNode.appendChild(document.createTextNode(message.titles[i]));
                     linkNode.setAttribute('data-toggle', 'tab');
                     linkNode.setAttribute('data-value', message.titles[i]);
                     linkNode.setAttribute('href', '#tab-' + hrefCodes[i]);
                     
                     navNode.appendChild(linkNode);
                     tabsetTarget.appendChild(navNode);
                     };
                     
                     /* Move the tabs content to where they are normally stored. Using timeout, because
                     it can take some 20-50 millis until the elements are created. */ 
                     setTimeout(function(){
                     var creationPool = document.getElementById('creationPool').childNodes;
                     var tabContainerTarget = document.getElementsByClassName('tab-content')[0];
                     
                     /* Again iterate through all Panels. */
                     for(var i = 0; i < creationPool.length; i++){
                     var tabContent = creationPool[i];
                     tabContent.setAttribute('id', 'tab-' + hrefCodes[i]);
                     
                     tabContainerTarget.appendChild(tabContent);
                     };
                     }, 100);
                     });
                     "))
    ),

fluidPage("Ranch Drought", id = "navBar",


tabsetPanel(id = "mainPanels",
  
 ## Instruction panel
 tabPanel("Input",
          fluidRow(
            column(2,
                   h4("Drought Facts"),
                   
                   # Start of the drought year
                   numericInput("act.st.yr", "Year", 2002, min = 1900, step = 1)
                   
                  
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
                   numericInput("cull.num", "Number of Cows Culled in a Normal Year", .16, min = 0, max = 1, step = 1),
                   
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
                   numericInput("normal.wn.succ", "Average Weaning Percentage", .88, min = 0, max = 1),
                   
                   # Percent of calfs sold
                   numericInput("calf.sell", "Average Percent of Calves Sold", .77, min = 0, max = 1),
                   
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
 
 tabPanel("Instructions",
    fluidRow(
      column(12,
             h4("Instructions"),
             p("Thank you for participating in our study. In this study, you 
                will be participating in a game where you act as the owner of a 
                cow-calf ranch. Please read these instructions carefully before 
                you proceed to the next screen."),
             p("First, you will read information you’ll need to “run” a ranch in 
               this game. Read this information carefully, but don’t worry about 
               memorizing everything. This information will be available to you 
               throughout the simulation by clicking back on the information 
               tabs."),
             p("Next, you will take a comprehension quiz. You can look back at 
                the information and instructions while you take the quiz, but 
                you must answer all questions correctly to continue the game. If 
                you do not answer them correctly the first time, you will have 
                two more chances to answer the questions you missed."),
             p("After you pass the comprehension quiz, you will do five practice 
               rounds of the ranching game. What you do during these practice 
               rounds will have NO impact on your game that counts. Feel free to 
               play around with your decisions to better understand how they 
               affect your outcomes."),
             p("Next, you will play ten rounds of the ranching game. The 
               rainfall will be different in each year and is drawn from a 
               historical distribution of rainfall in Colorado. The rainfall 
               will be different than in the practice rounds. Make the best 
               choices you can about buying hay and selling cows and calves. 
               These choices will affect your net worth at the end of the game."),
             p("At the end of the game, your net worth (your bank account plus 
              the value of the herd) will be translated into a real-world MTurk 
              bonus at a rate of $100,000 of simulation money to $1 of real 
              world money. You will receive your bonus within one month of 
              completing this study."),
             p("Next you will answer a series of basic demographic questions 
               that we will only use for control variables, you will not be 
               identified based on your answers."),
             p(" Finally, we will ask you to choose from a series of ten 
              lotteries. One person in this study will be chosen and one 
              randomly selected choice will be implemented. The winner will 
              receive a bonus according to the lottery that is chosen."),
             p("You must complete the full study to be eligible for the bonuses. 
              You may skip demographic questions that you do not feel 
              comfortable answering, but you must continue to the end of the 
              survey and complete the final round of questions."),
             br(),
             
             h4("Release/Agreeement"),
             p("You must agree to continue"),
             actionButton("agree", "I Agree")
      )
    )),

 tabPanel("Background Info",
          fluidRow(
            column(12,
              h4("Ranching Information"),
              p("You run a ranch which breeds and raises calves to be sold at 
                auction."),
              p("Calves are born in early spring and are raised by drinking milk 
                from their mother until they reach a weight of about 600 pounds. 
                Once the calves stop taking milk from their mothers they are 
                called weaned calves. Because the calves gain weight throughout 
                the summer from their mothers’ milk, it is important that the 
                mother cows are well fed and healthy."),
              p("Each year, most weaned calves and some mother cows are sold at 
                auction in October. If a weaned calf is not sold it will stay on 
                the ranch for two years until it reaches maturity and can be 
                bred. If a mother cow is sold at auction it is called culling."),
              p("There is a delicate balance between the size of a ranch and the 
                number of mother cows and calves that can be kept there. If too 
                many cattle are grazing on the ranch, they will eat the grass 
                down to the dirt and it won’t grow back as well for the next 
                season. This means there is less food for next year’s herd. If 
                too few cattle are present on the land the ranch cannot be 
                profitable and stay in business."),
              p("The ranch operates by allowing the mother cows to graze on 
                pasture land. In years with less precipitation the ranch must 
                purchase extra hay because there is less grass available and 
                calves will not be able to reach their target weight. Skinny 
                calves mean less revenue when they go to market. Feeding the 
                mother cows more hay keeps the mother cows healthy and able to 
                produce milk for the calves. Also, if the mother cows aren’t 
                healthy, they won’t produce as many cows for the next season."),
              p("(Insurance treatment only)"),
              p("The rainfall each year is unpredictable, but it can have a big 
                impact on ranchers’ bottom lines. To help protect their income, 
                ranchers can purchase insurance that will send them a check when 
                droughts occur during months when they normally grow a lot of 
                grass. The worse the drought, the bigger the check. The 
                insurance payment is based off the total rainfall per month as 
                compared to historical normal: if the rainfall is below the 
                historical norms then you are entitled to an insurance payout."),
              br(),
              
              h4("Facts About Your Ranch"),
              tags$li("Your ranch is located about an hour northeast of Denver, 
                Colorado."),
              tags$li("Your herd has 500 calves and 500 mother cows."),
              tags$li("On your ranch, calves are born between February and 
                March, and are sold in October."),
              tags$li("Under normal conditions, your ranch has a calf birth and 
                weaning success rate of 88%."),
              tags$li("Your ranch usually sells 75% of calves and 15% of mother 
                cows each year."),
              tags$li("The expected weight for a calf when it is ready to sell 
                is 600lb, while a mother cow will be about 1,400lb when sold."),
              tags$li("The market price for calves is $1.45/lb. Mother cows are 
                sold for $850 per cow."),
              tags$li("Your ranch can support 500 cow-calf pairs on a total of 
                5000 acres without damaging the grass in a normal year."),
              tags$li("Each mother cow/calf have a normal yearly operating cost 
                of $500, and your ranch has an extra yearly base operating cost 
                of $20,000."),
              tags$li("(Insurance only) Each year, you buy $X of insurance for 
                your ranch. The biggest payout you could get is $Y."),
              br(),
              
              h5("In the next section, you will take a quiz to ensure that you
                 know the basic information you will need in this game.")
              
            )
          )),
  
  ## Panel for comprehension quiz
 tabPanel("Quiz",
  fluidRow(
    column(12,
           h4("Comprehension Quiz"),
           h5("You may look back at the background info tab to find the 
              information you need to answer these questions."),
           numericInput("ranchSizeQ", "What is the size of your ranch (in acres)?", 
                        0, min = 0, step = 100),
           numericInput("herdSizeQ", "How many cows do you have in your herd (not 
                        including calves or yearlings)?", 0, min = 0, step = 100),
           radioButtons("cullQ", "What does it mean to 'cull' a cow?", 
                        c("To keep a cow in the herd to breed in the following year",
                          "To sell a cow")),
           checkboxGroupInput("weanQ", "What happens if you keep a weaned calf instead of selling it (select all that apply)?",
                              choices = c("You earn revenue from the sale" = "sale",
                                          "Your herd will grow" = "grow",
                                          "Your herd will shrink" = "shrink",
                                          "You will produce more cows in the year after the calf is weaned" = "wean",
                                          "You will create more grazing pressure on your land")),
           textInput("lHerdQ", "What is the largest herd you can keep on your land without causing damage if rainfall is normal?"),
           checkboxGroupInput("bigHerdQ", "What happens if you increase the size of your herd beyond the recommended maximum (Select all that apply)",
                              choices = c("You will produce more calves" = "moreCalves",
                                          "You will damage your land if there is not enough rainfall" = "damage")),
           radioButtons("priceQ", "In this game, do calf prices change each year or stay the same?", 
                        choices = c("Change" = "change", "Stay the same" = "same")),
           checkboxGroupInput("adaptQ", "What will likely happen if there is not enough rain and 
                        you do not buy sufficient hay?", 
                        choices = c("Your calves will be underweight and will generate less revenue at market.", 
                                    "Your cows will be produce fewer cows next year.",
                                    "Your calving success rate will decline meaning fewer calves to sell at market.",
                                    "You will have to cull more cows.")),
           selectInput("earningsQ", "Your net worth at the end of the game will be translated into real-life bonus money 
                      at a rate of $100,000 game money to $1 real money. So if you have $300,000 in the net worth (bank 
                      account plus the value of your herd), how much bonus money will you get after the game ends?",
                       choices = c("$0", "$3", "$6", "$10")),
           selectInput("practiceQ", "True or False, before starting the game, I will play five “practice rounds” that 
                      will not count towards my final net worth.", choices = c("True", "False")),
           selectInput("bonusQ", "If I do not complete the survey after the simulation, will I receive my bonus?",
                       choices = c("Yes", "No")),
           
           #INSURANCE TREATMENT ONLY
           selectInput("premiumQ", "How much does your rain-index insurance cost each year?",
                       choices = c("$0", "$100", "something reasonable")),
           radioButtons("rainmonthsQ", "Your insurance payouts depend on rain in which months?",
                        choices = c("May-June, July-August", "May-June, June-July", 
                                    "February-March, May-June", "July-August, October-November")),
           selectInput("payoutQ", "Would you get a larger insurance payout if you get 5 inches of rain or 2 inches of rain during 
                        a month that is insured?", choices = c("5 inches", "2 inches"))
           )
  )),
                   
  ## Panel for users to enter demographics         
  tabPanel("Demographics",
    fluidRow(
      column(3,
        selectInput("gender", "Select Gender", c("Female", "Male", "Other")),
        textInput("age", "Enter Age")
        ),
      column(4,
        selectInput("experience", "Have you ever worked on a ranch", c("No", "Yes")),
        
        ## You can create dynamic inputs using the uiOutput function see the corresponding section in the
        ## server file under output$exp
        uiOutput("exp"),
        
        ## This is silly just here as an example
        sliderInput("like", "On a scale of 1 to 10 rate how you feel about ranchers with 1 being hate
                    and 10 being love", min = 1, max = 10, value = 5),
        actionButton("begin", "Begin Ranch Game")
        )
      
        
    ))
 
  
), 
## Code to insert new tabs, these get inserted into the main panel tabset via the JS at top
uiOutput("creationPool", style = "display: none;")
)
)


