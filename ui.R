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
             h4("Background"),
             p("This is background on what the model is and what it does blah blah balh"),
             h4("Instructions"),
             p("These are instructions for what you should do"),
             h4("Release/Agreeement"),
             p("You must agree to continue"),
             actionButton("agree", "I Agree")
      )
    )),
  
  ## Panel for users to enter demographics         
  tabPanel("Demographics",
    fluidRow(
      column(3,
        selectInput("gender", "Select Gender", c("No answer", "Female", "Male")),
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
      
        
    )), 
 tabPanel("Quiz",
    fluidRow(
      column(12,
          textInput("ranchSizeQ", "How Big is your Ranch?"),
          textInput("herdSizeQ", "How many cows do you have in your herd?"),
          radioButtons("cullQ", 'What does it mean to "cull" a cow', 
                      choices = c("To keep a cow in the herd to breed in the following year" = "keep",
                                  "To sell a cow" = "sell"), width = "100%"),
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
                       choices = c("Change" = "change", "Stay the same" = "same"))
          
      )
    )
  )
 
  
), 
## Code to insert new tabs, these get inserted into the main panel tabset via the JS at top
uiOutput("creationPool", style = "display: none;")
)
)


