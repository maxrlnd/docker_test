# TODOs
  # Add dollar signs to all of the dollar inputs
  # Change some numeric inputs to text inputs and create a reactive
  #   statement in the server to change them to numerics (http://stackoverflow.com/questions/24960407/r-shiny-numeric-input-without-selectors)


library(shiny)
library(markdown)
library(leaflet)
library(RColorBrewer)

source("R/load.R")
source("R/dynamicFunctions.R")
source("R/shinny_support.R")
source("R/weaning_success.R")


#### Setup ####

# Populate a new environment with station gauge info.
# Default location is CPER site
getStationGauge()

# Populate a new environment with constant (user) variables
getConstantVars()

##

navbarPage("Ranch Drought",
 #       tags$head(tags$script("
 #    window.onload = function() {
 #        $('#mynavlist a:contains(\"Demographics\")').parent().addClass('disabled');
 #        $('#mynavlist a:contains(\"Input\")').parent().addClass('disabled');
 #        $('#mynavlist a:contains(\"Results\")').parent().addClass('disabled');
 #        $('#mynavlist a:contains(\"Results\")').parent().addClass('disabled');
 #    };
 # 
 #    Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
 #        $('#mynavlist a:contains(\"' + nav_label + '\")').parent().removeClass('disabled');
 #    });
 # ")),
 tabPanel("Instructions",
    fluidRow(
      column(12,
             h4("Background"),
             p("This is background on what the model is and what it does blah blah balh"),
             h4("Instructions"),
             p("These are instructions for what you should do"),
             h4("Release/Agreeement"),
             p("You must agree to continue"),
             radioButtons("agreement", "Agreement", c("I do not agree" = F, "I agree with everything above" = T))
      )
    )),
           
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
                    and 10 being love", min = 1, max = 10, value = 5)
        )
        
    )),
  
  # Input pane with all inputs from Excel UI
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
)
# )


