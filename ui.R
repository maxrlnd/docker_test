# TODOs
  # Add dollar signs to all of the dollar inputs
  # Change some numeric inputs to text inputs and create a reactive
  #   statement in the server to change them to numerics (http://stackoverflow.com/questions/24960407/r-shiny-numeric-input-without-selectors)


library(shiny)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(shinyjs)

source("R/load.R")
source("R/dynamicFunctions.R")
source("R/shinny_support.R")
source("R/weaning_success.R")

## Code to disable tab
jscode <- '
shinyjs.init = function() {
$(".nav").on("click", ".disabled", function (e) {
e.preventDefault();
return false;
});
}
'

css <- '
.disabled {
background: white !important;
cursor: default !important;
color: white !important;
}
'

navbarPageWithJS <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}

#### Setup ####

# Populate a new environment with station gauge info.
# Default location is CPER site
station.gauge <- getStationGauge()

# Populate a new environment with constant (user) variables
constvars <- getConstantVars()

practiceVars <- getSimVars(
  station.gauge,
  constvars,
  start_year = 2002,
  sim_length = 3,
  use.forage = T,
  random.acres=FALSE,
  random.productivity=TRUE,
  acres = 3000)

simvars <- getSimVars(
  station.gauge,
  constvars,
  start_year = 2002,
  sim_length = 10,
  use.forage = T,
  random.acres=FALSE,
  random.productivity=TRUE,
  acres = 3000)

practiceRuns <- (append(append(station.gauge, constvars), (practiceVars)))
simRuns <- (append(append(station.gauge, constvars), (simvars)))

practiceOuts <- createResultsFrame(practiceRuns)
outs <- createResultsFrame(simRuns)

currentYear <- 1

practiceYear <- 1


##
tagList(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "init"),
  tags$style(css),
  tags$head(tags$script('Shiny.addCustomMessageHandler("myCallbackHandler",
                          function(typeMessage) {console.log(typeMessage)
                          if(typeMessage == 1){
                          console.log("got here");
                          $("a:contains(Demographics)").click();
                          }
                          if(typeMessage == 2){
                          $("a:contains(Select Data range)").click();
                          }
                          });
                          ')),

navbarPage("Ranch Drought", id = "navBar",

 uiOutput("test"),
 # for(i in 1:3){
 #   uiOutput("test")
 # }
 tabPanel("Instructions",
    fluidRow(
      column(12,
             checkboxInput("foo", "Disable tab2", T),
             h4("Background"),
             p("This is background on what the model is and what it does blah blah balh"),
             h4("Instructions"),
             p("These are instructions for what you should do"),
             h4("Release/Agreeement"),
             p("You must agree to continue"),
             actionButton("agree", "I Agree")
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
  
  tabPanel("Year 1",
    fluidRow(
      h4("Year 1 Winter Finance Assessment"),
      p(paste("Your Current Networth is:"))
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
)


