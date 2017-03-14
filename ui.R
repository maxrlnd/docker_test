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

fluidPage("Ranch Drought", id = "navBar",


tabsetPanel(
  
  
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
      column(6,
        uiOutput("winterInfo"),
        fluidRow(column(12, style = "background-color:white;", div(style = "height:50px;"))),
        uiOutput("decision1")
      ),
      column(2,
        fluidRow(column(12, style = "background-color:white;", div(style = "height:170px;"))),
        actionButton("year1Start", "Begin Simulation")
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
)
)
