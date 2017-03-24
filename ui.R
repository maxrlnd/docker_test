# TODOs
  # Add dollar signs to all of the dollar inputs
  # Change some numeric inputs to text inputs and create a reactive
  #   statement in the server to change them to numerics (http://stackoverflow.com/questions/24960407/r-shiny-numeric-input-without-selectors)


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
  tags$head(
    tags$script('Shiny.addCustomMessageHandler("myCallbackHandler",
                  function(typeMessage) {console.log(typeMessage)
                  if(typeMessage == 1){
                  console.log("got here");
                  $("a:contains(Demographics)").click();
                  }
                  if(typeMessage == 2){
                  $("a:contains(Select Data range)").click();
                  }
                  });
                  '),
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
# Important! : 'Freshly baked' tabs first enter here.
uiOutput("creationPool", style = "display: none;")
# End Important
)
)
