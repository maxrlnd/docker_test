tabPanel("Insurance",
         div(class="outer",
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css")
               # includeScript("gomap.js")
             ),
             leafletOutput("map", width = "100%", height = "100%"),
             
             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                           width = 330, height = "auto",
                           
                           h3("Insurance"),
                           # Purchase Insurance?
                           ## Change all further options to be dynamic based on this input
                           selectInput("purchase.insurance", h4("Purchase Insurance"),
                                       c("Yes" = 1, "No" = 0)),
                           selectInput("pfactor",label=h4("Productivity Factor"),choices=60:150,selected=100),
                           selectInput("clv",label=h4("Coverage Level"),choices=seq(70,90,by=5),selected=90),
                           numericInput("acres.param", label = h4("Acres"), 2000, min = 0, step = 100),
                           h4("Insurance Allocation"),
                           column(4,
                                  selectInput("i1",label="Jan-Feb",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i4",label="Apr-May",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i7",label="Jul-Aug",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i10",label="Oct-Nov",
                                              choices=seq(0,1,by=0.1))
                           ),
                           column(4,
                                  selectInput("i2",label="Feb-Mar",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i5",label="May-Jun",
                                              choices=seq(0,1,by=0.1),selected=0.5),
                                  selectInput("i8",label="Aug-Sep",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i11",label="Nov-Dec",
                                              choices=seq(0,1,by=0.1))
                           ),
                           column(4,
                                  selectInput("i3",label="Mar-Apr",
                                              choices=seq(0,1,by=0.1),selected=0.5),
                                  selectInput("i6",label="Jun-Jul",
                                              choices=seq(0,1,by=0.1)),
                                  selectInput("i9",label="Sep-Oct",
                                              choices=seq(0,1,by=0.1))
                           ),
                           actionButton("updateIns", "Update Insurance"),
                           h4(textOutput("text1"))
             )
         )
),

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

output$map <- renderLeaflet({
  leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    setView(lng = -93.85, lat = 37.45, zoom = 4)
})