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