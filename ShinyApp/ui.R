
# Define UI for application that assists user to search their Dream Workation
shinyUI(
    
    fluidPage(
        titlePanel(h1("Find Your Dream Workation")),
        
        tabsetPanel(
            
            tabPanel("Motivation",
                     mainPanel(
                         HTML('<center><img src="W1.png" width="50%"></center>'),
                     column(12,
                        checkboxInput("F", label = "Remote-Work: Biggest Legacy Of Covid-19 (Forbes)",
                                      value=FALSE, width='700px'), align='center',
                                      tags$style(type = 'text/css', "label {font-size:25px}")),
                     
                     fluidRow(
                         column(width = 12, align = 'center',
                                conditionalPanel(
                                    condition = "input.F == true",
                                    HTML('<center><img src="F.png" width="70%" height="600px"></center>')))),
                     )
                     ),
            
            tabPanel("Your Preferences",
                     mainPanel(
                         fluidRow(
                                 column(width = 12, align='center',
                                    selectizeInput(inputId = "GeoPara",
                                                   label = div(style = "font-size:20px;",
                                                               style = "color: blue;",
                                                               "Select Your Parameter"),
                                                   choices = rownames(t(df))[5:14]),
                                    tags$head(tags$style(HTML(".selectize-input {height: 40px;
                                                              width: 300px; font-size: 20px}"))),
                                    tags$style("[type = 'number'] {height: 40px;
                                                              width: 300px; font-size: 20px}"),
                                    plotOutput(outputId = "Box_Para", width="60%"))
                                 
                         ),
                         
                         br(),
                         
                         fluidRow(
                         column(4,
                                checkboxInput("Prior", label = "Select Your Preferences (0 = Neutral)",
                                              value=FALSE, width='600px'),
                                align='center'),
                         
                         column(4,
                                checkboxInput("Facet", label = "View All Box Plots",
                                              value=FALSE, width='600px'),  
                                align = 'center'),
                         
                         column(4,
                                checkboxInput("Outs", label = "View Outliers",
                                              value=FALSE, width='600px'),  
                                align = 'center')
                         ),
                         
                         
                         
                    conditionalPanel(
                           condition = "input.Prior == true",  
                           
                           fluidRow(
                               column(width = 3, align='center',
                                      uiOutput('slider1')),
                               
                               column(width = 3, align='center',
                                      uiOutput('slider3')),
                               
                               column(width = 3, align='center',
                                      uiOutput('slider5')),
                               
                               column(width = 3, align='center',
                                      uiOutput('slider7'))
                               ),
                           
                           fluidRow(
                               column(width = 3, align='center',
                                      uiOutput('slider9')),
                               
                               column(width = 3, align='center',
                                      uiOutput('slider11')),
                               
                               column(width = 3, align='center',
                                      uiOutput('slider13')),
                               
                               column(width = 3, align='center',
                                      uiOutput('slider15'))
                           ),
                           
                           
                           fluidRow(
                               column(width = 3), 
                               
                               column(width = 3, align='center',
                                      uiOutput('slider17')),
                               
                               column(width = 3, align='center',
                                      uiOutput('slider19')),
                               
                               column(width = 3)
                           )),
                          
                         br(),          
                    
                         conditionalPanel(
                             condition = "input.Facet == true",
                             
                             fluidRow(
                             column(width = 3, align='center', plotOutput(outputId = "Facet1", width="90%", height="150px")),
                             column(width = 3, align='center', plotOutput(outputId = "Facet2", width="90%", height="150px")),
                             column(width = 3, align='center', plotOutput(outputId = "Facet3", width="90%", height="150px")),
                             column(width = 3, align='center', plotOutput(outputId = "Facet4", width="90%", height="150px"))),
                             
                             fluidRow(
                             column(width = 3, align='center', plotOutput(outputId = "Facet5", width="90%", height="150px")),   
                             column(width = 3, align='center', plotOutput(outputId = "Facet6", width="90%", height="150px")),
                             column(width = 3, align='center', plotOutput(outputId = "Facet7", width="90%", height="150px")),
                             column(width = 3, align='center', plotOutput(outputId = "Facet8", width="90%", height="150px"))),
                             
                             fluidRow(
                             column(width = 3),   
                             column(width = 3, align='center', plotOutput(outputId = "Facet9", width="90%", height="150px")),   
                             column(width = 3, align='center', plotOutput(outputId = "Facet10", width="90%", height="150px")),
                             column(width = 3)
                             )),
                    
                         br(),
                    
                         conditionalPanel(
                             condition = "input.Outs == true",
                             
                             fluidRow(
                                 column(width = 2, align='center', tableOutput(outputId = "outwifi")),
                                 column(width = 2, align='center', tableOutput(outputId = "outCo")),
                                 column(width = 2, align='center', tableOutput(outputId = "outCoffee")),
                                 column(width = 2, align='center', tableOutput(outputId = "outTaxi")),
                                 column(width = 2, align='center', tableOutput(outputId = "outBeer")),
                                 column(width = 2, align='center', tableOutput(outputId = "outRent"))),
                             
                             fluidRow(
                                 column(width=2),     
                                 column(width = 2, align='center', tableOutput(outputId = "outMeal")),   
                                 column(width = 2, align='center', tableOutput(outputId = "outSun")),
                                 column(width = 2, align='center', tableOutput(outputId = "outFun")),
                                 column(width = 2, align='center', tableOutput(outputId = "outInsta")),
                                 column(width=2)))
                         
                         
                         )),
            
            tabPanel("Map Your Options",
                     mainPanel(
                         fluidRow(leafletOutput(outputId = "Geo3")),
                         
                         fluidRow(column(width = 3, align='center', 
                                         uiOutput('slider2')),
                                  
                                  column(width = 3, align='center', 
                                         uiOutput('slider4')),
                                  
                                  column(width = 3, align='center', 
                                         uiOutput('slider6')),
                                         
                                  column(width = 3, align='center',
                                         uiOutput('slider8'))
                         ),
                         
                         fluidRow(
                             column(width = 3, align='center',
                                    uiOutput('slider10')),
                             
                             column(width = 3, align='center',
                                    uiOutput('slider12')),
                             
                             column(width = 3, align='center',
                                    uiOutput('slider14')),
                             
                             column(width = 3, align='center',
                                    uiOutput('slider16'))
                         ),
                         
                         fluidRow(
                             column(width = 3, align='center',
                                    checkboxInput("IRank", "Dream Workation Ranking", value=FALSE)),
                             column(width = 3, align='center',
                                    uiOutput('slider18')),
                             column(width = 3, align='center',
                                    uiOutput('slider20')),
                             column(width = 3, align='center',
                                    sliderInput(inputId = "NumCities",
                                                label = "How Many Cities?",
                                                min=1, max=147,
                                                value=10,
                                                step=1))
                         ),
                         
                         conditionalPanel(
                            condition = "input.IRank == true",
                            fluidRow(column(width = 12, align='center',
                                    tableOutput(outputId = 'Geo3Table'))))
                         )),
                        
            
                tabPanel("Compare 2 Cities",
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 selectizeInput(inputId = 'CityBox1',
                                                label = 'City1',
                                                choices = colnames(citycomp)[3:149],
                                                select = 'New York'),
                                 
                                 selectizeInput(inputId = 'CityBox2',
                                                label = 'City2',
                                                choices = colnames(citycomp)[3:149],
                                                select = 'Singapore'),
                                 
                                 checkboxInput("ZSC", label = "What is Z-Score Scaling?",
                                               value=FALSE, width='600px'),
                                 
                                 checkboxInput("Unscaled", label = "Unscaled Box Plots",
                                               value=FALSE, width='600px'), 
                                 
                                 checkboxInput("Bars", label = "Bar Plots",
                                               value=FALSE, width='600px'),
                                 
                                 checkboxInput("TableComps", label = "Table",
                                               value=FALSE, width='600px')
                                 
                                 
                             ),
                             
                             mainPanel(
                                 plotOutput(outputId = "Box"),
                                 
                                 conditionalPanel(
                                     condition = "input.ZSC == true",
                                     fluidRow(
                                         column(width = 12, align='left',
                                                h2("What is Z-Score Scaling?"),
                                                 htmlOutput("Z2")))),
                                 
                                 conditionalPanel(
                                     condition = "input.Unscaled == true",
                                     fluidRow(plotOutput(outputId = "UnscaledBox"))),
                                 
                                 conditionalPanel(
                                     condition = "input.Bars == true",
                                 fluidRow(
                                          column(width = 1),
                                          column(width = 1, align='center',
                                                 plotOutput(outputId = "Bar1", width="100%")),
                                          
                                          column(width = 1, align='center',
                                                 plotOutput(outputId = "Bar2", width="100%")),
                                          
                                          column(width = 1, align='center',
                                                 plotOutput(outputId = "Bar3", width="100%")),
                                          
                                          column(width = 1, align='center',
                                                 plotOutput(outputId = "Bar4", width="100%")),
                                        
                                          column(width = 1, align='center',
                                                 plotOutput(outputId = "Bar5", width="100%")),
                                          
                                          column(width = 1, align='center',
                                                 plotOutput(outputId = "Bar6", width="100%")),
                                          
                                          column(width = 1, align='center',
                                                 plotOutput(outputId = "Bar7", width="100%")),
                                          
                                          column(width = 1, align='center',
                                                 plotOutput(outputId = "Bar8", width="100%")),
                                          
                                          column(width = 1, align='center',
                                                 plotOutput(outputId = "Bar9", width="100%")),
                                          
                                          column(width = 1, align='center',
                                                 plotOutput(outputId = "Bar10", width="100%"))
                                          
                                                 
                                 )),
                                 
                                 
                                 
                                 conditionalPanel(
                                     condition = "input.TableComps == true",
                                     fluidRow(
                                         column(width = 12, align='center',
                                         tableOutput(outputId = "CityComps"))))
                             
                                 )
                                )),
     
     tabPanel("Your Dream Workation",
              mainPanel(
                  
                  fluidRow(
                      align = "center",
                      h3("CLICK On Your City!"),
                      width=12),
                  
                  fluidRow(
                      column(width=12, align='center',
                             leafletOutput(outputId = "Geo2", width="100%"))),
                  
                  fluidRow(
                      column(width=12, align='center',
                             selectizeInput(inputId = "Para2",
                                            label = "Parameter",
                                            choices = rownames(t(df))[5:14])),
                      column(width=3, align='center',
                             tableOutput(outputId = "MapTable")),
                      column(width=9, align='center',
                             plotOutput(outputId = "Dist2")))
              ))
     
)))
