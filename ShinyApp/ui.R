
# Define UI for application that assists user to search their Dream Workation
shinyUI(
    
    fluidPage(
        titlePanel(h1("Find Your Dream Workation")),
        
        tabsetPanel(
            
            tabPanel("Motivation",
                     mainPanel(
                         HTML('<center><img src="W1.png" width="50%"></center>'),
                     column(6,
                        checkboxInput("F", label = "Remote-Work: Biggest Legacy Of Covid-19 (Forbes)",
                                      value=FALSE, width='700px'), align='center'),
                     column(6,
                        checkboxInput("McK", label = "No Loss In Productivity Across Sector (McKinsey)",
                                      value=FALSE, width='600px'), align='center', 
                                      tags$style(type = 'text/css', "label {font-size:25px}")),
                     
                     fluidRow(
                         column(width = 6, align = 'center',
                                conditionalPanel(
                                    condition = "input.F == true",
                                    HTML('<center><img src="S.png" width="70%" height="600px"></center>'))),
                         
                         column(width = 6, align = 'center',
                                conditionalPanel(
                                    condition = "input.McK == true",
                                    HTML('<center><img src="McK.png" width="60%" height="600px"></center>'))))
                     )
                     ),
            
                    
            tabPanel("Your Priorities",
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
                                checkboxInput("Prior", label = "Set Your Priorities",
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
            
            tabPanel("Your Dream Workation",
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
                        
            
            
            
                                    
     #       tabPanel("1 City vs 1 Parameter",
     #                sidebarLayout(
     #                    sidebarPanel(
     #                        
     #                        selectizeInput(inputId = 'Ci',
     #                                       label = 'City',
     #                                       choices = df$City),
     #                        
     #                        selectizeInput(inputId = "Para",
     #                                       label = "Parameter",
     #                                       choices = rownames(t(df))[5:14]),
     #                        
     #                        checkboxInput("CRank1", "Rank Cities By Parameter", value=FALSE),
     #                        
     #                        conditionalPanel(
     #                            condition = "input.CRank1 == true",
     #                        
     #                        numericInput(inputId = 'Top',
     #                                     label = 'Ranking: High To Low',
     #                                     value=10, 
     #                                     min=1, 
     #                                    max=75, 
     #                                     step=1),
     #                        
     #                        numericInput(inputId = 'Bot',
     #                                     label = 'Ranking: Low To High',
     #                                    value = 10, 
     #                                     min = 1, 
     #                                     max = 75, 
     #                                     step = 1)
     #                    )),
     #                    
     #                    mainPanel(
     #                            plotOutput(outputId = "Dist"),
     #                            fluidPage(
     #                                checkboxInput("CRank", "Show Ranking", value=FALSE),
     #                                
     #                                conditionalPanel(
     #                                    condition = "input.CRank == true",
     #               
     #                               fluidRow(
     #                                   column(width=6, align="center", h3("Ranking: High To Low"), 
     #                                           tableOutput(outputId = "View")),
     #                                   column(width=3, align="center", h3("Ranking: Low To High"), 
     #                                           tableOutput(outputId = "View2")))
     #                               
     #                               ))
     #                            
     #                            ))),
                     
                   
            tabPanel("Zone In: 2 Cities",
                     sidebarLayout(
                         sidebarPanel(
                             
        
                selectizeInput(inputId = 'CityComp1',
                               label = 'City1',
                               choices = colnames(citycomp)[3:149],
                               select = 'New York'),
                
                selectizeInput(inputId = 'CityComp2',
                               label = 'City2',
                               choices = colnames(citycomp)[3:149],
                               select = 'Singapore'),
                
                checkboxGroupInput("my_choices", "Parameter (Select 1 or All)",
                                   choices = c("Wifi", "Co-Working", "Coffee", "Taxi",
                                               "Beer", "Rentals", "Meals", "Sun",
                                               "Fun", 'Insta', 'All'))),
                
                mainPanel(
                    
                        fluidRow(h2("Comparing 2 Cities Across Parameters", align='center'),
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Wifi' || input.my_choices == 'All'",
                                   plotOutput(outputId = "Bar1", width="90%"))),
                                   
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Co-Working' || input.my_choices == 'All'",
                                   plotOutput(outputId = "Bar2", width="90%"))),
                            
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Coffee' || input.my_choices == 'All'",
                                   plotOutput(outputId = "Bar3", width="90%"))),
                                   
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Taxi' || input.my_choices == 'All'",
                                   plotOutput(outputId = "Bar4", width="90%")))),
                        
                        fluidRow( 
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Beer' || input.my_choices == 'All'",
                                   plotOutput(outputId = "Bar5", width="90%"))),
                            
                            column(width = 6, align='center', 
                                   tableOutput(outputId = "CityComps")),
                            
                            
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Rentals' || input.my_choices == 'All'",
                                   plotOutput(outputId = "Bar6", width="90%")))),
                        
                        fluidRow(
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Meals' || input.my_choices == 'All'",
                                   plotOutput(outputId = "Bar7", width="90%"))),
                            
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Sun' || input.my_choices == 'All'",
                                   plotOutput(outputId = "Bar8", width="90%"))),
                            
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Fun' || input.my_choices == 'All'",
                                   plotOutput(outputId = "Bar9", width="90%"))),
                            
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Insta' || input.my_choices == 'All'",
                                   plotOutput(outputId = "Bar10", width="90%")))),
                        
                        ))),
     
     
            tabPanel("Your City vs Median",
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
                  )),
     
            
                tabPanel("Secret Sauce",
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 selectizeInput(inputId = 'CityBox1',
                                                label = 'City1',
                                                choices = colnames(citycomp)[3:149],
                                                select = 'New York'),
                                 
                                 selectizeInput(inputId = 'CityBox2',
                                                label = 'City2',
                                                choices = colnames(citycomp)[3:149],
                                                select = 'Singapore')
                             ),
                             
                             mainPanel(
                                 plotOutput(outputId = "Box"))
                                 
                                ))
)))
