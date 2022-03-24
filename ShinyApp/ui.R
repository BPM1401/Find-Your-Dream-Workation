
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
            
            tabPanel("10 Parameters",
                     mainPanel(
                         fluidRow(
                                 column(width = 12, align='center',
                                    selectizeInput(inputId = "GeoPara",
                                                   label = div(style = "font-size:20px;",
                                                               style = "color: blue;",
                                                               "Pick YOUR Parameter!"),
                                                   choices = rownames(t(df))[5:14]),
                                    tags$head(tags$style(HTML(".selectize-input {height: 40px;
                                                              width: 300px; font-size: 20px}"))),
                                    tags$style("[type = 'number'] {height: 40px;
                                                              width: 300px; font-size: 20px}"),
                                    plotOutput(outputId = "Box_Para", width="60%")),
                         ),
                         
                         fluidRow(
                         column(3),     
                         column(3,
                                checkboxInput("Outs", label = "View Outliers",
                                              value=FALSE, width='600px'),  
                                align = 'center'),
                         column(3,
                                checkboxInput("Facet", label = "View All Box Plots",
                                              value=FALSE, width='600px'),  
                                align = 'center'),
                         column(3)
                         ),
                         
                         
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
                             column(width=2))),
                             
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
                             column(width = 3), 
                             )))
                         ),
                         
                     
            
            tabPanel("147 Cities",
                mainPanel(
                    
                    fluidRow(
                        align = "center",
                        h3("CLICK On Your City!"),
                        width=12),
                    fluidRow(
                             column(
                                # h4("The 147 Cities"),
                                 width = 12, align='center',
                                    leafletOutput(outputId = "Geo2", width="100%"),
                                    p(),
                                    tableOutput(outputId = "MapTable")),
                                    ),
                )),
                                    
            tabPanel("1 City vs 1 Parameter",
                     sidebarLayout(
                         sidebarPanel(
                             
                             selectizeInput(inputId = 'Ci',
                                            label = 'City',
                                            choices = df$City),
                             
                             selectizeInput(inputId = "Para",
                                            label = "Parameter",
                                            choices = rownames(t(df))[5:14]),
                             
                             checkboxInput("CRank1", "Rank Cities By Parameter", value=FALSE),
                             
                             conditionalPanel(
                                 condition = "input.CRank1 == true",
                             
                             numericInput(inputId = 'Top',
                                          label = 'Ranking: High To Low',
                                          value=10, 
                                          min=1, 
                                          max=75, 
                                          step=1),
                             
                             numericInput(inputId = 'Bot',
                                          label = 'Ranking: Low To High',
                                          value = 10, 
                                          min = 1, 
                                          max = 75, 
                                          step = 1)
                         )),
                         
                         mainPanel(
                                 plotOutput(outputId = "Dist"),
                                 fluidPage(
                                     checkboxInput("CRank", "Show Ranking", value=FALSE),
                                     
                                     conditionalPanel(
                                         condition = "input.CRank == true",
                    
                                    fluidRow(
                                        column(width=6, align="center", h3("Ranking: High To Low"), 
                                                tableOutput(outputId = "View")),
                                        column(width=3, align="center", h3("Ranking: Low To High"), 
                                                tableOutput(outputId = "View2")))
                                    
                                    ))
                                 
                                 ))),
                     
                   
            tabPanel("Compare 2 Cities",
                     sidebarLayout(
                         sidebarPanel(
                             
        
                selectizeInput(inputId = 'CityComp1',
                               label = 'City1',
                               choices = colnames(citycomp)[3:149]),
                
                selectizeInput(inputId = 'CityComp2',
                               label = 'City2',
                               choices = colnames(citycomp)[3:149]),
                
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
            
                tabPanel("Scaled Box",
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 selectizeInput(inputId = 'CityBox1',
                                                label = 'City1',
                                                choices = colnames(citycomp)[3:149]),
                                 
                                 selectizeInput(inputId = 'CityBox2',
                                                label = 'City2',
                                                choices = colnames(citycomp)[3:149])
                             ),
                             
                             mainPanel(
                                 plotOutput(outputId = "Box"))
                                 
                                )),
            
                tabPanel("Your Index",
                        sidebarLayout(
                             sidebarPanel(
                                 
                                 selectizeInput(inputId = "ParaIndex1",
                                                label = "Parameter #1 (Wtd 10x)",
                                                choices = rownames(t(df))[5:14],
                                                selected = rownames(t(df))[5]),
                                 
                                 selectizeInput(inputId = "ParaIndex2",
                                                label = "Parameter #2 (Wtd 9x)",
                                                choices = rownames(t(df))[5:14],
                                                selected = rownames(t(df))[6]),
                                 
                                 selectizeInput(inputId = "ParaIndex3",
                                                label = "Parameter #3 (Wtd 8x)",
                                                choices = rownames(t(df))[5:14],
                                                selected = rownames(t(df))[7]),
                                 
                                 selectizeInput(inputId = "ParaIndex4",
                                                label = "Parameter #4 (Wtd 7x)",
                                                choices = rownames(t(df))[5:14],
                                                selected = rownames(t(df))[8]),
                                 
                                 selectizeInput(inputId = "ParaIndex5",
                                                label = "Parameter #5 (Wtd 6x)",
                                                choices = rownames(t(df))[5:14],
                                                selected = rownames(t(df))[9]),
                                 
                                 selectizeInput(inputId = "ParaIndex6",
                                                label = "Parameter #6 (Wtd 5x)",
                                                choices = rownames(t(df))[5:14],
                                                selected = rownames(t(df))[10]),
                                 
                                 selectizeInput(inputId = "ParaIndex7",
                                                label = "Parameter #7 (Wtd 4x)",
                                                choices = rownames(t(df))[5:14],
                                                selected = rownames(t(df))[11]),
                                 
                                 selectizeInput(inputId = "ParaIndex8",
                                                label = "Parameter #8 (Wtd 3x)",
                                                choices = rownames(t(df))[5:14],
                                                selected = rownames(t(df))[12]),
                                 
                                 selectizeInput(inputId = "ParaIndex9",
                                                label = "Parameter #9 (Wtd 2x)",
                                                choices = rownames(t(df))[5:14],
                                                selected = rownames(t(df))[13]),
                                 
                                 selectizeInput(inputId = "ParaIndex10",
                                                label = "Parameter #10 (Wtd 1x)",
                                                choices = rownames(t(df))[5:14],
                                                selected = rownames(t(df))[14]),
                                 
                                 numericInput(inputId = 'Rankers',
                                              label = 'Ranked Cities: How Many?',
                                              value=25, 
                                              min=1, 
                                              max=150, 
                                              step=1)
                                 
                         ),
                         
                         mainPanel(
                             
                             tableOutput(outputId = "Index_Ranking"))
                         
                     ))
            
)))
