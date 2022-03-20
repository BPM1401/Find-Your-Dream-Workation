
# Define UI for application that assists user to search their Dream Workation
shinyUI(
    
    fluidPage(
        titlePanel(h1("Find Your Dream Workation")),
        
        tabsetPanel(
            
            tabPanel("Overview",
                mainPanel(
                    
                    fluidRow(
                        align = "center",
                        h1("Fuse 147 Cities Across 10 Parameters"),
                        width=12),
                    fluidRow(
                             column(width = 5, align='center',
                                    plotOutput(outputId = "Geo", width="80%")),
                             column(width = 7, align='center',
                                    selectizeInput(inputId = "GeoPara",
                                                   label = div(style = "font-size:20px;",
                                                               style = "color: blue;",
                                                               "10 Parameters"),
                                                   choices = rownames(t(df))[5:14]),
                                    tags$head(tags$style(HTML(".selectize-input {height: 40px;
                                                              width: 300px; font-size: 20px}"))),
                                    tags$style("[type = 'number'] {height: 40px;
                                                              width: 300px; font-size: 20px}"),
                                    plotOutput(outputId = "Box_Para", width="50%"))
                                    
                                    ),
                    HTML('<center><img src="W1.png" width="50%"></center>')
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
                
                checkboxGroupInput("my_choices", "Parameter",
                                   choices = c("Wifi", "Co-Working", "Coffee", "Taxi",
                                               "Beer", "Rentals", "Meals", "Sun",
                                               "Fun", 'Insta', 'All'))),
                
                mainPanel(
                        fluidRow(h3("Comparing 2 Cities Across Parameters", align='center'),
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Wifi'",
                                   plotOutput(outputId = "Bar1", width="90%"))),
                                   
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Co-Working'",
                                   plotOutput(outputId = "Bar2", width="90%"))),
                            
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Coffee'",
                                   plotOutput(outputId = "Bar3", width="90%"))),
                                   
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Taxi'",
                                   plotOutput(outputId = "Bar4", width="90%")))),
                        
                        fluidRow( 
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Beer'",
                                   plotOutput(outputId = "Bar5", width="90%"))),
                            
                            column(width = 6, align='center', 
                                   tableOutput(outputId = "CityComps")),
                            
                            
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Rentals'",
                                   plotOutput(outputId = "Bar6", width="90%")))),
                        
                        fluidRow(
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Meals'",
                                   plotOutput(outputId = "Bar7", width="90%"))),
                            
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Sun'",
                                   plotOutput(outputId = "Bar8", width="90%"))),
                            
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Fun'",
                                   plotOutput(outputId = "Bar9", width="90%"))),
                            
                            column(width = 3, align='center',
                                   conditionalPanel(
                                       condition = "input.my_choices == 'Insta'",
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
                                                label = "Parameter #1",
                                                choices = rownames(t(df))[5:14]),
                                 
                                 selectizeInput(inputId = "ParaIndex2",
                                                label = "Parameter #2",
                                                choices = rownames(t(df))[5:14]),
                                 
                                 selectizeInput(inputId = "ParaIndex3",
                                                label = "Parameter #3",
                                                choices = rownames(t(df))[5:14]),
                                 
                                 selectizeInput(inputId = "ParaIndex4",
                                                label = "Parameter #4",
                                                choices = rownames(t(df))[5:14]),
                                 
                                 selectizeInput(inputId = "ParaIndex5",
                                                label = "Parameter #5",
                                                choices = rownames(t(df))[5:14]),
                                 
                                 selectizeInput(inputId = "ParaIndex6",
                                                label = "Parameter #6",
                                                choices = rownames(t(df))[5:14]),
                                 
                                 selectizeInput(inputId = "ParaIndex7",
                                                label = "Parameter #7",
                                                choices = rownames(t(df))[5:14]),
                                 
                                 selectizeInput(inputId = "ParaIndex8",
                                                label = "Parameter #8",
                                                choices = rownames(t(df))[5:14]),
                                 
                                 selectizeInput(inputId = "ParaIndex9",
                                                label = "Parameter #9",
                                                choices = rownames(t(df))[5:14]),
                                 
                                 selectizeInput(inputId = "ParaIndex10",
                                                label = "Parameter #10",
                                                choices = rownames(t(df))[5:14]),
                                 
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
