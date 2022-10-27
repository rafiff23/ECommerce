library(shinydashboard)
library(shiny)

dashboardPage(skin = "blue",
              
              dashboardHeader(title = "E-commerce",titleWidth = 230),
              dashboardSidebar(
                sidebarMenu(
                  menuItem(
                    text = "Overview", 
                    tabName = "menu_1", 
                    icon = icon("clipboard")), 
                  
                  menuItem(text = "RFM",
                           tabName = "menu_2",
                           icon = icon("chart-simple")),
                  
                  menuItem(text = "Geo Map",
                           tabName = "menu_3",
                           icon = icon("globe")),
                  menuItem(text = "Data",
                           tabName = "menu_4",
                           icon = icon("table")),
                  menuItem(text = "GitHub", 
                           icon = icon("github"), 
                           href = "https://github.com/rafiff23/HumanResourceShinyWebApp")
                )
              ),
              dashboardBody(id = 'test',
                            tags$style('#test {
                             background-color: #FFF8EA;
              }'),
                            tags$style(HTML("

                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#222d32
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#C1A3A3;
                    border-left-color:#C1A3A3;
                    border-right-color:#C1A3A3;
                    border-top-color:#C1A3A3;
                    background:#C1A3A3
                    }

                    ")),
                            
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                            ),
                            
                            tabItems(
                              # --------- Page 1 : overview
                              tabItem(
                                tabName = "menu_1", 
                                
                                fluidPage(
                                  h2(tags$b("E-commerce with RFM Analysis")),
                                  br(),
                                  div(style = "text-align:justify", 
                                      p("E-commerce (electronic commerce) is the activity of electronically buying or selling of products on online services or over the Internet.
                                      E-commerce draws on technologies such as mobile commerce, electronic funds transfer, supply chain management, Internet marketing, 
                                      online transaction processing, electronic data interchange (EDI), inventory management systems, and automated data collection systems.
                                      E-commerce is in turn driven by the technological advances of the semiconductor industry, and is the largest sector of the electronics industry."),
                                      br()
                                  )
                                ),
                                fluidPage(
                                  box(
                                    radioButtons("btn1", "Change Theme?", c("yes", "no"), selected = "no"),
                                    # radioButtons("btn2", "Show Marker", c("yes", "no"), selected = "no"),
                                    selectizeInput(inputId = "ctr",label= h4("Select Country : "),choices = unique(df$Country), selected = "United Kingdom"),
                                    sliderInput(inputId = "range", label = "Show Items by : ",1,30,10 , width = 1800),
                                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #587195}")),
                                    width = 3,
                                    height = 425,
                                    # background = "light-blue",
                                    solidHeader = T,
                                    status = "primary"),
                                  
                                  box(
                                    echarts4rOutput(outputId = "plot1"),
                                    width="9") 
                                ),
                                fluidPage(
                                  box(
                                    echarts4rOutput(outputId = "plot2"),
                                    width = "12"
                                  ),
                                )
                              ),
                              
                              # --------- Page 2 : RFM
                              tabItem(
                                tabName = "menu_2",
                                
                                # --------- INPUT
                                fluidPage(
                                  box(
                                    width = 8,
                                    echarts4rOutput(outputId = "plot3")
                                  ),
                                  box(
                                    radioButtons("btn3", "Change Theme?", c("yes", "no"), selected = "no"),
                                    selectizeInput(inputId = "rfm",label= h4("Select Analysis Type : "),choices = c("Recency", "Frequency", "Monetary"), selected = "Monetary"),
                                    width = 4,
                                    background = "navy"
                                  ),
                                  valueBox("1.103 | 23.81%", 
                                           "Total Recency Value | Highest Segment : Champions", 
                                           icon = icon("city"),
                                           color = "maroon",
                                           width = 4),
                                  valueBox("4.243 | 26.31%", 
                                           "Total Frequency Value | Highest Segment : Champions", 
                                           icon = icon("users"),
                                           color = "aqua",
                                           width = 4),
                                  box(echarts4rOutput(outputId = "plot4"),width = 8),
                                    valueBox("2.567 | 25%", 
                                             "Total Monetary Value | Highest Segment : Champions", 
                                             icon = icon("chart-line"),
                                             color = "maroon",
                                             width = 4),
                                  box(width = 4,height = 300,
                                      style = 'font-size:12px;',
                                      h3("Explanation"),
                                      div(style = "text-align:justify",
                                          p("RFM is a method used for analyzing customer value. It is commonly used in database marketing 
                                            and direct marketing and has received particular attention in retail and professional services industries"),
                                          h3("RFM stands for the three dimensions:"),
                                          p("Recency – How recently did the customer purchase?
                                          Frequency – How often do they purchase?
                                          Monetary Value – How much do they spend?"),
                                          p("Source :",
                                            a(href = "https://en.wikipedia.org/wiki/RFM_(market_research)",
                                              "Wikipedia")),
                                          
                                      ),
                                      background = "navy"
                                  ),
                     
                                ),
              
                                
                              ),
                              
                              tabItem(
                                tabName = "menu_3",
                                fluidRow(
                               
                                    echarts4rOutput(outputId = "plot5", height = 1000)
                                  
                                
                                )
                              ),
                              
                              # --------- page 4 : data
                              tabItem(
                                tabName = "menu_4",
                              
                                selectizeInput(inputId = "selectfile",label= h4("Select Data Table : "),choices = c("Main Data", "RFM Data"), selected = "Main Data"),
                                DT::dataTableOutput(outputId = "dt1")
                              
                              )
                              
                            )
                            
              )
)