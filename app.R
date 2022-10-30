server <- options(shiny.maxRequestSize=200*1024^2)
options(DT.options = list(pageLength = 20, language = list(search = 'Filter:')))

library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
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
      menuItem(text = "Top 5",
               tabName = "menu_5",
               icon = icon("trophy")),
      menuItem(text = "Data",
               tabName = "menu_4",
               icon = icon("table")),
      menuItem(text = "GitHub", 
               icon = icon("github"), 
               href = "https://github.com/rafiff23/ECommerce")
    )
  ),
  dashboardBody(uiOutput('background_change'),
                
                
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
                        width="9", status = "primary", solidHeader = T) 
                    ),
                    fluidPage(
                      box(
                        echarts4rOutput(outputId = "plot2"),
                        width = "12", status = "primary", solidHeader = T
                      ),
                    )
                  ),
                  # --------- Page 2 : RFM
                  tabItem(
                    tabName = "menu_2",
                    fluidPage(
                      # --------- INPUT
                      fluidRow(column(4,
                                      box(
                                        selectizeInput(inputId = "rfm",label= h4("Select Analysis Type : "),choices = c("Recency", "Frequency", "Monetary"), selected = "Monetary"),
                                        background = "navy",
                                        height = 100,
                                        width = 12
                                      ),
                                      box(width = 12,height = 310
                                          ,
                                          style = 'font-size:12px;',
                                          h3("Explanation"),
                                          div(style = "text-align:justify",
                                              p("RFM is a method used for analyzing customer value. It is commonly used in database marketing 
                                            and direct marketing and has received particular attention in retail and professional services industries"),
                                              p("RFM stands for the three dimensions:"),
                                              p("Recency – How recently did the customer purchase?"),
                                              p("Frequency – How often do they purchase?"), 
                                              p("Monetary Value – How much do they spend?"),
                                              p("Source :",
                                                a(href = "https://en.wikipedia.org/wiki/RFM_(market_research)",
                                                  "Wikipedia")),
                                              
                                          ),
                                          background = "navy"
                                      ),
                                      valueBox("1.103 | 23.81%", 
                                               "Total Recency Value | Highest Segment : Champions", 
                                               icon = icon("city"),
                                               color = "maroon",
                                               width = 12),
                                      
                                      valueBox("4.243 | 26.31%", 
                                               "Total Frequency Value | Highest Segment : Champions", 
                                               icon = icon("users"),
                                               color = "aqua",
                                               width = 12),
                                      valueBox("2.567 | 25%", 
                                               "Total Monetary Value | Highest Segment : Champions", 
                                               icon = icon("chart-line"),
                                               color = "maroon",
                                               width = 12),
                      ),
                      
                      column(8, 
                             
                             box(
                               status = "primary", solidHeader = T,
                               echarts4rOutput(outputId = "plot3"),
                               width = 12
                             ),
                             
                             
                             
                             box(echarts4rOutput(outputId = "plot4"),
                                 width = 12, status = "primary", solidHeader = T)
                      )
                      )  
                    )
                  ),
                  
                  tabItem(
                    tabName = "menu_3",
                    fluidRow(
                      box(
                        selectizeInput(inputId = "type",label= h4("Visualize by : "),choices = c("Total Quantity", "Date"), selected = "Total"),
                        width = 12,solidHeader = T, status = "primary"
                      ),
                      box(
                        echarts4rOutput(outputId = "plot5", height = 1000),
                        width = 12,solidHeader = T, status = "primary"
                      ),
                      
                    )
                  ),
                  
                  # --------- page 4 : data
                  tabItem(
                    tabName = "menu_4",
                    
                    selectizeInput(inputId = "selectfile",label= h4("Select Data Table : "),choices = c("Main Data", "RFM Data"), selected = "Main Data"),
                    DT::dataTableOutput(outputId = "dt1")
                    
                  ),
                  
                  tabItem(
                    tabName = "menu_5",
                    fluidRow(
                      box(   width = 12,
                             solidHeader = T,
                             status = "primary",
                             tabsetPanel(
                               tabPanel("Without Timeline",echarts4rOutput(outputId = "plot6")),
                               tabPanel("With Timeline",echarts4rOutput(outputId = "plot8")),
                             )),
                      box(
                        echarts4rOutput(outputId = "plot7"),
                        width = 12,
                        solidHeader = T,
                        status = "primary"
                      )
                    )
                  )
                  
                )
                
  )
)

# Define server logic required to draw a histogram

ShinyServer <- function(input, output, session) { 
  
  output$plot1 <- renderEcharts4r({
    max <- list(
      name = "Max",
      type = "max"
    )
    
    min <- list(
      name = "Min",
      type = "min"
    )
    
    avg <- list(
      type = "average",
      name = "AVG"
    )
    
    # val3 <- paste0(input$btn2)
    
    val <- paste0(input$ctr)
    df2 <- df %>% group_by(Country, InvoiceDate) %>%
      summarise(Quantity = sum(Quantity))
    df2 <- filter(df2, Country %in% val) 
    df2 %>%
      e_chart(x= InvoiceDate, timeline = F ,renderer = T) %>% 
      e_line(serie = Quantity,showSymbol = F, symbol = "none", name = "Quantity") %>% 
      e_tooltip(trigger = "axis") %>%  
      e_datazoom(x_index = c(0, 1), start = 20, end = 50) %>% 
      e_mark_point(data = max) %>% 
      e_mark_point(data = min) %>% 
      e_legend(show = FALSE) %>% 
      e_theme(ifelse(input$btn1 == "yes", "chalk","vintage")) %>% 
      e_title(text = "Total Transaction") %>% 
      e_y_axis(name = "Quantity",
               nameLocation = "middle",
               nameTextStyle = list(
                 color = "black",
                 fontSize = 20
               )) %>% 
      e_y_axis(position = "left",
               nameGap = 50)
  })
  
  output$plot2 <- renderEcharts4r({
    
    val2 <- as.numeric(paste0(input$range))
    
    
    df_x <- df %>% mutate_at(.vars = c("StockCode"),
                             .funs = as.numeric)
    df_x <- df_x %>% drop_na() 
    df_x <- df_x %>% group_by(Description) %>% 
      summarise(max = max(UnitPrice))
    df_x <-df_x[order(df_x$max,decreasing = T),]
    df_x <- head(df_x,val2)
    df_x <-df_x[order(df_x$max,decreasing = F),]
    cols <- "#587195"
    df_x %>% 
      e_chart(Description) %>% 
      e_bar(max, label =list(show= T , position = "right", formatter = "{b}",
                             emphasis = list(focus = "series"))) %>% 
      e_color(cols) %>% 
      e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return(params.name + 
                '<br />Unit Price: ' + params.value[0]) 
                }
    ")) %>% 
      e_flip_coords() %>% 
      e_y_axis(axisLabel = list(show = F), 
               axisTick = list(show = FALSE),
               axisLine = list(show = FALSE)) %>% 
      e_legend(show = FALSE) %>% 
      e_title(text = "Most Expensive Units", left = "center", top = "5%") %>% 
      e_x_axis(name = "Price",
               nameLocation = "middle",
               nameTextStyle = list(
                 color = "black",
                 fontSize = 20
               )) %>% 
      e_y_axis(name = "Items",
               nameLocation = "middle",
               nameTextStyle = list(
                 color = "black",
                 fontSize = 20
               )) %>% 
      e_theme(ifelse(input$btn1 == "yes", "chalk","vintage")) %>% 
      e_grid(left ='5%')
  })
  
  output$btn1 <- {( 
    renderText(input$btn1) )}
  
  output$selected <- renderText({
    paste(input$ctr)
  })
  output$slider <- renderText({
    paste(input$range)
  })
  output$btn2 <- {( 
    renderText(input$btn2) )}
  
  output$btn3 <- {( 
    renderText(input$btn2) )}
  
  output$plot3 <- renderEcharts4r({
    res <- paste0(input$rfm)
    result <- switch(  
      
      res,  
      "Monetary"= df_clean %>% 
        e_charts() %>% 
        e_histogram(monitery, name = "Monetary") %>% 
        e_density(monitery, name = "density", areaStyle = list(opacity = .4), 
                  smooth = TRUE, y_index = 1) %>% 
        e_tooltip() %>% e_theme(ifelse(input$btn1 == "yes", "chalk","vintage")) %>% 
        e_y_axis(axisLabel = list(show = F), 
                 axisTick = list(show = FALSE),
                 axisLine = list(show = FALSE),   
                 name = "Customers",
                 nameLocation = "middle",
                 nameTextStyle = list(
                   color = "black",
                   fontSize = 20
                 )) %>% 
        e_x_axis(name = "Orders",
                 nameLocation = "middle",
                 nameTextStyle = list(
                   color = "black",
                   fontSize = 20
                 )) %>% 
        e_title(text = "Customer Orders by Monetary", left = "center") %>% 
        e_legend(top = '7%', left = 'center'),  
      "Frequency"= df_clean %>% 
        e_charts() %>% 
        e_histogram(freq, name = "Frequency") %>% 
        e_density(freq, name = "density", areaStyle = list(opacity = .4), 
                  smooth = TRUE, y_index = 1) %>%  
        e_tooltip() %>%  e_theme(ifelse(input$btn1 == "yes", "chalk","vintage"))%>% 
        e_y_axis(axisLabel = list(show = F), 
                 axisTick = list(show = FALSE),
                 axisLine = list(show = FALSE), 
                 name = "Customers",
                 nameTextStyle = list(
                   color = "black",
                   fontSize = 20,
                   left = "50%"
                 )) %>% 
        e_x_axis(name = "Orders",
                 nameLocation = "middle",
                 nameTextStyle = list(
                   color = "black",
                   fontSize = 20
                 )) %>% 
        e_title(text = "Customer Orders by Frequency", left = "center")%>% 
        e_legend(top = '7%', left = 'center'),    
      "Recency"= df_clean %>% 
        e_charts() %>%  
        e_histogram(recency, name = "Recency")  %>%  
        e_density(recency, name = "density", areaStyle = list(opacity = .4), 
                  smooth = TRUE, y_index = 1)  %>%  
        e_tooltip() %>% e_theme(ifelse(input$btn1 == "yes", "chalk","vintage"))%>% 
        e_y_axis(axisLabel = list(show = F), 
                 axisTick = list(show = FALSE),
                 axisLine = list(show = FALSE),   
                 name = "Customers",
                 nameLocation = "middle",
                 nameTextStyle = list(
                   color = "black",
                   fontSize = 20
                 )) %>% 
        e_x_axis(name = "Orders",
                 nameLocation = "middle",
                 nameTextStyle = list(
                   color = "black",
                   fontSize = 20
                 )) %>% 
        e_title(text = "Customer Orders by Recency", left = "center")%>% 
        e_legend(top = '7%', left = 'center'),    
    )  
    print(result)
  })
  
  output$selected2 <- renderText({
    paste(input$rfm)
  })
  
  
  output$plot4 <- renderEcharts4r({
    segments_res <- segments %>% group_by(segment) %>% 
      summarise(median = median(recency_score))
    segments_mon <- segments %>% group_by(segment) %>% 
      summarise(median = median(monetary_score))
    segments_freq <- segments %>% group_by(segment) %>% 
      summarise(median = median(frequency_score))
    
    res <- paste0(input$rfm)
    result <- switch(  
      res,  
      "Monetary"= segments_mon %>% 
        e_charts(segment) %>%  
        e_pie(median, radius = c('40%', '70%'),
              itemStyle = list(borderRadius= 10, borderColor = "#fff", borderWidth = 2),
              emphasis = list(show = T, fontSize = "40", label = list(show = T))) %>%  
        e_title("Median Monetary by Segment", position = "bottom")  %>% 
        e_labels(show = FALSE,
                 position = "center",
                 fontSize = 14,
                 fontWeigth = "bold",
                 formatter = "{b} \n median value : {d}%") %>%
        e_theme(ifelse(input$btn1 == "yes", "chalk","vintage")) %>% 
        e_legend(type = c("scroll"), right = 0,orient = "vertical"),   
      
      "Frequency"= segments_freq %>% 
        e_charts(segment) %>%  
        e_pie(median, radius = c('40%', '70%'),
              itemStyle = list(borderRadius= 10, borderColor = "#fff", borderWidth = 2),
              emphasis = list(show = T, fontSize = "40", label = list(show = T))) %>%  
        e_title("Median Frequency
                by Segment", position = "bottom")  %>% 
        e_labels(show = FALSE,
                 position = "center",
                 fontSize = 14,
                 fontWeigth = "bold",
                 formatter = "{b} \n median value : {d}%") %>%
        e_theme(ifelse(input$btn1 == "yes", "chalk","vintage")) %>% 
        e_legend(type = c("scroll"), right = 0,orient = "vertical"),
      
      "Recency"=  segments_res %>% 
        e_charts(segment) %>%  
        e_pie(median, radius = c('40%', '70%'),
              itemStyle = list(borderRadius= 10, borderColor = "#fff", borderWidth = 2),
              emphasis = list(show = T, fontSize = "40", label = list(show = T))) %>%  
        e_title("Median Recency by Segment", position = "bottom")  %>% 
        e_labels(show = FALSE,
                 position = "center",
                 fontSize = 14,
                 fontWeigth = "bold",
                 formatter = "{b} \n median value : {d}%") %>%
        e_theme(ifelse(input$btn1 == "yes", "chalk","vintage")) %>% 
        e_legend(type = c("scroll"), right = 0,orient = "vertical"),  
    )  
    print(result)
  })
  
  output$plot5 <- renderEcharts4r({
    test <- df %>% group_by(Country) %>% 
      summarise(TotalQuantity  = sum(Quantity))
    test2 <- df %>% group_by(InvoiceDate, Country) %>% 
      summarise(TotalQuantity  = sum(Quantity))
    tipe <- paste0(input$type)
    
    plot <- switch(tipe,
                   "Total Quantity" = 
                     test %>%  
                     e_charts(Country) %>% 
                     e_map(TotalQuantity , name = "Total Quantity of") %>%  
                     e_visual_map(TotalQuantity) %>% 
                     e_brush() %>% 
                     e_tooltip(),
                   "Date" = 
                     
                     test2 %>%  
                     e_charts(Country, timeline = T) %>% 
                     e_map(TotalQuantity, name = "Quantity of") %>%  
                     e_visual_map(TotalQuantity) %>% 
                     e_brush() %>% 
                     e_tooltip() %>%  
                     e_timeline_opts(
                       top = 80,
                       label = list(color = "white")
                     )
                   
                   
    )
    
    print(plot)
    
    
  })
  
  output$dt1 <- DT::renderDataTable ({
    x1 <- paste0(input$selectfile)
    
    xx <- switch (x1,
                  "Main Data" =   df,options = list(pageLength = 25, scrollx =T),
                  "RFM Data" = segments, options = list(pageLength = 25, scrollx=T))
  })
  
  output$selected3 <- renderText({
    paste(input$selectfile)
  })
  
  bg <- reactive({
    case_when(input$btn1 =='yes' ~ '.content-wrapper, .right-side {
                                             background-color: #5f7896;
                                             color: white;
                                        }
                                        .box.box-solid.box-primary>.box-header {
                                          color:#fff;
                                            background-color:#222d32
                                        }
              
                                      .box.box-solid.box-primary{
                                        border-bottom-color:#3b4b5e;
                                          border-left-color:#3b4b5e;
                                          border-right-color:#3b4b5e;
                                          border-top-color:#3b4b5e;
                                          background-color:#3b4b5e
                                      }
                                      
                                      /* background color of header (logo part) */
                                        .skin-blue .main-header .logo {
                                          background-color: #10151a;
                                        }
                                      
                                      
                                      /* change the background color of header (logo part) when mouse hover */
                                        .skin-blue .main-header .logo:hover {
                                          background-color: #C8DBBE;
                                        }
                                      
                                      
                                      /* background color for remaining part of the header */
                                        .skin-blue .main-header .navbar {
                                          background-color: #1d242e;
                                        } 
                                      
                                      /* main sidebar */
                                        .skin-blue .main-sidebar {
                                          background-color: #1d242e;
                                        }
                                      
                                      /* active sidebar menu item */
                                        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                          background-color: #000000;
                                            color:#fee0e7;
                                        }
                                      
                                      /* sidebar menuitems */
                                        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                          background-color: #1d242e;
                                            color: #fc97af;
                                        }
                                      
                                      /* sidebar menuitems when mouse hover */
                                        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                          background-color: #1d242e;
                                            color:white;
                                        }
                                      
                                      /* sidebar toggle button */           
                                        .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                          background-color: black;
                                        }
                                       ',
              input$btn1 =='no' ~  '.content-wrapper, .right-side {
                             background-color: #FFF8EA;
              }

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
                                        /* background color of header (logo part) */
                    .skin-blue .main-header .logo {
                              background-color: #594545;
                            }
                            
                            
                    /* change the background color of header (logo part) when mouse hover */
                    .skin-blue .main-header .logo:hover {
                              background-color: #C8DBBE;
                            }
                            
                    
                    /* background color for remaining part of the header */
                    .skin-blue .main-header .navbar {
                                                  background-color: #9E7676;
                                                  } 
                                                  
                    /* main sidebar */
                            .skin-blue .main-sidebar {
                                                  background-color: #9E7676;
                                                  }
                    
                    /* active sidebar menu item */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                                  background-color: #815B5B;
                                                  color:black;
                                                  }
                    
                    /* sidebar menuitems */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                                  background-color: #9E7676;
                                                  color: black;
                                                  }
                    
                    /* sidebar menuitems when mouse hover */
                             .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                                  background-color: #9E7676;
                                                  color:white;
                                                  }
                                                  
                    /* sidebar toggle button */           
                    .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                                  background-color: black;
                                                  }
                                                  ',
              
              input$btn1 =='Tool 3' ~ 'body {
                                            background-color: brown;
                                            color: white;
                                        }',
              TRUE  ~ 'body {
                background-color: red;
                color: white;
                }')
  })
  
  output$background_change <- renderUI({
    tagList(fluidPage(tags$style(HTML(bg()))))
  })
  output$type <- renderText({
    paste(input$type)
  })
  
  output$plot6 <- renderEcharts4r({
    df_line <- df %>% filter(Country %in% c("United Kingdom","Netherlands", "EIRE", "Germany", "France"))%>% 
      group_by(Country, InvoiceDate) %>%
      summarise(Quantity = sum(Quantity))
    
    df_line %>% 
      group_by(Country) %>% 
      e_charts(InvoiceDate) %>%
      e_line(serie = Quantity,showSymbol = F, symbol = "none") %>% 
      e_datazoom(x_index = c(0, 1), start = 20, end = 70) %>% 
      e_legend(orient = "vertical", right = "5", top ="15%",  
               selector = list(  
                 list(type = 'inverse', title = 'Invert'),
                 list(type = 'all', title = 'Reset')
               )
      ) %>% 
      e_tooltip(trigger = "axis") %>%  
      e_grid(right = "17%") %>% 
      e_animation(show = T, duration = 1000) %>% 
      e_theme(ifelse(input$btn1 == "yes", "dark","vintage")) %>% 
      e_title(paste0("Total Item Bought in Eacht Date"), 
              subtext = "Top 5 Country with the Highest Total Quantity", 
              left = "center", top = 10)
  })
  
  output$plot7 <- renderEcharts4r({
    df_bars <- df %>% group_by(InvoiceDate, Country) %>% summarise(TotalQuantity = sum(Quantity)) %>% 
      filter(Country %in% c("Netherlands", "EIRE", "Germany", "France","United Kingdom"))
    
    df_bars$TotalQuantity <- log(df_bars$TotalQuantity)
    df_bars$TotalQuantity <- format(round(df_bars$TotalQuantity, 2), nsmall = 2)
    df_bars %>%
      group_by(InvoiceDate) %>%
      e_charts(Country, timeline = TRUE) %>%
      e_bar(TotalQuantity, realtimeSort = TRUE, name = "Total Quantity",itemStyle = list(
        borderColor = "black", borderWidth = '1')
      ) %>%
      e_legend(show = FALSE) %>%
      e_flip_coords() %>%
      e_y_axis(inverse = TRUE)  %>%
      e_labels(position = "insideRight", 
               formatter = htmlwidgets::JS("
      function(params){
        return(params.value[0])
      }
    ") ) %>%
      # e_add("itemStyle", color) %>%
      e_timeline_opts(autoPlay = TRUE, top = "55", playInterval = 1000)  %>%
      e_tooltip(trigger = "axis") %>%  
      e_grid(top = 100) %>%
      e_theme(ifelse(input$btn1 == "yes", "chalk","vintage")) %>% 
      e_title(paste0("Total Item Bought in Eacht Date (log)"), 
              subtext = "Top 5 Country with the Highest Total Quantity", 
              left = "center", top = 10)
    
  })
  output$plot8 <- renderEcharts4r({
    df_line <- df %>% filter(Country %in% c("United Kingdom","Netherlands", "EIRE", "Germany", "France"))%>% 
      group_by(Country, InvoiceDate) %>%
      summarise(Quantity = sum(Quantity))
    
    df_line %>% 
      group_by(Country) %>% 
      e_charts(InvoiceDate, timeline = T) %>%
      e_line(serie = Quantity,showSymbol = F, symbol = "none") %>% 
      e_tooltip(trigger = "axis") %>%  
      e_grid(right = "17%") %>% 
      e_title(paste0("Total Item Bought in Eacht Date"), 
              subtext = "Top 5 Country with the Highest Total Quantity", 
              left = "center", top = 10) %>% 
      e_legend(show = F) %>% 
      e_datazoom(orient = "vertical", right = "150", top ="15%") %>% 
      e_theme(ifelse(input$btn1 == "yes", "chalk","vintage")) 
  })
}
# Run the application 
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(highcharter)
library(scales)
library(DT)
library(echarts4r)
library(rfm)
library(shinydashboard)
library(shiny)

# ------
df <- read_csv("data.csv")

# ------ Cleansing
df <- df %>% mutate(Quantity = replace(Quantity, Quantity<=0, NA),
                    UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))
df <- df %>% drop_na() 
df<- df %>% 
  mutate_at(.vars = c("InvoiceNo", "CustomerID","StockCode","Country"),
            .funs = as.character)
# mutate_at(.vars = c("Country"),
#           .funs = as.factor) 
df$InvoiceDate <- date(parse_date_time(df$InvoiceDate,"mdy_HM"))
df <- df %>% mutate(TotalPrice = Quantity*UnitPrice)

# ------ RFM
df_clean <- df %>% group_by(CustomerID) %>% 
  summarise(recency = as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            freq =n_distinct(InvoiceNo), monitery = sum(TotalPrice)/n_distinct(InvoiceNo))
df_clean$monitery <- log(df_clean$monitery)

# ------ Removing Skewdness in Monitory
df_clean$monitery <- log(df_clean$monitery)

# ------ Making Customer Segment
rfm_result <- rfm_table_order(
  data = df,
  customer_id = CustomerID,
  revenue = TotalPrice,
  order_date = InvoiceDate, 
  analysis_date = as.Date("2010-12-01") 
)
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

segments <- rfm_segment(rfm_result,
                        segment_names,
                        recency_lower,
                        recency_upper,
                        frequency_lower, 
                        frequency_upper, 
                        monetary_lower,
                        monetary_upper)

shinyApp(ui = ui, server = server)
