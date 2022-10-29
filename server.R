options(shiny.maxRequestSize=200*1024^2)
options(DT.options = list(pageLength = 20, language = list(search = 'Filter:')))

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
}