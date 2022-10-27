options(shiny.maxRequestSize=200*1024^2)
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
      e_line(serie = Quantity,showSymbol = F, symbol = "none") %>% 
      e_tooltip(trigger = "axis") %>%  
      e_datazoom(x_index = c(0, 1), start = 20, end = 50) %>% 
      e_mark_point(data = max) %>% 
      e_mark_point(data = min) %>% 
      e_legend(show = FALSE) %>% 
      e_theme(ifelse(input$btn1 == "yes", "chalk","vintage")) 
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
      e_title(text = "Most Expensive Units", left = "center") %>% 
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
        e_histogram(monitery) %>% 
        e_density(monitery, name = "density", areaStyle = list(opacity = .4), 
                  smooth = TRUE, y_index = 1) %>% 
        e_tooltip() %>% e_theme(ifelse(input$btn3 == "yes", "chalk","vintage")),  
      "Frequency"= df_clean %>% 
        e_charts() %>% 
        e_histogram(freq) %>% 
        e_density(freq, name = "density", areaStyle = list(opacity = .4), 
                  smooth = TRUE, y_index = 1) %>%  
        e_tooltip() %>%  e_theme(ifelse(input$btn3 == "yes", "chalk","vintage")),  
      "Recency"= df_clean %>% 
        e_charts() %>%  
        e_histogram(recency)  %>%  
        e_density(recency, name = "density", areaStyle = list(opacity = .4), 
                  smooth = TRUE, y_index = 1)  %>%  
        e_tooltip() %>% e_theme(ifelse(input$btn3 == "yes", "chalk","vintage")),  
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
        e_theme(ifelse(input$btn3 == "yes", "chalk","vintage")) %>% 
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
        e_theme(ifelse(input$btn3 == "yes", "chalk","vintage")) %>% 
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
        e_theme(ifelse(input$btn3 == "yes", "chalk","vintage")) %>% 
        e_legend(type = c("scroll"), right = 0,orient = "vertical"),  
    )  
    print(result)
  })
  
  output$plot5 <- renderEcharts4r({
    
    test <- df %>% group_by(Country) %>% 
      summarise(TotalQuantity  = sum(Quantity))
    test %>%  
      e_charts(Country) %>% 
      e_map(TotalQuantity) %>%  
      e_visual_map(TotalQuantity) %>% 
      e_brush() %>% 
      e_tooltip()
  })
  
  output$dt1 <- DT::renderDataTable ({
    x1 <- paste0(input$selectfile)
    
    xx <- switch (x1,
                  "Main Data" =   df,options = list(pageLength = 20, scrollx =T),
                  "RFM Data" = segments, options = list(pageLength = 20, scrollx = T))
  })
  
  output$selected3 <- renderText({
    paste(input$selectfile)
  })
  
}