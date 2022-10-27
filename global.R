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