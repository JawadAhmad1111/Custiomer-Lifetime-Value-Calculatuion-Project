
library(tidyverse)
library(lubridate)
df_salesData <- read.csv("storeData.csv")


# calculation for lifetime

df_salesData |> 
  mutate(subtotal = Quantity*UnitPrice,
         bill_date = strptime(InvoiceDate, format = "%m/%d/%Y %H:%M"))|>
  group_by(CustomerID) |>
  summarise(total_revenue = sum(subtotal),
            lifespan = difftime(max(bill_date), min(bill_date), units = "days")) |>
  filter(total_revenue > 0) |> 
  mutate(lifespan = as.integer(lifespan)/(30)) |>
  summarise(avg_lifetime = round (mean(lifespan))) -> Average_lifespan
  

#Calculations for Average Purchase frequency
df_salesData |> 
  mutate(subtotal = UnitPrice*Quantity,
         bill_date = strptime(InvoiceDate, format = "%m/%d/%Y %H:%M"),
         bill_month = month(x = bill_date,label = TRUE,abbr = FALSE),
         bill_year = year(x = bill_date))|>
  drop_na() |>
  group_by(CustomerID,bill_month, bill_year) |>
  summarise(total = sum(subtotal),
            n_purchase = n_distinct(InvoiceNo)) |>
  filter(total >= 1) |>
  ungroup() |> 
  summarise(avg_freq = mean(n_purchase)) -> Average_freq

# Calculations For Average Purchase Value 

df_salesData |> 
  mutate(subtotal = Quantity*UnitPrice) |> 
  group_by(InvoiceNo) |> 
  summarise( total = sum(subtotal)) |> 
  summarise(avg_purchase = mean(total)) -> Average_Purchase

#Gross_margin (given)
data.frame( gross_margin = c(0.3)) -> gross_margin



library(tidyverse)
Average_freq |> 
  mutate( id = 1) |> 
  full_join(Average_lifespan |> mutate(id = 1),by = "id") |>
  full_join(Average_Purchase |> mutate(id = 1 ), by = "id") |>
  full_join(gross_margin |> mutate(id = 1), by = "id") |> 
  mutate(clv = avg_lifetime * avg_freq * avg_purchase * gross_margin) |>
  select( avg_lifetime , avg_freq, avg_purchase ,gross_margin, clv) -> Clv


  


  

  








 






  
  


