library(tidyverse)
library(ggplot2)
library(sqldf)
##Read in all of the alterations in the data warehouse
alterations <- read.csv('all_alterations.csv',header = T,na.strings = c(""))
colnames(alterations)[min(grep('product_order_code',colnames(alterations)))] <- 'order_num'
alteration_filtered <- alterations %>% select(order_num,invoice_alt_cost)

#product_orders from January through july
product_orders <- read.csv('jan_july_orders.csv',header = T,na.strings = c(""))
products <- product_orders %>% select(order_code,order_num,order_total,order_date,advisor_fullname,predicted_total_cogs,showroom)
products$order_num <- noquote(substr(products$order_num,start = 6, stop = 11))

#joining alterations data to product order data
merge1 <- merge(products,alteration_filtered,by = 'order_num',all.x = T)

#Validating the showroom
employees <- readxl::read_xlsx('employee_list.xlsx')
employees <- employees %>% select(Name,Location)

showroom_check <- merge(merge1,employees, by.x = 'advisor_fullname',by.y = 'Name',all = T)
showroom_check$Location[is.na(showroom_check$Location)] <- noquote(as.character(showroom_check$showroom[is.na(showroom_check$Location)]))
po_alterations <- showroom_check %>% select(-showroom)

##Changing alteration cost nulls to zero and making invoice alt cost numeric
po_alterations$invoice_alt_cost[is.na(po_alterations$invoice_alt_cost)] <- 0
po_alterations <- po_alterations %>% select(-order_num)
po_alterations$invoice_alt_cost <-as.numeric(as.character(po_alterations$invoice_alt_cost))
table(po_alterations$invoice_alt_cost)

#Want to aggregate the numerics so I'm creating another df that'll hold order data and showroom with order code for the join
alteration_totals <- rownames_to_column(as.data.frame(tapply(po_alterations$invoice_alt_cost, po_alterations$order_code, sum)))
cogs_total <- rownames_to_column(as.data.frame(tapply(po_alterations$predicted_total_cogs, po_alterations$order_code, sum)))
colnames(alteration_totals) <- c('order_code','alteration costs')
colnames(cogs_total) <- c('order_code','COGS')
order_totals <- merge(alteration_totals,cogs_total,by = 'order_code')

#Changing column names and merging it with by order code with order date, advisor name, and Location
merge_df <- po_alterations %>% select(order_code,order_date,advisor_fullname,Location)
complete_df <- merge(order_totals,merge_df,by='order_code')
complete_data <- complete_df[!duplicated(complete_df$order_code),]
writexl::write_xlsx(complete_data,'Alterations.xlsx',col_names = T)
