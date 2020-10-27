library(dplyr)
library(lubridate)
library(tidyr)
#Looking at product orders because it has individual products that can be grouped into baskets
product_orders <- read.csv('OP 010118 061819.csv',stringsAsFactors = FALSE,header = TRUE,na.strings = c(""))


#Selecting relevant columns and reformatting so that mice can impute missing values across chained columns. Converting to factors 
#and dates
relevant_columns <- c('customer_id','date_calculated','showroom','order_code',
                      'order_num','product_type','fabric_mill',
                      'fabric_collection')
product_orders <- product_orders[,relevant_columns]

product_orders$date_calculated <- as.Date(mdy_hm(product_orders$date_calculated))
product_orders$showroom <- as.factor(product_orders$showroom)
product_orders$product_type <- as.factor(product_orders$product_type)
product_orders$fabric_mill <- as.factor(product_orders$fabric_mill)
product_orders$fabric_collection <- as.factor(product_orders$fabric_collection)



#Dealing with NA values, using the mice package
library(mice)
library(VIM)
leave_out <- c('order_code','order_num','fabric_collection')
imp <- mice(drop_columns(product_orders,leave_out),m=1,method=NULL,
            seed = 123)
imputed_columns <- complete(imp)
left_out_columns <- product_orders[,leave_out]
product_orders <- cbind(imputed_columns,left_out_columns)
product_orders <- na.omit(product_orders)

##We need to get the data into a transaction format which means we need one row for a given purchase by a specific customer.
##Before that happens, I want the products box to also have the fabric brand associated with each product so combine the columns
#product_type and fabric_mill into product column
sorted <- product_orders[order(product_orders$customer_id),]
sorted$customer_id <- as.numeric(sorted$customer_id)
sorted$product <- paste(sorted$fabric_mill,sorted$product_type,sep=" ")

#We have the branded products so now we use plyr to create the bundle of purchased items for each transaction
library(plyr)
transactions <- ddply(sorted,c("customer_id","date_calculated",'showroom'), 
                     function(x)paste(x$product, 
                                        collapse = ","))
colnames(transactions)[4] <- 'products'
##Just need the order id and item. Then write it to a csv that can be redownloaded
item <- sorted[,c('order_code','product')]
write.csv(item, 'item_purchases.csv',row.names = FALSE)
baskets  <- read.transactions('item_purchases.csv',format = 'single',sep=",",cols = c(1,2))
baskets@itemInfo$labels <- noquote(baskets@itemInfo$labels)

##Now we can run the apriori algorithm if we specify the parameters for the algorithm. We can also plot the results
basket_rules <- apriori(baskets,parameter = list(sup = 0.002, conf = 0.5,target="rules"));
inspect(basket_rules)

library(arulesViz)
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)

