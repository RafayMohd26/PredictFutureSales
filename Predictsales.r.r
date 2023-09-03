library(tidyverse)
list.files(path = "../input")
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(e1071)
library(gbm)
library(data.table)
library(tictoc)
library(datasets)
library(ggthemes)
library(plotly)
library(Amelia)
library(caTools)
library(class)
library(scales)
library(corrplot)
train <- read.csv("/kaggle/input/competitive-data-science-predict-future-sales/sales_train.csv")
test<-read.csv("/kaggle/input/competitive-data-science-predict-future-sales/test.csv")
item <- read.csv("/kaggle/input/competitive-data-science-predict-future-sales/items.csv")
head(train)
head(test)
head(item)
str(train)
str(item)
train <- merge(train, item[,c("item_id", "item_category_id")], by = "item_id", all.x = T)
head(train)
str(train)
train$date <- as.Date(train$date, "%d.%m.%Y")
str(train)
train$year <- year(train$date)
train$month <- month(train$date)
train$day <- day(train$date)
train$weekday <- weekdays(train$date)
train$year <- as.factor(train$year)
train$month <- as.factor(train$month)
train$day <- as.factor(train$day)
train$weekday <- as.factor(train$weekday)
# Converting itemID,shopID,categoryID alsoin to factor variable

#  train$item_id <- as.factor(train$item_id)
#  train$shop_id <- as.factor(train$shop_id)
train$item_category_id <- as.factor(train$item_category_id)

str(train)
# missing Values and Duplicate Values
MissV <- as.data.frame(colSums(is.na(train)))
MissV

is.null(train)

DupV <- sum(duplicated(train))
DupV
# Lets check the correlation for all numeric variable

train1<- train %>% select_if(is.numeric)
CorM <- cor(train1)
corrplot(CorM,method = "color")

highCor <- findCorrelation(CorM,cutoff = .85,names = T)
# Sales shop wise

sales_shopwise <- train %>% group_by(shop_id) %>%
  summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))

options(scipen=100)
options(repr.plot.width = 15, repr.plot.height = 12)

ggplot(sales_shopwise,aes(x= reorder(shop_id,-item_cnt_day),y=item_cnt_day)) + geom_histogram(stat = "identity",bins=50,fill="orange")+
  xlab("ShopID")+ylab("ItemCount")+ggtitle("Shop wise sales")+theme(axis.text = element_text(size=11,angle=-45))
# sales item category wise

train %>% group_by(item_category_id) %>% summarize(item_cnt_day =  sum(item_cnt_day, na.rm = T)) %>% ggplot(aes(x= reorder(item_category_id,-item_cnt_day),y=item_cnt_day)) + geom_histogram(stat = "identity",bins=50,fill="orange")+
  xlab("CategoryID")+ylab("ItemCount")+ggtitle("Category wise sales")+theme(axis.text = element_text(size=11,angle=-45))
# most items in shop

train %>% group_by(shop_id) %>%
  summarise(item_id = n_distinct(item_id)) %>%  ggplot(aes(x= reorder(shop_id,-item_id),y=item_id,fill=factor(shop_id))) + geom_histogram(stat = "identity",bins=50)+
  xlab("ShopID")+ylab("Items in shop")+ggtitle("Most Items In Shop")+theme(axis.text = element_text(size=11,angle=-45))
# which catefgory of item is available most

train %>% group_by(item_category_id) %>% summarise(item_id =  n_distinct(item_id)) %>%
  ggplot(aes(x= reorder(item_category_id,-item_id),y=item_id,fill= factor(item_category_id)))+geom_col() + xlab(" Category ID")+ ylab(" Items in Category")+
  ggtitle("Most Items per Category")
# which item is most popular and most sold in the each shop

popularity  <-  train %>%
  group_by(shop_id, item_id) %>%
  summarise(sold_item_count = sum(item_cnt_day)) %>%
  # filter(sold_item_count == max(sold_item_count)) %>%
  arrange(desc(sold_item_count))

popular_items_in_shop <-  train %>%
  group_by(shop_id, item_id) %>%
  summarise(sold_item_count = sum(item_cnt_day)) %>%
  filter(sold_item_count == max(sold_item_count)) %>%
  arrange(desc(sold_item_count))

ggplot(data = popular_items_in_shop,
       mapping = aes(x = reorder(shop_id, sold_item_count),
                     y = sold_item_count,
                     fill = factor(item_id))) +
  geom_histogram(stat = "identity", color = "yellow") +
  xlab("") + ylab("Sales Count") +
  ggtitle("Most Popular Item per shop") +
  coord_flip()
  # which shop has most category of items
shop_with_most_category = train %>%
    select(shop_id, item_category_id) %>%
    group_by(shop_id) %>%
    summarise(category_count =  n_distinct(item_category_id)) %>%
    arrange(desc(category_count))

ggplot(data = shop_with_most_category,
       mapping = aes(x = reorder(shop_id, category_count),
                     y = category_count,
                     fill = factor(shop_id))) +
    geom_histogram(stat = "identity", color = "yellow") +
    xlab("Shop ID") + ylab("Item Category Count") +
    ggtitle("Most Item category per shop") +
    coord_flip()

# which item category is most popular and most sold in each shop

popular_category <- train %>% group_by(shop_id,item_category_id)  %>%
summarise(category_count = sum(item_cnt_day)) %>% filter(category_count == max(category_count)) %>% arrange(desc(category_count))

ggplot(popular_category,aes(x=reorder(shop_id,category_count),y=category_count))+geom_histogram(stat="identity", color = "yellow")+
xlab("ShoID")+ylab("CategoryCount")+ggtitle("Most popular item category per shop")+theme(axis.text= element_text(size = 11))

# which item category is highest sales grossing in all shops
most_grossing_category <- train %>% group_by(item_category_id) %>%
    summarise(total_gross = sum(item_cnt_day * item_price)) %>%
    arrange(desc(total_gross))

ggplot(most_grossing_category,
       aes(x = reorder(item_category_id, total_gross),
           y = total_gross,
           fill = factor(item_category_id))) +
    geom_col() +
    xlab("Category ID") + ylab("Total Gross")+
    ggtitle("Total Gross per Item category") +
    coord_flip()

# item categories available in each shop
item_category_in_shops = train %>%
    group_by(shop_id) %>%
    summarise(item_category =  paste(sort(unique(item_category_id)), collapse = ", "))
head(item_category_in_shops)

# which item gets sold  the most under which category

most_sold_item_per_category = train %>%
    group_by(item_category_id, item_id) %>%
    summarise(total_sales = sum(item_price * item_cnt_day)) %>%
    filter(total_sales == max(total_sales)) %>%
    arrange(desc(total_sales))


ggplot(most_sold_item_per_category,
       aes(x = reorder(item_category_id, total_sales),
           y = total_sales,
           fill = factor(item_id))) +
    geom_histogram(stat = "identity", color = "yellow") +
    labs(title = "Items sold per category",x = "Category ID", y = "Sales", fill = "Item ID") +
    coord_flip()

# Total Sales Month and Day wise

SalesMonthlyDaywise <- train %>% group_by(month,day) %>%
                summarise(TotalSales = sum(item_price * item_cnt_day)) %>%
                arrange(desc(TotalSales))

ggplot(SalesMonthlyDaywise,aes(x= day,y=TotalSales,group=month,color =  factor(month)))+ geom_line()+geom_point()+ xlab("Days") +ylab("TTotal Sales") + ggtitle("Total Sales month-day wise")

ggplot(SalesMonthlyDaywise,
       aes(x = day,
           y = TotalSales,
           fill =  factor(day))) +
    geom_histogram(stat = "identity", color = "yellow") +
    labs(title = "Total Sales month-day wise", x = "Days", y = "Total sales", fill = "Days") +
    facet_wrap(~month, ncol = 2)

#year wise total sales

AnnualSales <- train %>% group_by(year) %>% summarise(TotalSale= sum(item_price * item_cnt_day)) %>% arrange(desc(TotalSale))

ggplot(AnnualSales,aes(x=year,y=TotalSale,fill=factor(year)))+geom_col() + xlab("Year")+ylab("Total Sale")+ggtitle("Total Sales Annualy") + theme_bw()+ theme(axis.text.x = element_text(size=12,angle=-45),axis.text.y = element_text(size=12))

# year and month wise total sales

YMSales <- train %>% group_by(year,month) %>% summarise(YMSale = sum(item_price * item_cnt_day)) %>% arrange(year)

ggplot(YMSales,aes(x=month,y=YMSale,fill = factor(year))) +geom_col() +
    labs(title = "Yearly-Monthly Sales", x = "Months", y =  "Total sales", fill = "Year")

# number of items sold each day
daily_sale = train %>%
    group_by(date) %>%
    summarise(items_sold =  sum(item_cnt_day))

ggplot(daily_sale, aes(x =  date, y = items_sold, color =  items_sold)) +
    geom_line() +
    geom_point()+
    labs(title = "Daily Item sold", x =  "Date", y = "Items sold")

# items sold on weekdays
weekdays_item_sold = train %>%
    group_by(weekday) %>%
    summarise(item_sold = sum(item_cnt_day)) %>%
    arrange(desc(item_sold))

ggplot(weekdays_item_sold, aes(x =reorder(weekday, item_sold), y =  item_sold, fill = factor(weekday)))+
    geom_bar(stat = "identity", color = "yellow") +
    labs(title = "Items sold on weekdays", x = "Week Days", y =  "Items sold", fill = "Week Days") +
    geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = item_sold))

# sale revenue on weekdays

weekdays_sales <- train %>%
    group_by(weekday) %>%
    summarise(total_sale = sum(item_cnt_day * item_price)) %>%
    arrange(desc(total_sale))
weekdays_sales$total_sale = round(weekdays_sales$total_sale, 2)

ggplot(weekdays_sales, aes(x =reorder(weekday, total_sale), y =  total_sale, fill = factor(weekday)))+
    geom_bar(stat = "identity", color ="yellow") +
    labs(title = "Sales on weekdays", x = "Week Days", y =  "Items sold", fill = "Week Days") +
   
    geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = total_sale))

#model
library(tictoc)
library(gbm)
tic("Time Taken to Run GBM Model ")
gbm_model  <-  gbm(item_cnt_day ~ shop_id + item_id,
                  data = train,
                  shrinkage = 0.01,
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 5,
                  bag.fraction = 0.5,
                  train.fraction = 0.8,
                  # cv.folds = 5,
                  n.cores = -1,
                  verbose = T)

toc()

result2 <- predict(gbm_model,newdata = test[,c("shop_id","item_id")], n.trees = 1000)

sub2 <- data.frame(ID = test$ID,
                  item_cnt_month =  result2)

write.csv(sub2, "submission.csv", row.names = F)

#predictionn formula for linear regression

q <- predict(linear_model, newdata=(data.frame(month = c(13:24))))

q
