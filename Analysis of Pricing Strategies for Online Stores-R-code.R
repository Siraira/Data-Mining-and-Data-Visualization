#Details of the project----
# price spy data , sweden
# group 4
# total observation is more than 14 million with 14 variables
#Data Visualization course, Dalarna University
# install the needed packages.----

install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gtsummary")
install.packages("scales")
install.packages("broom")
install.packages("gridExtra")
install.packages("knitar")
install.packages("scales")
install.packages("flextable")
install.packages("crosstable")

# load the libraries.----

library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(scales)
library(broom)
library(gridExtra)
library(knitr)
library(scales)
library(flextable)
library(crosstable)

# get the working directory to put the data in it.----

getwd()

# load the data ----

pricedata<- read.csv("homedata.csv")

# exploring the data----

str(pricedata)
dim(pricedata)
summary(pricedata)
class(pricedata)
variable.names(pricedata)

#factorizing the data----

pricedata$category <- as.factor(pricedata$category)
pricedata$weekday<- as.factor(pricedata$weekday)
pricedata$week<- as.factor(pricedata$week)
pricedata$product_id<- as.factor(pricedata$product_id)
pricedata$store_id<- as.factor(pricedata$store_id)
pricedata$year<- as.factor(pricedata$year)
pricedata$date<- as.Date(pricedata$date)
pricedata$month_numerical<- as.factor(pricedata$month_numerical)
pricedata$total_products_in_category<- as.factor(pricedata$total_products_in_category)
#pricedata$last_digit<- as.factor(pricedata$last_digit)

# explore each variable----
#variable 1:product id----

unique(pricedata$product_id)

pricedata %>%
  group_by(product_id) %>%
  tally


#variable 2:date----

unique(pricedata$date)
range(pricedata$date)

pricedata %>%
  group_by(date) %>%
  tally

#variable 3:category----

unique(pricedata$category)
table(pricedata$category)

pricedata %>%
  group_by(category) %>%
  tally
options(scipen = 500)
barplot(table(pricedata$category),ylab = "frequency",angle = 45 , ylim = c(0,10000000))


#variable 4 :price----

summary(pricedata$price)
unique(pricedata$price)
mean(pricedata$price)
var(pricedata$price)
sd(pricedata$price)

pricedata %>%
  group_by(price) %>%
  tally

hist(pricedata$price)

#variable 5:weekday----

unique(pricedata$weekday)
table(pricedata$weekday)
pricedata %>%
  group_by(weekday) %>%
  tally

barplot(table(pricedata$weekday))

#variable 6: week----

unique(pricedata$week)

pricedata %>%
  group_by(week) %>%
  tally
table(pricedata$week)
barplot(table(pricedata$week))

#variable 7:store id----

unique(pricedata$store_id)

pricedata %>%
  group_by(store_id) %>%
  tally

#variable 8 :year----

unique(pricedata$year)

pricedata %>%
  group_by(year) %>%
  tally

table(pricedata$year)

barplot(table(pricedata$year))

#variable 9 :month numerical----

unique(pricedata$month_numerical)

pricedata %>%
  group_by(month_numerical) %>%
  tally
table(pricedata$month_numerical)

barplot(table(pricedata$month_numerical))

#variable 10 :cpi adjusted price----

summary(pricedata$cpi_adjusted_price)
unique(pricedata$cpi_adjusted_price)
mean(pricedata$cpi_adjusted_price)
var(pricedata$cpi_adjusted_price)
sd(pricedata$cpi_adjusted_price)

pricedata %>%
  group_by(cpi_adjusted_price) %>%
  tally

hist(pricedata$cpi_adjusted_price)

#variable 11 :log of cpi adjusted price----

summary(pricedata$log_of_cpi_adjusted_price)
unique(pricedata$log_of_cpi_adjusted_price)
mean(pricedata$log_of_cpi_adjusted_price)

pricedata %>%
  group_by(log_of_cpi_adjusted_price) %>%
  tally

hist(pricedata$log_of_cpi_adjusted_price)

#variable 12 :number of store per product per day----

summary(pricedata$number_of_store_per_product_and_day)
unique(pricedata$number_of_store_per_product_and_day)
mean(pricedata$number_of_store_per_product_and_day)

pricedata %>%
  group_by(number_of_store_per_product_and_day) %>%
  tally

table(pricedata$number_of_store_per_product_and_day)

barplot(table(pricedata$number_of_store_per_product_and_day))

#variable 13 :total days for product----

summary(pricedata$total_days_for_product)
unique(pricedata$total_days_for_product)
mean(pricedata$total_days_for_product)

pricedata %>%
  group_by(total_days_for_product) %>%
  tally

#variable 14 :total products in category----

summary(pricedata$total_products_in_category)
unique(pricedata$total_products_in_category)
pricedata %>%
  group_by(total_products_in_category) %>%
  tally

table(pricedata$total_products_in_category)
barplot(table(pricedata$total_products_in_category))

# see different variables together----

table(pricedata$category,pricedata$year)

table(pricedata$weekday,pricedata$year)

table(pricedata$month_numerical,pricedata$year)

table(pricedata$total_products_in_category,pricedata$year)

table(pricedata$category,pricedata$total_products_in_category)

table(pricedata$weekday,pricedata$category)



# select randomly----

set.seed(12345)

stores<-pricedata %>%
  group_by(store_id) %>%
  tally   

rows <- sample(nrow(stores), 2)


random_numbers <- stores[rows, "store_id"]

# select two stores with the highest number of products----

ourdata<-pricedata[pricedata$store_id %in% c(3303 ,112),]

# factorizing  our selected data----

ourdata$category <- as.factor(ourdata$category)
ourdata$weekday<- as.factor(ourdata$weekday)
ourdata$week<- as.factor(ourdata$week)
ourdata$product_id<- as.factor(ourdata$product_id)
ourdata$store_id<- as.factor(ourdata$store_id)
ourdata$year<- as.factor(ourdata$year)
ourdata$date<- as.Date(ourdata$date)
ourdata$month_numerical<- as.factor(ourdata$month_numerical)
ourdata$total_products_in_category<- as.factor(ourdata$total_products_in_category)
#ourdata$last_digit<- as.factor(ourdata$last_digit)
#ourdata$price_type<-as.factor(ourdata$price_type)

# split the data into two parts----
# according to prices that end with 9----

price9_ourdata<-ourdata[ourdata$price %% 10==9,]

# according to prices that does not end with 9----

pricenot9_ourdata<-ourdata[ourdata$price %% 10!=9,]

# look at the data with prices that end with 9----

summary(price9_ourdata)
mean(price9_ourdata$price)
mean(price9_ourdata$cpi_adjusted_price)
sd(price9_ourdata$price)

# look at the data with prices that does not end with 9----

summary(pricenot9_ourdata)
mean(pricenot9_ourdata$price)
mean(pricenot9_ourdata$cpi_adjusted_price)
sd(pricenot9_ourdata$price)

# conduct student t test to check whether the mean for the two group is the same----
# the null hypothesis is: the mean for the two groups is equal
# the alternative hypothesis is :the mean for the two groups is not equal
# Perform the t-test for all the data----


# process the data first----

store_112 <- ourdata %>% 
  filter(store_id== 112)

store_112$price_type <- ifelse(store_112$price %% 10 == 9, "9-Ending Price", "Non 9-Ending Price")

store_3303 <- ourdata %>% 
  filter(store_id== 3303)


store_3303$price_type <- ifelse(store_3303$price %% 10 == 9, "9-Ending Price", "Non 9-Ending Price")


store_112_9<-store_112[store_112$price %% 10==9,]
store_112_not9<-store_112[store_112$price %% 10!=9,]

store_3303_9<-store_3303[store_3303$price %% 10==9,]
store_3303_not9<-store_3303[store_3303$price %% 10!=9,]

# t test for all the data----
set.seed(123)

price9_sample <- sample_n(price9_ourdata, 200000)

pricenot9_sample <- sample_n(pricenot9_ourdata, 200000)

ttest <- t.test(price9_sample$price, pricenot9_sample$price)

ttest<-tidy(ttest)
ttest
#t_test_results
# Create a table grob using the tableGrob() function from the gridExtra package
table_grob1 <- tableGrob(ttest)

# Create a blank ggplot with no axes or labels
plot_grob1 <- ggplot() +
  theme_void() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

# Combine the table grob and the plot grob using the grid.arrange() function from the gridExtra package
grid.arrange(plot_grob1, table_grob1, ncol = 2, widths = c(.2, 0.9))

# Perform the t-test for store 112----

set.seed(123)

price9_sample_112 <- sample_n(store_112_9, 100000)

pricenot9_sample_112 <- sample_n(store_112_not9, 100000)

ttest <- t.test(price9_sample_112$price, pricenot9_sample_112$price)

ttest<-tidy(ttest)

#t_test_results
# Create a table grob using the tableGrob() function from the gridExtra package
table_grob1 <- tableGrob(ttest)

# Create a blank ggplot with no axes or labels
plot_grob1 <- ggplot() +
  theme_void() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

# Combine the table grob and the plot grob using the grid.arrange() function from the gridExtra package
grid.arrange(plot_grob1, table_grob1, ncol = 2, widths = c(.2, 0.9))

# Perform the t-test for store 3303----

set.seed(123)
price9_sample_3303 <- sample_n(store_3303_9, 100000)

pricenot9_sample_3303 <- sample_n(store_3303_not9, 100000)

ttest <- t.test(price9_sample_3303$price, pricenot9_sample_3303$price)

ttest<-tidy(ttest)

#t_test_results
# Create a table grob using the tableGrob() function from the gridExtra package
table_grob1 <- tableGrob(ttest)

# Create a blank ggplot with no axes or labels
plot_grob1 <- ggplot() +
  theme_void() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

# Combine the table grob and the plot grob using the grid.arrange() function from the gridExtra package
grid.arrange(plot_grob1, table_grob1, ncol = 2, widths = c(.2, 0.9))


# plot the prices for the two products----
# our data
# all the data both 112 anfd 3303

ggplot(ourdata, aes(x=factor(store_id), y=price, group=factor(store_id))) + 
    geom_boxplot() +
    xlab(expression(bold("Store_ id"))) +
    ylab(expression(bold("Prices of products "))) +
    theme_classic()+
    ggtitle(expression(bold("Price comparison between Store # 3303 and Store # 112")))

# plot the prices that end with 9 for the two stores 112, 3303----

ggplot(price9_ourdata, aes(x=factor(store_id), y=price, group=factor(store_id))) + 
  geom_boxplot() +
  xlab(expression(bold("Store_ id"))) +
  ylab(expression(bold("Prices of products "))) +
  theme_classic()+
  ggtitle(expression(bold("Price that end with 9 comparison between Store # 3303 and Store # 112")))
          

# plot the prices that does not end with 9 for the two stores 112, 3303----

ggplot(pricenot9_ourdata, aes(x=factor(store_id), y=price, group=factor(store_id))) + 
  geom_boxplot() +
  xlab(expression(bold("Store_ id"))) +
  ylab(expression(bold("Prices of products "))) +
  theme_classic()+
  ggtitle(expression(bold("Price that does not end with 9 comparison between Store # 3303 and Store # 112")))


# plot the prices that end with 9 for nine product categories----

ggplot(price9_ourdata, aes(x=factor(category), y=price, group=factor(category))) + 
  geom_boxplot() +
  xlab(expression(bold("Category of the product"))) +
  ylab(expression(bold("Prices of products "))) +
  theme_classic()+
  ggtitle(expression(bold("Category Prices for products that with prices that end with 9")))

# plot the prices that end with 9 for nine product categories with facet wrap by category----

ggplot(price9_ourdata, aes(x=factor(category), y=price, group=factor(category))) + 
  geom_boxplot() +
  xlab(expression(bold("Category of the product"))) +
  ylab(expression(bold("Prices of products "))) +
  theme_classic()+
  facet_wrap(~category,scales = "free")+
  ggtitle(expression(bold("Category Prices for products that end with 9")))

# plot the prices that does not end with 9 for nine product categories with facetwrap by category----

ggplot(pricenot9_ourdata, aes(x=factor(category), y=price, group=factor(category))) + 
  geom_boxplot() +
  xlab(expression(bold("Category of the product"))) +
  ylab(expression(bold("Prices of products "))) +
  theme_classic()+
  facet_wrap(~category,scales = "free")+
  theme(axis.text.x = element_text(face = "bold"),axis.text.y = element_text(face = "bold"))+
  ggtitle(expression(bold("Category Prices for products with prices that does not end with 9")))


# plot the prices that end with 9 for the two stores 112, 3303, and prices that does not end with 9----


ourdata$price_type <- ifelse(ourdata$price %% 10 == 9, "9-Ending Price", "Non 9-Ending Price")



#boxplot for the price in both stores 112 and 3303----
# did not work .. need fixing
ggplot(ourdata, aes(x =price_type, y = price, fill = price_type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("steelblue", "orange")) +
  labs(title = "Prices by Store and Price Type",
       x = "Store",
       y = "Price",
       fill = "Price Type")+
  stat_summary(fun.y = median, geom = "point", shape = 20, size = 2, color = "black", position = position_dodge(width = 0.75)) +
  stat_summary(fun.y = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1.5, position = position_dodge(width = 0.75))

#box plot for store_id = 112

ggplot(store_112, aes(x =price_type, y = price, fill = price_type)) +
  geom_boxplot() +
  scale_y_log10()+
  theme_classic()+
  scale_fill_manual(values = c("steelblue", "orange")) +
  labs(title = expression(bold("Prices by Store 112 and Price Type")),
       x = expression(bold("Store_ID")),
       y = expression(bold("Price")),
       fill = "Price Type")+
stat_summary(fun.y = median, geom = "point", shape = 20, size = 2, color = "black", position = position_dodge(width = 0.75)) +
  stat_summary(fun.y = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1.5, position = position_dodge(width = 0.75))
  

class(store_112)
#boxplot for store_id = 3303

ggplot(store_3303, aes(x =price_type, y = price, fill = price_type)) +
  geom_boxplot() +
  scale_y_log10()+
  theme_classic()+
  scale_fill_manual(values = c("steelblue", "orange")) +
  labs(title = expression(bold("Prices by Store 3303 and Price Type")),
       x = expression(bold("Store_ID")),
       y = expression(bold("Price")),
       fill = expression(bold("Price Type")))+
  stat_summary(fun.y = median, geom = "point", shape = 20, size = 2, color = "black", position = position_dodge(width = 0.75)) +
  stat_summary(fun.y = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1.5, position = position_dodge(width = 0.75))


#boxplot for store 112----
ggplot(store_112, aes(x =factor(store_id), y = price, fill = price_type)) +
  geom_boxplot() +
  scale_y_log10()+
  theme_classic()+
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = expression(bold("Prices by Store 112 and Price Type")),
       x = expression(bold("Store_ID")),
       y = expression(bold("Price")),
       fill = "Price Type")+
  stat_summary(fun.y = median, geom = "point", shape = 20, size = 2, color = "black", position = position_dodge(width = 0.75)) +
  stat_summary(fun.y = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, position = position_dodge(width = 0.75))
  

# see the prices based on price ending----

pricedata$last_digit<-as.numeric(substring(pricedata$price,nchar(pricedata$price)))

# check prices end for store 112 and 3303----


ourdata$last_digit<-as.numeric(substring(ourdata$price,nchar(ourdata$price)))

# plot the prices by the last digit----
# bar plot for our selected data , our data


ggplot(ourdata, aes(x = last_digit)) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  theme_classic()+
  labs(x = expression(bold("Last Digit of Price")), y =expression(bold( "Frequency")), 
       title = expression(bold("Price Distribution by Last Digit for store_id 112 and 3303 "))) +
  scale_x_discrete(breaks = 0:9)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+ geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100), "%")), vjust = -1)

# check prices end for store 112 ----


store_112$last_digit<-as.numeric(substring(store_112$price,nchar(store_112$price)))

# plot the prices last digit----
# bar plot for store 112 by last digit

ggplot(store_112, aes(x = last_digit)) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  theme_classic()+
  labs(x = expression(bold("Last Digit of Price")), y =expression(bold( "Frequency")), 
       title = expression(bold("Price Distribution by Last Digit for store_id 112"))) +
  scale_x_discrete(breaks = 0:9)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+ geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100), "%")), vjust = -1)

# check prices end for store 3303----


store_3303$last_digit<-as.numeric(substring(store_3303$price,nchar(store_3303$price)))

# plot the prices last digit----
# bar plot for store 3303 by last digit


ggplot(store_3303, aes(x = last_digit)) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  labs(x = expression(bold("Last Digit of Price")), y =expression(bold( "Frequency")), 
       title = expression(bold("Price Distribution by Last Digit for store_id 3303"))) +
  scale_x_continuous(breaks = 0:9)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+ geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100), "%")), vjust = -1)

# look at the data----
summary(pricenot9_ourdata)
summary(price9_ourdata)
#  chi-square test:included in the paper
#Chi-Square test for all the data in 112 and 3303 stores for 9 ending prices----

# Create a contingency table of last digit of price and product category

subset_data <- ourdata[ourdata$price_type == "9-Ending Price",]

contingency_table_price_category11 <- table(subset_data$category, subset_data$price_type)

# Perform the chi-square test
chi_sq_test <- chisq.test(contingency_table_price_category11)

# Print the results
cat("Chi-square test results:\n")
print(chi_sq_test)

#Chi-Square test for all the data in 112 and 3303 stores----


# Create a contingency table of last digit of price and product category
contingency_table_price_category <- table(ourdata$category, ourdata$price %% 10)

# Perform the chi-square test
chi_sq_test <- chisq.test(contingency_table_price_category)

# Print the results
cat("Chi-square test results:\n")
print(chi_sq_test)


#see last price digit by year for store 3303----

ggplot(store_3303, aes(x = last_digit)) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  facet_wrap(~year)+
  labs(x = expression(bold("Last Digit of Price")), y =expression(bold( "Frequency")), 
       title = expression(bold("Price Distribution by Last Digit for store_id 3303"))) +
  scale_x_continuous(breaks = 0:9)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+ geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100), "%")), vjust = -1)

#see last price digit by year for store 112----
ggplot(store_112, aes(x = last_digit)) +
  facet_wrap(~year)+
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  labs(x = expression(bold("Last Digit of Price")), y =expression(bold( "Frequency")), 
       title = expression(bold("Price Distribution by Last Digit for store_id 112"))) +
  scale_x_continuous(breaks = 0:9)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+ geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100), "%")), vjust = -1)

#see last price digit by year for all data----
price_9_freq<-aggregate(price~date, data = price9_ourdata,length)

ggplot(price_9_freq, aes(x = date,y=price)) +
  geom_line() +
  labs(x = expression(bold("year")), y =expression(bold( "Frequency")), 
       title = expression(bold("Frequency of Price Ending 9 over Years "))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

#create a line plot for the price 9 over years


ggplot(ourdata, aes(x = last_digit)) +
  facet_wrap(~year)+
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  labs(x = expression(bold("Last Digit of Price")), y =expression(bold( "Frequency")), 
       title = expression(bold("Price Distribution by Last Digit for store_id 112 and 3303 ")))
# Create a cross table for category and price type----

#cross table----
table_category_price_type<- crosstable(ourdata, c(category),by=price_type,percent_digits=0,percent_pattern = "{n} ({p_row})") %>%
  as_flextable()%>%
  colformat_num(2, digits = 2, suffix = "%")

table_category_price_type

R.version.string
