#Final Project R Code:
#Authors - Namrita Gupta & Akanksha Sharma

#load rawdata.rdata to see initial data set
#OR Dunn_Final.rdata to jump directly to univariate analysis (Line No 105) and skip data merging, aggregation, and cleaning

#Importing CSV files:
transaction<-read.csv(file.choose(),header=T)
hh_demographic<-read.csv(file.choose(),header=T)
product<-read.csv(file.choose(),header=T)
DAY_MAPPING<-read.csv(file.choose(), header = T)

View(transaction)
View(product)
View(hh_demographic)
View(DAY_MAPPING)

#Merging datasets based on common columns as seen in ER Diagram

#Merging transaction's DAY column with DAY MAPPING's DAY column
transaction<-merge(transaction, DAY_MAPPING, by="DAY")
#Merging transaction and Brand,Sub_commodity_desc attribute from product using PRODUCT_ID. 
transaction_product<-merge(transaction, product[ , c("PRODUCT_ID","BRAND","SUB_COMMODITY_DESC")], by=c("PRODUCT_ID","PRODUCT_ID"))
#transaction_product<-merge(transaction_product, product[ , c("PRODUCT_ID","SUB_COMMODITY_DESC")], by=c("PRODUCT_ID","PRODUCT_ID"))

#Adding weekday information:
transaction_product$DATE<-as.Date(transaction_product$DATE,format='%m/%d/%Y')
transaction_product$Day_of_Week<-weekdays(transaction_product$DATE)
#Creating weekend flag
transaction_product$weekend_flag <- ifelse(transaction_product$Day_of_Week %in% c("Saturday","Sunday"),1,0)
#Creating Flag for National Products Purchased
transaction_product$National_Flag <- ifelse(transaction_product$BRAND %in% "National",1,0)

#calculate the Recency(days) with respect to end Date, the smaller days value means more recent
transaction_product$endDate<-max(transaction_product$DATE)
transaction_product$Recency<-as.numeric(difftime(transaction_product$endDate,transaction_product$DATE,units="days"))

#aggregating data at household level:
install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)
#We intend to obtain count of unique values of some of the columns and that's where we want to use uniqueN function from data.table package
#Aggregating at household level:
Dunn1 <- transaction_product  %>% group_by(household_key) %>% summarize(total_sales = sum(SALES_VALUE), products_purchased=sum(QUANTITY), no_of_stores=uniqueN(STORE_ID), no_of_orders=uniqueN(BASKET_ID), total_visits=uniqueN(DAY), no_of_national_products=sum(National_Flag), recency=min(Recency)+1)
#Calculating weekend visits for each household:
Dunn2 <- transaction_product %>% group_by(household_key, DATE) %>% summarize(weekend_visits_count=sum(weekend_flag))
Dunn2$weekend_visits <- ifelse(Dunn2$weekend_visits_count!=0,1,0)
Dunn3 <- Dunn2 %>% group_by(household_key) %>% summarize(weekend_visits=sum(weekend_visits))
#Merging weekend_visits info with Dunn2
Dunn_Final <- merge(Dunn1, Dunn3, by="household_key")

#The household data is cleaned and treated in excel before importing
#Columns removed (Large % of missing values as well as they were redundant)-MARITAL_STATUS_CODE,HOMEOWNER_DESC,HH_COMP_DESC,KID_CATEGORY_DESC
#Discretization of remaining categorical columns-AGE_DESC,INCOME_DESC,HOUSEHOLD_SIZE_DESC

#Importing household demographic data
install.packages("readxl")
library("readxl")
demographic<-read_excel("demography_hh.xlsx")

#Checking data types of all columns
str(Dunn_Final)
#Converting household key to factor:
Dunn_Final$household_key<-as.factor(Dunn_Final$household_key)
#Adding percentages for national brand products and weekend_visits purchased for better exploration:
Dunn_Final$BrandNational <- (Dunn_Final$no_of_national_products/Dunn_Final$products_purchased)*100
Dunn_Final$PercentWeekendVisits <- (Dunn_Final$weekend_visits/Dunn_Final$total_visits)*100
#Calculating Average Basket Size for each household:
Dunn_Final$Average_Basket_Size<-Dunn_Final$products_purchased/Dunn_Final$no_of_orders

#Data Cleaning:

#Checking for NAs:
sum(is.na(Dunn_Final))

#Univariate Analysis:
summary(Dunn_Final)
#We see some unexpected results in summary output especially for Average_Basket_Size, it looks right skewed
#Average_Basket_Size shows number of products purchased by a given household averagred for all visits
#We suspect there might be few products that are purchased in very large quantities and this might be the reason for this skewness
#To delve further, we aim to calculate percentage of products purchased and sales produced by those products:
x<- transaction_product %>% group_by(SUB_COMMODITY_DESC) %>% summarize(quantity_sold=sum(QUANTITY), sales_produced=sum(SALES_VALUE)) %>% arrange(-quantity_sold)
x$average_product_price<-x$sales_produced/x$quantity_sold
head(x)
#We see product like Gasoline-Reg-Unleaded have very low product price, nearly 0.2 cents and are purchased in very high quantities
#We suspect this could be a major cause of skewness and hence we remove all instances of product- Gasoline-Reg-Unleaded from our consideration
transaction_product1<-transaction_product[which(transaction_product$SUB_COMMODITY_DESC!="GASOLINE-REG UNLEADED"),]

#There were 14466 records where the sales and quantity were zero, so we removed them from our dataset
transaction_product2<-transaction_product1[which(transaction_product1$QUANTITY>0),]

#Repeating above code to recreate Dunn_Final
transaction_product<-transaction_product2
#Re run code from line 45 to 78
summary(Dunn_Final)

#Univariate Analysis:
Dunn_Final1<-Dunn_Final #creating a backup
plot(density(Dunn_Final$total_sales)) #right skewed
#trying out transformations:
plot(density(1/Dunn_Final$total_sales^0.5)) #slightly better
plot(density(1/Dunn_Final$total_sales^2)) # does not help
plot(density(1/Dunn_Final$total_sales)) # little more better
plot(density(log(Dunn_Final$total_sales))) # best plot
#removing outliers
b<-boxplot(log(Dunn_Final$total_sales))
c<-b$out
max(c)
#less than max(c) must be removed
Dunn_Final<-Dunn_Final[which(log(Dunn_Final$total_sales)>max(c)),]
boxplot(log(Dunn_Final$total_sales))
plot(density(log(Dunn_Final$total_sales)), main = "Distribution of total_sales") # data is now normally distributed

#We see no_of_national_products and weekend_visits are represented as percentages and hence we drop these columns:
#Dunn_Final$weekend_visits<-NULL
#Dunn_Final$no_of_national_products<-NULL
sum(is.na(Dunn_Final))

#Data is now free of any NAs and our dependent variable has a normal distribution, thus the data is clean for further analysis
#Note: Everywhere we would be using log(total_sales) as this transformation gave us normal distribution

#For creating histogram raw demographic data is merged with the indexed demographic to get the data labels
demographic1<-merge(demographic, hh_demographic[ , c("household_key","AGE_DESC","INCOME_DESC","HOUSEHOLD_SIZE_DESC")], by=c("household_key","household_key"))

#Exploratory Data Analysis on Demographic data by creating histograms 
# Histogram of Customers Age
counts <- table(demographic1$AGE_INDEX)
barplot(counts, main="Histogram of Customers Age", 
        xlab="Age_Index",col="blue",ylim=c(0,300))

# Histogram of Customers Income
counts <- table(demographic1$INCOME_INDEX)
barplot(counts, main="Histogram of Customers Income", 
        xlab="Income_Index",col="orange",ylim=c(0,250))

# Histogram of Customers Household size
counts <- table(demographic1$HOUSEHOLD_SIZE_INDEX)
barplot(counts, main="Histogram for Customers Household Size", 
        xlab="Household_size_Index",col="red",ylim=c(0,350))

# Merging the cleaned demographics and transaction data
trans_demo<-merge(Dunn_Final, demographic, by=c("household_key"))

# Rounding up Brand National (renaming to BrandNationalPercent) and Percent weekend visist
trans_demo$BrandNational<-round(trans_demo$BrandNational,0)
trans_demo = rename(trans_demo, BrandNationalPercent = 'BrandNational')
trans_demo$PercentWeekendVisits<-round(trans_demo$PercentWeekendVisits,0)
trans_demo$total_sales_log<-log(trans_demo$total_sales)

#exporting the merged dataset to WEKA by converting it as csv file
write.csv(trans_demo, "C:/Users/Namrita15/Desktop/Rutgers/Fundamentals of analytics/Project/trans_demo_10_12.csv")

#After exporting to the above "trans_demo" to excel, total_sales_log attribute was changed from numeric to categorical 
#and then imported in WEKA
#Modelling techniques were all executed in WEKA
#Thankyou!!

