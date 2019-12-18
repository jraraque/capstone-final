
################################
# Capstone Project: Customer attrition for online retailer
# Prepared by piRSquare
# December, 2019
# WARNING: it takes 12 minutes to run this code on a Core i7 laptop
# 2 minutes to download the data, or longer depending on internet bandwidth
# 9.5  minutes to run cross-validations for knn and random forest
# 0.5 minutes for the rest of calculations
################################


###############################
# Data Source: UCI Machine Leaarning Repository
# Dataset On-Line Retail II
# Contributed by: Dr. Daqing Chen, chend@lsbu.ac.uk, School of Engineering, London South Bank University, UK
###############################


###############################
# Upload libries
###############################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(forecast)
library(readxl) # upload library to read data from excel, it is part of tidyverse package
library(gridExtra)

##############################
# Download data from UCI
# Note: this process could take a couple of minutes
##############################

# Online retailer dataset:
# https://archive.ics.uci.edu/ml/datasets/Online+Retail+II
# https://archive.ics.uci.edu/ml/machine-learning-databases/00502/online_retail_II.xlsx


dl <- tempfile(fileext= ".xlsx") # create temporary file with the correct extension
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00502/online_retail_II.xlsx", dl, mode = "wb") #use mode wb

# read sheets 1 and 2 from data file in excel
dat1 <- read_xlsx(dl, sheet = 1)
dat2 <- read_xlsx(dl, sheet = 2)

# consolidate data from both sheet in a single file, and after that remove single-sheet files and teporary file dl
data <-rbind(dat1,dat2)
rm(dat1,dat2,dl)


##############################
# Exploratory analysis
##############################
# Count invoices, products and customers
table1 <- data %>%  summarize(Number_records = n(),
                              Number_invoices = n_distinct(Invoice), 
                              Number_products = n_distinct(StockCode), 
                              Number_customers = n_distinct(`Customer ID`))
table1

# check for NAs
data %>%  select(Invoice, InvoiceDate, Quantity, Price, `Customer ID`) %>% 
        summarize(Invoice_NA = sum(is.na(Invoice)), 
                  InvoiceDate_NA = sum(is.na(InvoiceDate)), 
                  Quantity_NA = sum(is.na(Quantity)), 
                  Price_NA =sum(is.na(Price)),
                  CustomerID_NA = sum(is.na(`Customer ID`)))
# create table with largest 3 quantity outliers, and largest 3 price outliers
rbind(top_n(data, 3, Quantity),top_n(data, -3, Quantity)) %>% arrange(InvoiceDate)
rbind(top_n(data, 3, Price),top_n(data, -3, Price)) %>% arrange(InvoiceDate)




# count days per month and plot them
calendar <- data %>% 
    mutate(month=floor_date(InvoiceDate,"month"), day=floor_date(InvoiceDate,"day")) %>% 
    group_by(month) %>% 
    summarize(active_days=n_distinct(day))
# plot(calendar, type = "b", main = "Count of active days per month")
calendar %>% ggplot(aes(month, active_days)) + geom_point() + geom_line() + 
              ggtitle("Count of active days per month") +
              geom_text(aes(x = month[1], y = active_days[1]-1, label = "Dec 2009")) + 
              geom_text(aes(x = month[13], y = active_days[13]-1, label = "Dec 2010")) +
              geom_text(aes(x = month[25], y = active_days[25]+1, label = "Dec 2011"))

# count activity by day of the week and plot it
calendar <- data %>% 
  mutate(day=floor_date(InvoiceDate,"day")) %>% 
  distinct(day) %>%  mutate( week_day = wday(day, label = TRUE)) %>% group_by(week_day) %>% summarize(active_days = n())
# barplot(table(calendar$week_day), ylab = "Days", main = "Count ot active days per day of the week")
calendar %>% ggplot(aes(week_day,active_days))+ geom_bar(stat= "identity") + ggtitle("Count ot active days per day of the week")

# explore sales seasonality
data_month <- data %>% 
          mutate(month = floor_date(InvoiceDate,"month")) %>%  
          group_by(month) %>% 
          summarize(Sales = sum(Price*Quantity))
data_month %>% ggplot(aes(month, Sales)) +
                geom_point(size = 3, alpha = .6, color = "grey") + geom_line() + ggtitle("Sales by month") +
                geom_smooth(color="red", span=0.4, method = "loess", se= FALSE) + 
                geom_text(aes(x= month[1], y= Sales[1]+10000), label = "Short term trend", color="red") +
                geom_smooth(color="blue", span=2, method = "loess", se= FALSE) +
                geom_text(aes(x= month[13], y= Sales[1]+10000), label = "Annual trend", color="blue")

# plot autocorrelations
ggAcf(ts(data_month$Sales), main= "Autocorrelation by month")

# explore customers

# calculate sales by customer ID, and sort decreasingly
customers <- data %>% group_by(`Customer ID`) %>%  summarize( LineItems=n(), Qty = sum(Quantity), Sales = sum(Price*Quantity)) %>% arrange(desc(Sales))
# calculate median and max customer sales
sales_median <- median(customers$Sales)
sales_max <- max(customers$Sales)

# make sales histogram 
customers %>% ggplot(aes(Sales))+geom_histogram(binwidth=0.2, color="black") +
              scale_x_continuous(trans ="log10", breaks = 10^seq(0,7)) + 
              ggtitle("Customer Sales Histogram using log scale") +
              ylab("Customer count") +
              geom_text(aes(x=10000, y=800), label=paste0("  Median = ", sales_median, " sterling pounds")) +
              geom_text(aes(x=500000, y=100), label=paste0("Largest customer = ", round(sales_max/1000000,1), " million sterling pounds"))

top_n(customers,5)

# prepare data to explore impact of customers without ID, by month
data_graph <- data %>% 
        mutate(month = floor_date(InvoiceDate,"month"), CustomerGroup = ifelse(is.na(`Customer ID`),"No_ID","Has_ID")) %>% 
        group_by(month, CustomerGroup) %>% 
        summarize(Sales = sum(Price*Quantity))
# make histogram by month, splitting customers without ID from customers with ID        
data_graph %>%  ggplot(aes(month,Sales, fill=CustomerGroup))+geom_bar(stat="identity") + ggtitle("Sales by month by customer group")

# prepare data set to create Pareto chart of customer sales
# customers are sorted decreasing by sales
# estimate how many customers are needed to achieve 80% of the total sales
TotSales  <- sum(customers$Sales)

sales_pareto <- data.frame(c=seq(6.5,0,-0.1)) # select points to plot in the x-axis, using log scale
sales_pareto <- sales_pareto %>% group_by(c) %>% 
          mutate(
                  sales_cut = 10^c, # find cut in sterling pouns, by transforming from log scale
                  cust_perc = mean(customers$Sales >= sales_cut), # percentage of customers above the cut
                  cust_count = sum(customers$Sales >= sales_cut), # count of customers above the cut
                  sales_cum = sum(customers$Sales*(customers$Sales >= sales_cut))/TotSales # cumulative sales asa percentage
                  )
# calculate number of customers for which cumulative sales are 80%, i.e. the Pareto point
index  <- first(which(sales_pareto$sales_cum >=0.8))
pareto_point <- ((0.8-sales_pareto$sales_cum[index-1])*sales_pareto$cust_count[index] + (sales_pareto$sales_cum[index]-0.8)*sales_pareto$cust_count[index-1])/(sales_pareto$sales_cum[index]-sales_pareto$sales_cum[index-1])

# plot Pareto chart using the data set prepared
sales_pareto %>% ggplot(aes(cust_count,sales_cum)) +
                geom_line() +
                geom_segment(x = 0, y = 0.8, xend = pareto_point, yend = 0.8, color = "black", lty = 2) +
                geom_segment(x = pareto_point, y = 0, xend = pareto_point, yend = 0.8, color = "black", lty = 2) +
                ggtitle("Pareto Sales") + ylab("Percentage of total sales") + xlab("Customer count") +
                geom_text(aes(x = pareto_point, y = 0.9, label = paste0("Pareto: 80% of sales with ",round(pareto_point)," customers;  ", round(100*pareto_point/max(cust_count),1)," % of customers")))

#############################################
# Customer attrition model
# define customer attrited = no sales in 2011
# build prediction model using 2010 customer sales purchase patterns
# explore 3 mathematical formulations for sales patterns
#############################################

# identify customers with 2011 sales >0
act2011 <- data %>% filter(year(InvoiceDate)==2011) %>% group_by(`Customer ID`) %>% summarize(active2011 = ifelse(sum(Price*Quantity)>0,"1","0"))

# prepare 2010 data set
# Sales = total sales by customer by month
# activity = 1 if sales >0, 0 otherwise, by customer by month
# active2011 =1 if active in 2011, 0 if not active
data2010 <- data %>% 
              filter(year(InvoiceDate)==2010) %>% 
              group_by(`Customer ID`,month=month(InvoiceDate)) %>% 
              summarize(sales=sum(Price*Quantity),activity= ifelse(sales>0,1,0))
# for 2010 customers define 
data2010 <- data2010 %>% left_join(act2011, by= "Customer ID") %>% mutate(active2011=ifelse(is.na(active2011),"0",active2011))

# first mathematical formulation, matrix with value = sales, months as columns
sales_matrix <- data2010 %>% select(-activity) %>% spread(month,sales, fill=0)

# second mathematical formulation, binary matrix with value = activity, months as columns
activity_matrix <- data2010 %>% select(-sales) %>% spread(month,activity, fill=0)

# third mathematical formulation usign RFM, recency-frequency-monetary
# recency = months to the last time with sales, 1 if sales in Dec 2010, 12 in last sales in Jan 2010, by customer
# freq = count of months with activity in 2010, by customer
# monetary = total 2010 sales, by customer
rfm_matrix <- data2010 %>% group_by(`Customer ID`) %>% summarize(active2011 = max(active2011), recency = 13-max(month), freq = sum(activity), monetary = sum(sales))

# partition into train and test sets, for the 3 mathematical models, sales, activity, rfm
set.seed(1984) # set orwellian seed
test_index <- createDataPartition( y= sales_matrix$active2011, times = 1, p = 0.2, list = FALSE)
test_sales <- sales_matrix[test_index,]
test_activity <- activity_matrix[test_index,]
test_rfm <- rfm_matrix[test_index,]
train_sales <- sales_matrix[-test_index,]
train_activity <- activity_matrix[-test_index,]
train_rfm <- rfm_matrix[-test_index,]
options(digits = 3) # print accuracy resuts with 3 digits

# prepare matrices to store accuracy results for train and test sets
train_results <- matrix(0, nrow = 3, ncol = 4)
colnames(train_results)<-c("knn","rf", "logistics", "naive_bayes")
rownames(train_results) <-c("sales", "activity","rfm")
test_results <- train_results


# sales model, knn method
set.seed(1492)
train_knn_sales <- train(active2011 ~ ., method = "knn", 
                   data = train_sales[,2:14],
                   tuneGrid = data.frame(k = seq(3, 60, 2)))
p1 <- ggplot(train_knn_sales, highlight = TRUE) + ggtitle( "Cross-validation Sales model, knn method")
train_results["sales","knn"] <- max(train_knn_sales$results[["Accuracy"]])
test_results ["sales","knn"] <- mean(predict(train_knn_sales, test_sales[,2:14]) == test_sales$active2011)

# activity model, knn method
set.seed(1492)
train_knn_activity <- train(active2011 ~ ., method = "knn", 
                         data = train_activity[,2:14],
                         tuneGrid = data.frame(k = seq(3, 30, 2)))
p2 <- ggplot(train_knn_activity, highlight = TRUE) + ggtitle( "Cross-validation Activity model, knn method")
train_results["activity","knn"] <- max(train_knn_activity$results[["Accuracy"]])
test_results ["activity","knn"] <- mean(predict(train_knn_activity, test_activity[,2:14]) == test_activity$active2011)

# rfm model, knn method
set.seed(1492)
train_knn_rfm <- train(active2011 ~ ., method = "knn", 
                            data = train_rfm[,2:5],
                            tuneGrid = data.frame(k = seq(3, 60, 2)))
p3 <- ggplot(train_knn_rfm, highlight = TRUE) + ggtitle( "Cross-validation RFM model, knn method")
train_results["rfm","knn"] <- max(train_knn_rfm$results[["Accuracy"]])
test_results ["rfm","knn"] <- mean(predict(train_knn_rfm, test_rfm[,2:5]) == test_rfm$active2011)

grid.arrange(p1, p2, p3, ncol=2) # plot crossvalidations for knn

# sales model, random forest method
set.seed(1492)
train_rf_sales <- train(active2011 ~ ., method = "rf", 
                         data = train_sales[,2:14])
p4 <- ggplot(train_rf_sales, highlight = TRUE) + ggtitle( "Cross-validation Sales model, random forest method")
train_results["sales","rf"] <- max(train_rf_sales$results[["Accuracy"]])
test_results ["sales","rf"] <- mean(predict(train_rf_sales, test_sales[,2:14]) == test_sales$active2011)

# activity model, random forest method
set.seed(1492)
train_rf_activity <- train(active2011 ~ ., method = "rf", 
                            data = train_activity[,2:14])
p5 <- ggplot(train_rf_activity, highlight = TRUE) + ggtitle( "Cross-validation Activity model, random forest method")
train_results["activity","rf"] <- max(train_rf_activity$results[["Accuracy"]])
test_results ["activity","rf"] <- mean(predict(train_rf_activity, test_activity[,2:14]) == test_activity$active2011)

# rfm model, random forest method
set.seed(1492)
train_rf_rfm <- train(active2011 ~ ., method = "rf", 
                       data = train_rfm[,2:5])
p6 <- ggplot(train_rf_rfm, highlight = TRUE) + ggtitle( "Cross-validation RFM model, random forest method")
train_results["rfm","rf"] <- max(train_rf_rfm$results[["Accuracy"]])
test_results ["rfm","rf"] <- mean(predict(train_rf_rfm, test_rfm[,2:5]) == test_rfm$active2011)

grid.arrange(p4, p5, p6, ncol=2) # plot crossvalidations for rf

# sales model, logistics method
train_glm_sales <- train(active2011 ~ ., method = "glm", 
                        data = train_sales[,2:14])
train_results["sales","logistics"] <- max(train_glm_sales$results[["Accuracy"]])
test_results ["sales","logistics"] <- mean(predict(train_glm_sales, test_sales[,2:14]) == test_sales$active2011)

# activity model, logistics method
train_glm_activity <- train(active2011 ~ ., method = "glm", 
                           data = train_activity[,2:14])
train_results["activity","logistics"] <- max(train_glm_activity$results[["Accuracy"]])
test_results ["activity","logistics"] <- mean(predict(train_glm_activity, test_activity[,2:14]) == test_activity$active2011)

# rfm model, logistics method
train_glm_rfm <- train(active2011 ~ ., method = "glm", 
                      data = train_rfm[,2:5])
train_results["rfm","logistics"] <- max(train_glm_rfm$results[["Accuracy"]])
test_results ["rfm","logistics"] <- mean(predict(train_glm_rfm, test_rfm[,2:5]) == test_rfm$active2011)

# sales model, naive bayes method
train_nb_sales <- train(active2011 ~ ., method = "naive_bayes", 
                         data = train_sales[,2:14])
train_results["sales","naive_bayes"] <- max(train_nb_sales$results[["Accuracy"]])
test_results ["sales","naive_bayes"] <- mean(predict(train_nb_sales, test_sales[,2:14]) == test_sales$active2011)

# activity model, naive bayes method
train_nb_activity <- train(active2011 ~ ., method = "naive_bayes", 
                            data = train_activity[,2:14])
train_results["activity","naive_bayes"] <- max(train_nb_activity$results[["Accuracy"]])
test_results ["activity","naive_bayes"] <- mean(predict(train_nb_activity, test_activity[,2:14]) == test_activity$active2011)

# rfm model, naive bayes method
train_nb_rfm <- train(active2011 ~ ., method = "naive_bayes", 
                       data = train_rfm[,2:5])
train_results["rfm","naive_bayes"] <- max(train_nb_rfm$results[["Accuracy"]])
test_results ["rfm","naive_bayes"] <- mean(predict(train_nb_rfm, test_rfm[,2:5]) == test_rfm$active2011)

rowMeans(train_results) # compare results by model

# make chart to compare methods
chart <- gather(data.frame(train_results), key = "method", value = "accuracy") # transform data from matrix to tidy
chart <- data.frame(model = c("sales", "activity","rfm"), chart) # add model names
chart1 <- data.frame(data = "train", chart) # add name of data set
chart <- gather(data.frame(test_results), key = "method", value = "accuracy") # transform data from matrix to tidy
chart <- data.frame(model = c("sales", "activity","rfm"), chart) # add model name
chart2 <- data.frame(data = "test", chart) # add name of data set
chart_all <- rbind(chart1,chart2) # put train and test resukts in one set with tidy format

chart_all %>% ggplot(aes(model,accuracy, color = method )) + geom_point(size = 5) +
  ggtitle("Accuracy by model, method as a color") + facet_grid(.~ data)



###############################################################
# run one-month ahead prediction models using linear regression
###############################################################

# set up matix with customers as rows, months as columns, Sales as value
mod_matrix <- data %>% 
  mutate(month = floor_date(InvoiceDate,"month")) %>% 
  group_by(month, `Customer ID`) %>% 
  summarize(Sales = sum(Price*Quantity)) %>% 
  spread(month, Sales, fill=0)  # use fill=0 to assing 0 instead of NA to a month when a customer has no sales

# define vector of names to label variables
names <- c(paste0("x",1:12),"y")

# because of sales seasonality, we use 12 months to train a model that predicts month 13th. 
# the model is applied on the data from the 2nd to the 13th month to predict the upcoming month, month 14th
# the process is repeated, shifting one month ahead
# Dec 2011 is not predicted, data is a partial month

n <- seq(2,12,1) 

####################################################
#the first model uses all 12 prior months to predict
####################################################

predictions <- sapply(n,function(n){
  mod <-mod_matrix[,n:(n+12)] %>% filter(rowSums(.)>0)  # take 13 columns from mod_matrix, keep rows for customers with actual sales in those 13 months
  names(mod) <- names # rename the 13 columns as x1, ..., x12, y
  fit_lm <- lm(formula = y ~ ., data = mod) # train linear regression model to predict y usig all 12 prior months
  mod <- mod_matrix[,(n+1):(n+13)] %>% filter(rowSums(.)>0) # build test set by shifting one column to the right, keep rows for customers with actual sales
  names(mod) <- names # rename the 13 columns for the new data set
  return(c(sum(mod$y),sum(predict(fit_lm,mod)))) # calculate actual sales and predicted sales
})

# create plots, assess rmse of predictions

hist(predictions[2,]/predictions[1,], breaks=seq(0,3,0.4), xlab="Prediction/Actual", main = "Histogram of prediction/actual", col="gray")
period <- as_date(names(mod_matrix)[15:25])
chart_data <- data.frame(period,actual =predictions[1,], prediction = predictions[2,])
chart_data %>% ggplot(aes(period, prediction/actual)) +geom_line() +geom_point()+
  geom_hline(yintercept=1, col = "red") +geom_hline(yintercept=1.2, linetype =2, col="red") +geom_hline(yintercept=0.8, linetype =2, col="red") +
  ggtitle("2018 Predictions using monthly model") + 
  geom_text(aes(x = chart_data$period[7], y = 1.25), label = paste0("RMSE = ",round(RMSE(chart_data$actual,chart_data$prediction)), " per month"))

####################################################################
#the second model uses the 3 months that have highest autocorrelation
# 1, 11 and 12 months prior to the month to be predicted
####################################################################

predictions <- sapply(n,function(n){
  mod <-mod_matrix[,n:(n+12)] %>% filter(rowSums(.)>0)  # take 13 columns from mod_matrix, keep rows for customers with actual sales in those 13 months
  names(mod) <- names # rename the 13 columns as x1, ..., x12, y
  fit_lm <- lm(formula = y ~ x1 + x2 + x12, data = mod) # train linear regression model to predict y using x1 (a year ago), x2 (11 months), x12 (prior month)
  mod <- mod_matrix[,(n+1):(n+13)] %>% filter(rowSums(.)>0) # build test set by shifting one column to the right, keep rows for customers with actual sales
  names(mod) <- names # rename the 13 columns for the new data set
  return(c(sum(mod$y),sum(predict(fit_lm,mod)))) # calculate actual sales and predicted sales
})

# create plots, assess rmse of predictions

hist(predictions[2,]/predictions[1,], breaks=seq(0,3,0.4), xlab="Prediction/Actual", main = "Histogram of prediction/actual", col="gray")
period <- as_date(names(mod_matrix)[15:25])
chart_data <- data.frame(period,actual =predictions[1,], prediction = predictions[2,])
chart_data %>% ggplot(aes(period, prediction/actual)) +geom_line() +geom_point()+
  geom_hline(yintercept=1, col = "red") +geom_hline(yintercept=1.2, linetype =2, col="red") +geom_hline(yintercept=0.8, linetype =2, col="red") +
  ggtitle("2018 Predictions using monthly model") + 
  geom_text(aes(x = chart_data$period[7], y = 1.25), label = paste0("RMSE = ",round(RMSE(chart_data$actual,chart_data$prediction)), " per month"))
