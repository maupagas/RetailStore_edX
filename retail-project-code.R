#####

# Load All Packages Required

if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(dummies)) install.packages("dummies", repos = "http://cran.us.r-project.org")
if(!require(FNN)) install.packages("FNN", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(ipred)) install.packages("ipred", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")

# Load initial libraries
library(ggplot2)
library(tidyverse)
library(tidyr)
library(lubridate)
library(Matrix)
library(caret)
library(scales)
library(dummies)

options(dplyr.summarise.inform = FALSE)

#####

## DATA LOADING AND PREPARATION

# Load csv data to explore it initially
# customer <- read.csv("source-data-kaggle/Customer.csv")
# product_info <- read.csv("source-data-kaggle/prod_cat_info.csv")
# transactions <- read.csv("source-data-kaggle/Transactions.csv")

customer <- read.csv("https://raw.githubusercontent.com/maupagas/RetailStore_edX/main/retail-store-files/Customer.csv")
transactions <- read.csv("https://raw.githubusercontent.com/maupagas/RetailStore_edX/main/retail-store-files/Transactions.csv")
product_info <- read.csv("https://raw.githubusercontent.com/maupagas/RetailStore_edX/main/retail-store-files/prod_cat_info.csv")

# 1. Data Exploration
summary(product_info)
summary(customer)
summary(transactions)

str(transactions)
head(transactions)

n_users <- n_distinct(transactions$cust_id)
n_transactions <- n_distinct(transactions$transaction_id)

# 2. Data preparation

# Convert time factors into date
date_1 <- as.Date(transactions$tran_date, "%d-%m-%Y")
date_2 <- as.Date(transactions$tran_date, "%d/%m/%Y")
transactions$tran_date <- ifelse(is.na(date_1), date_2, date_1)

# Fix the date into appropriate format
transactions$tran_date <- as.Date(transactions$tran_date, origin = "1970-01-01")


# Convert categories as factors in both variables
transactions$prod_subcat_code <- as.factor(transactions$prod_subcat_code)
transactions$prod_cat_code <- as.factor(transactions$prod_cat_code)

# Convert product into factors 
product_info$prod_sub_cat_code <- as.factor(product_info$prod_sub_cat_code) 
product_info$prod_cat_code <- as.factor(product_info$prod_cat_code) 


# Merge data 

# First, have the same column name for both variables
colnames(product_info)[3] <- "prod_subcat_code"
colnames(customer)[1] <- "cust_id"

# Once columns have the same name, we can join the data
transactions.new <- transactions %>% 
                            left_join(product_info, 
                                      by = c('prod_subcat_code',
                                             'prod_cat_code')) %>% 
                            left_join(customer, by = 'cust_id') %>% 
                            unite("prod_category", 
                                  c(prod_cat, prod_subcat),
                                  sep = " ", 
                                  remove = F) # Keep the original variables

# Obtain the age of the customer
transactions.new$cust_age <- year(transactions.new$tran_date) - 
                             year(as.Date(transactions.new$DOB, "%d-%m-%Y"))

# Fix missing values and NA's 

## Fix the Genders 
summary(transactions.new$Gender)
nonGender <- which(transactions.new$Gender == "" )

# Replace by sampling randomly according to the proportion in the sample
transactions.new$Gender[nonGender] <- sample(transactions.new$Gender, length(nonGender), replace = T)
transactions.new$Gender <- as.factor(transactions.new$Gender)

## Avoid having NA's in the variable city_code
nonCity <- which(is.na(transactions.new$city_code))
nonCity

transactions.new$city_code[nonCity] <- sample(transactions.new$city_code, length(nonCity), replace = T)


# Finally, we reorder the data in the columns we want
transactions.ord <- transactions.new[ , c("cust_id", "DOB", "cust_age", "Gender", "city_code",
                                          "transaction_id", "tran_date", "prod_subcat_code",  
                                          "prod_cat_code","prod_category", "prod_cat", "prod_subcat",
                                          "Store_type", "Qty", "Rate", "Tax", "total_amt")]


## Aggregate also data by customers
cust.desc <- transactions.ord %>% filter(total_amt > 0) %>% 
  group_by(cust_id) %>%
  summarise(n = n(), 
            age = mean(cust_age), 
            city_code = mean(city_code), 
            Gender = Gender, 
            amount = sum(total_amt)) %>% 
            unique()


#####

## DATA EXPLORATION

# Gender distribution
transactions.ord %>% filter(total_amt>0) %>% 
  group_by(cust_age, Gender) %>% summarise(n = n()) %>% 
  ggplot(aes(cust_age, n, fill = Gender)) + 
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  scale_y_continuous(breaks = seq(0, 600, 100), labels = number) +
  xlab("Customer Age") + ylab("Number of customers")

# Distribution plot of ALL transactions
transactions %>% ggplot(aes(total_amt)) +
                        geom_histogram(bins = 20, color = "black", fill = "steelblue") + 
                        xlab("Amount paid") + 
                        ylab("Number of transactions")

# Distribution plot of only sales
transactions %>% filter(total_amt > 0) %>%
                 ggplot(aes(total_amt)) + 
                 geom_histogram(bins = 20, color = "black", fill = "steelblue") + 
                 xlab("Amount paid") + 
                 ylab("Number of transactions") + 
                 geom_density()

## We can do the transactions over time per retail store

# Plot per month
transactions.ord %>% mutate(year = year(tran_date), month = month(tran_date)) %>% 
                     ggplot(aes(as.factor(month), total_amt, fill = Store_type)) + 
                            geom_boxplot() + 
                            xlab("Month") + ylab("Total Amount")

# Plot per year
transactions.ord %>% filter(total_amt > 0) %>% 
                     mutate(year = year(tran_date), month = month(tran_date)) %>% 
                     ggplot(aes(as.factor(year), total_amt, fill = prod_cat_code)) + 
                     geom_boxplot() + 
                     xlab("Year") + ylab("Total Amount")

# See the amount of money spent per category per year 
transactions.ord %>% filter(total_amt > 0) %>% 
                     mutate(year = year(tran_date), month = month(tran_date)) %>%
                     group_by(prod_cat, year) %>% 
                     summarise(n = n(), total_amount = sum(total_amt)) %>% 
                     ggplot(aes(x = year, y =  total_amount,  fill = prod_cat)) + 
                     geom_bar(stat="identity", color = "black", position = "dodge") + 
                     ylab("Total Amount Spent") + xlab("Year") +
                     scale_y_continuous(labels = number)

# See the amount of money spent per category per month 
transactions.ord %>% filter(total_amt > 0) %>% 
                     mutate(year = year(tran_date), month = month(tran_date)) %>%
                     group_by(prod_cat, month) %>% 
                     summarise(n = n(), total_amount = sum(total_amt)) %>% 
                     ggplot(aes(x = month, y =  total_amount,  fill = prod_cat)) + 
                     geom_bar(stat="identity", color = "black", position = "dodge") +
                     ylab("Total Amount Spent") + xlab("Month") +
                     scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar",
                                                                  "Apr", "May", "Jun",
                                                                  "Jul", "Aug", "Sep", 
                                                                  "Oct", "Nov", "Dic")) +
                     scale_y_continuous(breaks = seq(0, 1.5e6, 2.5e5), labels = number)


# Plot of number of items sold over time
transactions %>% group_by(month = floor_date(tran_date, unit = "month")) %>% 
                 summarise(n = n()) %>% 
                 ggplot(aes(month, n)) + geom_point() + geom_smooth()

# Plot of average amount received per each month
transactions %>% group_by(month = floor_date(tran_date, unit = "month")) %>% 
                 summarise(n = n(), amnt = mean(total_amt)) %>% 
                 ggplot(aes(month, amnt)) + geom_point() + geom_smooth()


# Plot of average amount received per each month
transactions.ord %>% filter(total_amt>0) %>% 
                   # mutate(year = year(tran_date), month = month(tran_date)) %>%
                     group_by(cust_age, prod_cat) %>% summarise(n = n(), total_amount = sum(total_amt)) %>% 
                     ggplot(aes(x = cust_age, y =  total_amount,  fill = prod_cat)) + 
                            geom_bar(stat="identity", color = "black") + 
                            ylab("Total Amount Spent") + xlab("Customer Age") +
                            scale_y_continuous(labels = number) + 
                            scale_fill_discrete(name = "Product Category")

# Alternative plot by lines
transactions.ord %>% filter(total_amt>0) %>% 
                    # mutate(year = year(tran_date), month = month(tran_date)) %>%
                     group_by(cust_age, prod_cat) %>% summarise(n = n(), total_amount = sum(total_amt)) %>% 
                     ggplot(aes(x = cust_age, y =  total_amount,  color = prod_cat)) + 
                     geom_point() + geom_line() +
                     ylab("Total Amount Spent") + xlab("Customer Age") +
                     scale_y_continuous(labels = number) + 
                     scale_color_discrete(name = "Product Category")


# Check if users are recurrent or not 
summary.transactions <- transactions.ord %>% filter(total_amt>0) %>% 
                                             group_by(cust_id) %>% 
                                             summarise(n = n(), amount  = sum(total_amt))

avg_transaction <- mean(summary.transactions$amount)
avg_n_usr <- mean(summary.transactions$n)

#Plot how many times a user repeats in the store
summary.transactions %>% ggplot(aes(n)) + geom_histogram(binwidth = 1, fill = "steelblue", color = "black") + 
                         scale_x_continuous(breaks = 1:11) +
                         geom_vline(xintercept = avg_n_usr,  col = "maroon", lty = 2, lwd = 1) + 
                         xlab("Number of times a user repeats") + 
                         ylab("Number of users")

#Plot the amount of money a user spends in the store
summary.transactions %>% ggplot(aes(amount)) + 
                                geom_histogram(bins = 30, fill = "steelblue", color = "black") +
                                # facet_wrap(~Gender) +  
                                # scale_x_continuous(breaks = 1:11) +
                                geom_vline(xintercept = avg_transaction,  col = "maroon", lty = 2, lwd = 1) + 
                                xlab("Amount spent by user") + 
                                ylab("Number of users") 

#####

# MODEL DEVELOPMENT

#####
#Prepare rmse_results
rmse_results <- tibble ()

## METHOD 1: K-Nearest Neighbors
library(FNN)

# Prepare dummy variables for the k-nearest-neighbors
dms1 <- dummy("Gender", as.data.frame(cust.desc), sep = "_")
dms2 <- dummy("city_code", as.data.frame(cust.desc), sep = "_")
dms2 <- dms2[,-11]

#Explore that the number of the variables is the correct one
head(dms2)

# Create dummy variables needed for the k-nearest neighbors
cust.desc.dummies <- cbind(as.data.frame(cust.desc), dms1, dms2)
head(cust.desc.dummies)

#Calculate the average amount spent per customer
avg_amount <- mean(cust.desc.dummies$amount)

# Scale the data for the K-Nearest Neighbors algorithm
cust.desc.dummies$age.s <- rescale(cust.desc.dummies$age)
cust.desc.dummies$n.s   <- rescale(cust.desc.dummies$n)
# view(cust.desc.dummies)

# We try now k-nearest neighbors by developing a train set with 60 % 
# a validation set of 20 and a test set of 20 %
tr.id <- createDataPartition(cust.desc.dummies$amount, p = 0.6, list = F)
tr <- cust.desc.dummies[tr.id, ]
temp <- cust.desc.dummies[-tr.id, ]

v.id <- createDataPartition(temp$amount, p = 0.5, list = F)
val  <- temp[v.id, ]
test <- temp[-v.id, ]

# Function to calculate the RMSE with a given K
func.knn.reg <- function(tr_predictor, val_predictor, 
                          tr_target, val_target, k){
  library(FNN)
  res <- knn.reg(tr_predictor, val_predictor, 
                 tr_target, k, algorithm = "brute")
  
  rmserror <- RMSE(res$pred, val_target)
  cat(paste("RMSE for k = ", toString(k), ": ", rmserror, "\n", sep = ""))
  rmserror
}

# Function to calculate the RMSE with a given range of k's
func.knn.reg.multi <- function(tr_predictor, val_predictor, 
                                tr_target, val_target, 
                                start_k, end_k){
  rms_errors <- vector()
  for (k in start_k:end_k){
    rms_error <- func.knn.reg(tr_predictor, val_predictor, 
                               tr_target, val_target, k)
    rms_errors <- c(rms_errors, rms_error)
  }
  # Plot the error
  plot(rms_errors, type ='o', xlab = "k", ylab = "RMSE")
  return(rms_errors)
}

# Run the k-nearest neighbors for a big range
rmse.knn.gencity.val <- func.knn.reg.multi(tr[,7:20], val[,7:20], 
                                        tr$amount, val$amount, 1, 30)

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "K-Nearest Neighbors - City and Gender Predictors
                                 (Validation Set)",
                                 RMSE = min(rmse.knn.gencity.val)))

# Run the k-nearest neighbors for a big range - for the test set
rmse.knn.gencity.test <- func.knn.reg.multi(tr[,7:20], val[,7:20], 
                                        tr$amount, val$amount, 1, 30)

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "K-Nearest Neighbors - City and Gender Predictors
                                 (Test Set)",
                                 RMSE = min(rmse.knn.gencity.test)))

# Try to perform k-nearest neighbors adding the category and the year of the product
transactions.grouped <- transactions.ord %>% filter(total_amt > 0) %>% 
       group_by(cust_id) %>% select(cust_id, tran_date, prod_cat, total_amt) 

# Create dummies for the product category
dummy.transactions <- dummy.data.frame(as.data.frame(transactions.grouped), 
                                       names = "prod_cat", sep = " ")
colnames(dummy.transactions) <- str_remove(colnames(dummy.transactions), 
                                           "prod_cat ")
colnames(dummy.transactions) <- str_replace_all(colnames(dummy.transactions), "-", ".")
colnames(dummy.transactions) <- str_replace_all(colnames(dummy.transactions), " ", ".")

head(dummy.transactions)

# Group the dummy variables created by customer id
dummy.trans.grouped <- dummy.transactions %>%  group_by(cust_id) %>%      
                             summarise(n = n(), bags = sum(Bags), 
                                       Books = sum(Books), Clothing = sum(Clothing),
                                       Electronics = sum(Electronics),
                                       Footwear = sum(Footwear), 
                                       Home = sum(Home.and.kitchen))


# Add to the customer description data frame the dummy variables for the product category
final.cust <- cust.desc %>% left_join(dummy.trans.grouped, by = c("cust_id", "n"))
head(final.cust)

## Let's prepare the data again to apply K-nearest neighbors
dms3 <- dummy("Gender", as.data.frame(cust.desc), sep = "_")
dms4 <- dummy("city_code", as.data.frame(cust.desc), sep = "_")
dms4 <- dms4[,-11]

# Merge new dummy variables 
final.cust.dms <- cbind(as.data.frame(final.cust), dms3, dms4)

# Scale from 0 to 1 the other numerical variables
final.cust.dms$age.s <- rescale(final.cust.dms$age)
final.cust.dms$n.s   <- rescale(final.cust.dms$n)

# We try now k-nearest neighbors again
tr.id <- createDataPartition(final.cust.dms$amount, p = 0.6, list = F)
tr <- final.cust.dms[tr.id, ]
temp <- final.cust.dms[-tr.id, ]

v.id <- createDataPartition(temp$amount, p = 0.5, list = F)
val  <- temp[v.id, ]
test <- temp[-v.id, ]

# Try the K-Nearest Neighbors now
rmse.knn.prod.val <- func.knn.reg.multi(tr[,c(7:12)], val[,c(7:12)], 
                                        tr$amount, val$amount, 1, 30)

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "K-Nearest Neighbors - Product as Predictors
                                 (Validation Set)",
                                 RMSE = min(rmse.knn.prod.val)))

rmse.knn.prod.test <- func.knn.reg.multi(tr[,c(7:12)], test[,c(7:12)], 
                                        tr$amount, test$amount, 1, 30)

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "K-Nearest Neighbors - Product as Predictors 
                                 (Test Set)",
                                 RMSE = min(rmse.knn.prod.test)))

#Looks like our K-nearest neighbors works better when only considering the type of product

#####

## METHOD 2: Linear Regression

# Create a train and a test set
tr.id <- createDataPartition(final.cust$amount, p = 0.7, list = F)
# Ignore the first column because it is of no use and 
# the second one because it is captured in the dummy variables
mod_lm <- lm(amount ~., data = final.cust[tr.id,-c(1,2)]) 

#Explore the obtained linear model
summary(mod_lm)

# Calculate the RMSE of the linear model
rmse_linear <- sqrt(mean(as.matrix(mod_lm$fitted.values - final.cust[-tr.id, "amount"])^2))
rmse_linear

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Linear Model (Test Set)",
                                 RMSE = rmse_linear))

# The RMSE of the linear model is much larger than the k-nearest neighbors

#####

## METHOD 3: Random Trees Regression

# Explore the data again
head(final.cust)

#Create a training set 
tr.id <- createDataPartition(final.cust$amount, p = 0.7, list = F)

# Fit the model with random trees
store.fit <- rpart(amount ~., data = final.cust[tr.id,-c(1,2)])
store.fit

# The representation gives 10 trees as the adequate number
plotcp(store.fit)

# Plot the random trees fit with the rpart plot
prp(store.fit, type = 2, nn = T,
    fallen.leaves = T, faclen = 4,
    varlen = 8, shadow.col ="gray")

# See the cp table to get the first cp value below the cut-off point
store.fit$cptable

# Use the model of random trees with less branches for the new prediction
# Select tree below that value (10 in this case)
store.fitpruned <- prune(store.fit, cp = 0.01139806)

# Plot the new random trees pruned
prp(store.fitpruned, type = 2, nn = T,
    fallen.leaves = T, faclen = 4,
    varlen = 8, shadow.col ="gray")

# Obtain the predictions of the original random tree model
preds <- predict(store.fit, final.cust[-tr.id,-c(1,2)])
rmse.trees <- sqrt(mean(as.matrix(preds - final.cust[-tr.id, "amount"])^2))
rmse.trees

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Random Trees  (Test Set)",
                                 RMSE = rmse.trees))

# Obtain the predictions of the pruned random tree model
preds <- predict(store.fitpruned, final.cust[-tr.id,-c(1,2)])
rmse.trees.pruned <- sqrt(mean(as.matrix(preds - final.cust[-tr.id, "amount"])^2))
rmse.trees.pruned

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Random Trees Pruned (Test Set)",
                                 RMSE = rmse.trees.pruned))

# The random trees seem to yield worse results than K-Nearest Neighbors

# Let's use some techniques of bagging 
library(ipred)

# Fit the model with random trees with bagging
bagging.fit <- bagging(amount ~., data = final.cust[tr.id,-c(1,2)])
prediction.t <- predict(bagging.fit,  final.cust[-tr.id,-c(1,2)])

rmse.bagging <- sqrt(mean(as.matrix(prediction.t - final.cust[-tr.id, "amount"])^2))
rmse.bagging

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Random Trees - Bagging (Test Set)",
                                 RMSE = rmse.bagging))
#With Bagging, the RMSE becomes smaller

# Let's use some techniques of boosting
library(gbm)

# Fit the model with random trees with boosting
gbm.fit <- gbm(amount ~., data = final.cust[tr.id,-c(1,2)], distribution = "gaussian")

# Obtain the prediction values over the test set
prediction.gbm <- predict(gbm.fit,  final.cust[-tr.id,-c(1,2)])
#Get the rmse on the test set
rmse.gbm <- sqrt(mean(as.matrix(prediction.gbm - final.cust[-tr.id, "amount"])^2))
rmse.gbm

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Random Trees - Boosting (Test Set)",
                                 RMSE = rmse.gbm))


# With Boosting, it becomes even better the RMSE.....

#####

## METHOD 4: Random Forest
library(randomForest)

# The final.cust data frame does not have enough variables, therefore 
# we will use the original cust.desc.dummies instead
mod.rf <- randomForest(x = cust.desc.dummies[tr.id,-c(1,2,6)], y = cust.desc.dummies[tr.id, 6],
                    ntree = 1000,
                    xtest = cust.desc.dummies[-tr.id,-c(1,2,6)],
                    ytest = cust.desc.dummies[-tr.id, "amount"],
                    importance = T, keep.forest = T)


# Obtain the prediction values over the test set
prediction.rf <- predict(mod.rf, cust.desc.dummies[-tr.id,-c(1,2,6)])
prediction.rf

#Get the RMSE on the test set
rmse.rf <- sqrt(mean(as.matrix(prediction.rf - cust.desc.dummies[-tr.id, "amount"])^2))
rmse.rf

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Random Forest (Test Set)",
                                 RMSE = rmse.rf))

# It is slightly better, than the boosting technique


#####

## METHOD 5: Neural Networks 
library(nnet)
library(devtools)

# Get the maximum amount value to scale the neural network
max_amount <- max(final.cust$amount)

# Perform the fitting of the neural network
fit_nn <-nnet(amount/max_amount ~., data = final.cust[tr.id, -c(1,2)],
              size = 6, decay = 0.1,
              maxit = 1000, linout = T)

# Download source package to plot neural network
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/f30f338ecf143c089af1b6a731edcabd0b11a79d/nnet_plot_update.r")

#Plot the neural network
plot(fit_nn, max.sp = T)
 
# Compute RMSE over the training set
rmse.nn.tr <- sqrt(mean(as.matrix(fit_nn$fitted.values*max_amount - final.cust[tr.id, "amount"])^2))
rmse.nn.tr

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Neural Network (Train Set)",
                                 RMSE = rmse.nn.test))

# Obtain the predictions over the test set
pred <- predict(fit_nn, final.cust[-tr.id,-c(1,2)])

# Compute RMSE over the test set
rmse.nn.test <- sqrt(mean(as.matrix(pred*max_amount - final.cust[-tr.id, "amount"])^2, na.rm = T))
rmse.nn.test

# Collect RMSE results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Neural Network (Test Set)",
                                 RMSE = rmse.nn.test))

# The result is better than any previous method we already saw...