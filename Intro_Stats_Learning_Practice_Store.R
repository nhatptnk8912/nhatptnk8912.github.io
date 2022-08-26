#### This file is for an illustration of estimating function f in statistical learning
#### We use the "Realestate.csv" data for the illustration

### First thing to do is load the libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(MLmetrics)
library(FNN)

### Nex thing to do is to set up the working directory for R
### Guidance for how to set up working directory is in: http://www.sthda.com/english/wiki/running-rstudio-and-setting-up-your-working-directory-easy-r-programming

# Determine the current directory
getwd()

# Set the working directory (you need to choose different directory in your computer)
setwd("/Users/nh23294/Box/Teaching/SDS_323/Data/")

################################################################################
############## We import csv data
realestate = read.csv("Real_estate.csv", head = TRUE, check.names=FALSE)

############## Change the column names for easier implementation
realestate <- data.frame(realestate)
colnames(realestate) <- c("No","transact_date", "house_age", "dist_station", "number_stores", "latitude", "longitude", "price")
head(realestate, 6)
############## Create Train/ Test set

### 75% of the sample size
sample_size <- floor(0.75 * nrow(realestate))

## We create the seed to make our partition reproducible
set.seed(1000)
train_index <- sample(seq_len(nrow(realestate)), size = sample_size)
realestate_train <- realestate[train_index,]
realestate_test <- realestate[-train_index,]

## Create a label column for train and set data
label <- seq(1:nrow(realestate))
label[train_index] = 1
label[-train_index] = 0
realestate_new <- cbind(realestate, label)

################## Scatter plots for train and set data
ggplot(realestate_new, aes(x=number_stores, y=price, color = factor(label))) + geom_point() + theme(legend.title=element_blank()) + scale_colour_discrete(breaks=c(0, 1), labels=c("Test", "Train"))

################################################################################
############## Y = real estate price, X = number of stores

############## Parametric methods

## Linear and quadractic models on train data
ggplot(realestate_train, aes(x=number_stores, y=price)) + geom_point(size=2, color = "#F8766D", shape=19) + geom_smooth(method='lm', formula= y~x, se = FALSE, colour="red")

ggplot(realestate_train, aes(x=number_stores, y=price)) + geom_point(size=2, color = "#F8766D", shape=19) + geom_smooth(method='lm', formula= y~poly(x,2), se = FALSE, colour="red")

## Test Linear model on test data
price_test <- as.vector(realestate_test$price)
my_lm <- lm(price~number_stores, data=realestate_train) 
pred_val_test <- my_lm %>%  predict(realestate_test) #Create predictions of price based on test data
rmse_out <- RMSE(pred_val_test, price_test) # Compute the RMSE_out for test data

realestate_test$pred_val_test <- pred_val_test # Attach the predictions of price as new column of test data
ggplot(realestate_test, aes(x=number_stores, y=price)) + geom_point(size=2, color = "#00BFC4" , shape=19) + geom_line(aes(x = number_stores, y = pred_val_test), color = "red", size = 1.5)

## Test Quadratic model on test data
price_test <- as.vector(realestate_test$price)
my_lm <- lm(price~poly(number_stores, 3), data=realestate_train) 
pred_val_test <- my_lm %>%  predict(realestate_test) #Create predictions of price based on test data
rmse_out <- RMSE(pred_val_test, price_test) # Compute the RMSE_out for test data

realestate_test$pred_val_test <- pred_val_test # Attach the predictions of price as new column of test data
ggplot(realestate_test, aes(x=number_stores, y=price)) + geom_point(size=2, color = "#00BFC4" , shape=19) + geom_line(aes(x = number_stores, y = pred_val_test), color = "red", size = 1.5)


############# Nonparametric method: K-nearest neighbors
number_stores_train <- data.frame(realestate_train$number_stores)
number_stores_test <- data.frame(realestate_test$number_stores)
price_train <- data.frame(realestate_train$price)
price_train_vec <- as.vector(realestate_train$price)
price_test <- as.vector(realestate_test$price)

### RMSE for train and RMSE_out together
N = 150
K <- seq(3, N)
rmse_out_val <- seq(3, N)
rmse_val <- seq(3, N)
for (i in 1:(N-2))
{
  knnfit_train = knn.reg(train = realestate_train$number_stores, y = realestate_train$price, k = K[i])
  knnfit_test = knn.reg(number_stores_train, number_stores_test, price_train, k = K[i])
  rmse_val[i] = RMSE(knnfit_train$pred, price_train_vec)
  rmse_out_val[i] = RMSE(knnfit_test$pred, price_test)
}

### Determine best K of RMSE and RMSE_out
which.min(rmse_val)
which.min(rmse_out_val)

### Plot RMSE and RMSE_out
rmse_out_val <- data.frame(rmse_out_val)
rmse_val <- data.frame(rmse_val)
mydata <- data.frame(K, rmse_val, rmse_out_val)
newdata <- mydata %>% gather(key = "label", value = "combine_val", - K)
ggplot(newdata, aes(x = K, y = combine_val)) + geom_line(aes(color = label), size = 1.2) + scale_x_continuous(breaks = seq(3, N, 6)) + xlab('K_val') + ylab('RMSE') + scale_colour_discrete(breaks=c("rmse_out_val", "rmse_val"), labels=c("Test", "Train"))

### Plot K-nearest neighbors for best K = 13 of train and test data
# Training data
knnfit <- knn.reg(train = realestate_train$number_stores, y = realestate_train$price, k = 13)
ggplot(realestate_train, aes(x=number_stores, y=price)) + geom_point(size=2, color = "#F8766D", shape=19) + geom_line(aes(x = number_stores, y = knnfit$pred), color = "red")

# Test data
knnfit <- knn.reg(number_stores_train, number_stores_test, price_train, k = 13)
pred_val_test <- knnfit$pred
realestate_test$pred_val_test <- pred_val_test
ggplot(realestate_test, aes(x=number_stores, y=price)) + geom_point(size=2, color = "#00BFC4", shape=19) + geom_line(aes(x = number_stores, y = pred_val_test), color = "red")







