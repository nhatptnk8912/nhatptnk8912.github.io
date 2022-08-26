#### This file is for an illustration of estimating function f in statistical learning
#### We use the "Realestate.csv" data for the illustration

### First thing to do is load the libraries
library(ggplot2)
library(tidyr)
library(dplyr)

### Nex thing to do is to set up the working directory for R
### Guidance for how to set up working directory is in: http://www.sthda.com/english/wiki/running-rstudio-and-setting-up-your-working-directory-easy-r-programming

# Determine the current directory
getwd()

# Set the working directory (you need to choose different directory in your computer)
setwd("/Users/nh23294/Box/Teaching/SDS_323/Data/")

################################################################################
############## We first try with the "Real_estate.csv"
realestate = read.csv("Real_estate.csv", head = TRUE, check.names=FALSE)

############## Change the column names for easier implementation
realestate <- data.frame(realestate)
colnames(realestate) <- c("No","transact_date", "house_age", "dist_station", "number_stores", "latitude", "longitude", "price")
head(realestate, 6)

############# Regression of house price based on house age
### First we create scatter plots for house price and house age, distance to station, and number of stores
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) 
ggplot(realestate, aes(x=dist_station, y=price)) + geom_point(size=2, color = "blue", shape=19)
ggplot(realestate, aes(x=number_stores, y=price)) + geom_point(size=2, color = "blue", shape=19)

############################ House age versus price
### Linear regression fit
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_smooth(method='lm', formula= y~x, se = FALSE, colour="red")

### Quadratic regression fit
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_smooth(method='lm', formula= y~poly(x,2), se = FALSE, colour="red")

ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_smooth(method='lm', formula= y~poly(x,3), se = FALSE, colour="red")
############################ Distance to station versus price
### Linear regression fit
ggplot(realestate, aes(x=dist_station, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_smooth(method='lm', formula= y~x, se = FALSE, colour="red")

### Higher order polynomial
ggplot(realestate, aes(x=dist_station, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_smooth(method='lm', formula= y~poly(x,2), se = FALSE, colour="red")

############################ Number of Stores versus price
### Linear regression fit
ggplot(realestate, aes(x=number_stores, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_smooth(method='lm', formula= y~x, se = FALSE, colour="red")

### Higher order polynomials
ggplot(realestate, aes(x=number_stores, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_smooth(method='lm', formula= y~poly(x,5), se = FALSE, colour="red")

###################################### Noparametric method: K-nearest neighbor regression
install.packages('FNN') 
library(FNN)

##### With Y = real estate price, X = house age
#Train with large k = 50
knnfit <- knn.reg(train = realestate$house_age, y = realestate$price, k = 50)
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(aes(x = house_age, y = knnfit$pred), color = "red")

#Train with medium k = 10
knnfit <- knn.reg(train = realestate$house_age, y = realestate$price, k = 10)
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(aes(x = house_age, y = knnfit$pred), color = "red")

#Train with small k = 2
knnfit <- knn.reg(train = realestate$house_age, y = realestate$price, k = 2)
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(aes(x = house_age, y = knnfit$pred), color = "red")

##### With Y = real estate price, X = distance to station
#Train with large k
knnfit <- knn.reg(train = realestate$dist_station, y = realestate$price, k = 50)
ggplot(realestate, aes(x=dist_station, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(aes(x = dist_station, y = knnfit$pred), color = "red")

#Train with medium k
knnfit <- knn.reg(train = realestate$dist_station, y = realestate$price, k = 10)
ggplot(realestate, aes(x=dist_station, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(aes(x = dist_station, y = knnfit$pred), color = "red")

#Train with small k
knnfit <- knn.reg(train = realestate$dist_station, y = realestate$price, k = 2)
ggplot(realestate, aes(x=dist_station, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(aes(x = dist_station, y = knnfit$pred), color = "red")

##### With Y = real estate price, X = number of stores
#Train with large k
knnfit <- knn.reg(train = realestate$number_stores, y = realestate$price, k = 50)
ggplot(realestate, aes(x=number_stores, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(aes(x = number_stores, y = knnfit$pred), color = "red")

#Train with medium k
knnfit <- knn.reg(train = realestate$number_stores, y = realestate$price, k = 10)
ggplot(realestate, aes(x=number_stores, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(aes(x = number_stores, y = knnfit$pred), color = "red")

#Train with small k
knnfit <- knn.reg(train = realestate$number_stores, y = realestate$price, k = 2)
ggplot(realestate, aes(x=number_stores, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(aes(x = number_stores, y = knnfit$pred), color = "red")



####################### Nonparametric methods: kernel regression and bias-variance tradeoff

######## Y = house price, X = house age
### First we try with large bandwidth = 2.5 
ksmooth.fit <-  ksmooth(realestate$house_age, realestate$price, bandwidth = 2.5, kernel = "normal")
ksmooth.fit <- data.frame(ksmooth.fit)
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(data = ksmooth.fit, aes(x = x, y = y), color = "red")

### Now we try with medium bandwidth = 1
ksmooth.fit <-  ksmooth(realestate$house_age, realestate$price, bandwidth = 1, kernel = "normal")
ksmooth.fit <- data.frame(ksmooth.fit)
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(data = ksmooth.fit, aes(x = x, y = y), color = "red")

### Now we try with medium bandwidth = 0.1
ksmooth.fit <-  ksmooth(realestate$house_age, realestate$price, bandwidth = 0.1, kernel = "normal")
ksmooth.fit <- data.frame(ksmooth.fit)
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(data = ksmooth.fit, aes(x = x, y = y), color = "red")

######## Y = house price, X = distance to station
### First we try with large bandwidth = 400
ksmooth.fit <-  ksmooth(realestate$dist_station, realestate$price, bandwidth = 400, kernel = "normal")
ksmooth.fit <- data.frame(ksmooth.fit)
ggplot(realestate, aes(x=dist_station, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(data = ksmooth.fit, aes(x = x, y = y), color = "red")

### Now we try with medium bandwidth = 100
ksmooth.fit <-  ksmooth(realestate$dist_station, realestate$price, bandwidth = 100, kernel = "normal")
ksmooth.fit <- data.frame(ksmooth.fit)
ggplot(realestate, aes(x=dist_station, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(data = ksmooth.fit, aes(x = x, y = y), color = "red")

### Now we try with medium bandwidth = 20
ksmooth.fit <-  ksmooth(realestate$dist_station, realestate$price, bandwidth = 20, kernel = "normal")
ksmooth.fit <- data.frame(ksmooth.fit)
ggplot(realestate, aes(x=dist_station, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(data = ksmooth.fit, aes(x = x, y = y), color = "red")

######## Y = house price, X = number of stores
### First we try with large bandwidth = 1.5
ksmooth.fit <-  ksmooth(realestate$number_stores, realestate$price, bandwidth = 1.5, kernel = "normal")
ksmooth.fit <- data.frame(ksmooth.fit)
ggplot(realestate, aes(x=number_stores, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(data = ksmooth.fit, aes(x = x, y = y), color = "red")

### Now we try with large bandwidth = 0.5
ksmooth.fit <-  ksmooth(realestate$number_stores, realestate$price, bandwidth = 0.5, kernel = "normal")
ksmooth.fit <- data.frame(ksmooth.fit)
ggplot(realestate, aes(x=number_stores, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(data = ksmooth.fit, aes(x = x, y = y), color = "red")

### Now we try with large bandwidth = 0.3
ksmooth.fit <-  ksmooth(realestate$number_stores, realestate$price, bandwidth = 0.3, kernel = "normal")
ksmooth.fit <- data.frame(ksmooth.fit)
ggplot(realestate, aes(x=number_stores, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(data = ksmooth.fit, aes(x = x, y = y), color = "red")

#### Nonparametric methods: spline 
install.packages('ggformula')
library(ggformula)
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_spline(aes(x = house_age, y = price), color = "red", nknots = 40) ### Take more look at geom_spline
ggplot(realestate, aes(x=house_age, y=price)) + geom_point() + geom_spline(aes(x = house_age, y = price), color = "red", nknots = 30)

############################ Assessing model accuracy

install.packages('MLmetrics')
library(MLmetrics)

### Create vector from of price
price <- as.vector(realestate$price)

### Parametric methods: linear regression
my_lm <- lm(price~house_age, data=realestate) 
pred_val <- my_lm %>% predict()
pred_val <- as.vector(pred_val)
RMSE(pred_val, price)

### Parametric methods: quadratic regression
my_lm <- lm(price~ poly(house_age, 2), data=realestate) 
pred_val <- my_lm %>% predict()
pred_val <- as.vector(pred_val)
RMSE(pred_val, price)

### Nonparametric method: K-nearest neighbor
N = 150
K <- seq(1,N)
rmse_val <- seq(1,N)
for (i in 1:N)
{
  knnfit = knn.reg(train = realestate$house_age, y = realestate$price, k = K[i])
  rmse_val[i] = RMSE(knnfit$pred, price)
}

rmse_val <- data.frame(rmse_val)
ggplot(rmse_val, aes(x =seq(1, N), y = rmse_val)) + geom_line(color = "red", size = 1.2) + scale_x_continuous(breaks = seq(1, N, 6)) + xlab('K_val')

### Plot K-nearest neighbor when K = 12
knnfit <- knn.reg(train = realestate$house_age, y = realestate$price, k = 12)
ggplot(realestate, aes(x=house_age, y=price)) + geom_point(size=2, color = "blue", shape=19) + geom_line(aes(x = house_age, y = knnfit$pred), color = "red")

######################### Train/ Test set

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
cbind(realestate, label)

## Scatter plots for train and set data
scatter <- ggplot(realestate, aes(x=house_age, y=price, color = factor(label))) + geom_point() + theme(legend.title=element_blank()) + scale_colour_discrete(breaks=c(1, 0), labels=c("Train", "Test"))

## Train linear and quadractic models on train data
ggplot(realestate_train, aes(x=house_age, y=price)) + geom_point(size=2, color = "#F8766D", shape=19) + geom_smooth(method='lm', formula= y~x, se = FALSE, colour="red")

ggplot(realestate_train, aes(x=house_age, y=price)) + geom_point(size=2, color = "#F8766D", shape=19) + geom_smooth(method='lm', formula= y~poly(x,2), se = FALSE, colour="red")

## Linear regression
price_test <- as.vector(realestate_test$price)
my_lm <- lm(price~house_age, data=realestate_train) 
pred_val_test <- my_lm %>%  predict(realestate_test)
rmse_out <- RMSE(pred_val_test, price_test)

realestate_test$pred_val_test <- pred_val_test
ggplot(realestate_test, aes(x=house_age, y=price)) + geom_point(size=2, color = "#00BFC4" , shape=19) + geom_line(aes(x = house_age, y = pred_val_test), color = "red", size = 1.5)


## Quadratic regression
price_test <- as.vector(realestate_test$price)
my_lm <- lm(price~poly(house_age, 2), data=realestate_train) 
pred_val_test <- my_lm %>%  predict(realestate_test)
rmse_out <- RMSE(pred_val_test, price_test)

realestate_test$pred_val_test <- pred_val_test
ggplot(realestate_test, aes(x=house_age, y=price)) + geom_point(size=2, color = "#00BFC4" , shape=19) + geom_line(aes(x = house_age, y = pred_val_test), color = "red", size = 1.5)

######### K-nearest neighbor

## Fitting training data
knnfit <- knn.reg(train = realestate_train$house_age, y = realestate_train$price, k = 50)
ggplot(realestate_train, aes(x=house_age, y=price)) + geom_point(size=2, color = "#F8766D", shape=19) + geom_line(aes(x = house_age, y = knnfit$pred), color = "red")

knnfit <- knn.reg(train = realestate_train$house_age, y = realestate_train$price, k = 10)
ggplot(realestate_train, aes(x=house_age, y=price)) + geom_point(size=2, color = "#F8766D", shape=19) + geom_line(aes(x = house_age, y = knnfit$pred), color = "red")

knnfit <- knn.reg(train = realestate_train$house_age, y = realestate_train$price, k = 3)
ggplot(realestate_train, aes(x=house_age, y=price)) + geom_point(size=2, color = "#F8766D", shape=19) + geom_line(aes(x = house_age, y = knnfit$pred), color = "red")

## Using the fits to test data
age_train <- data.frame(realestate_train$house_age)
age_test <- data.frame(realestate_test$house_age)
price_train <- data.frame(realestate_train$price)

# With K = 50
knnfit <- knn.reg(age_train, age_test, price_train, k = 50)
pred_val_test <- knnfit$pred
realestate_test$pred_val_test <- pred_val_test
ggplot(realestate_test, aes(x=house_age, y=price)) + geom_point(size=2, color = "#00BFC4", shape=19) + geom_line(aes(x = house_age, y = pred_val_test), color = "red")
rmse_out <- RMSE(pred_val_test, price_test)

# With K = 10
knnfit <- knn.reg(age_train, age_test, price_train, k = 10)
pred_val_test <- knnfit$pred
realestate_test$pred_val_test <- pred_val_test
ggplot(realestate_test, aes(x=house_age, y=price)) + geom_point(size=2, color = "#00BFC4", shape=19) + geom_line(aes(x = house_age, y = pred_val_test), color = "red")
rmse_out <- RMSE(pred_val_test, price_test)

# With K = 3
knnfit <- knn.reg(age_train, age_test, price_train, k = 1)
pred_val_test <- knnfit$pred
realestate_test$pred_val_test <- pred_val_test
ggplot(realestate_test, aes(x=house_age, y=price)) + geom_point(size=2, color = "#00BFC4", shape=19) + geom_line(aes(x = house_age, y = pred_val_test), color = "red")
rmse_out <- RMSE(pred_val_test, price_test)

# Plot RMSE_out for various values of K
N = 150
K <- seq(3, N)
rmse_out_val <- seq(3, N)
for (i in 1:(N-2))
{
  knnfit = knn.reg(age_train, age_test, price_train, k = K[i])
  rmse_out_val[i] = RMSE(knnfit$pred, price_test)
}

rmse_out_val <- data.frame(rmse_out_val)
ggplot(rmse_out_val, aes(x =seq(3,N), y = rmse_out_val)) + geom_line(color = "red", size = 1.2) + scale_x_continuous(breaks = seq(3, N, 6)) + xlab('K_val') 

### Plot both RMSE for train and RMSE_out together
N = 150
K <- seq(3, N)
rmse_val <- seq(3, N)
for (i in 1:(N-2))
{
  knnfit = knn.reg(train = age_train, y = price_train, k = K[i])
  rmse_val[i] = RMSE(knnfit$pred, price_train)
} 

rmse_val <- data.frame(rmse_val)
mydata <- data.frame(K, rmse_val, rmse_out_val)
newdata <- mydata %>% gather(key = "label", value = "combine_val", - K)
ggplot(newdata, aes(x = K, y = combine_val)) + geom_line(aes(color = label), size = 1.2) + scale_x_continuous(breaks = seq(3, N, 6)) + xlab('K_val') + ylab('RMSE') + scale_colour_discrete(breaks=c("rmse_out_val", "rmse_val"), labels=c("Test", "Train"))

### Plot K-nearest neighbors for best K of train and test data
# Training data
knnfit = knn.reg(train = realestate$house_age, y = realestate$price, 39)
rmse_val = RMSE(knnfit$pred, price)
knnfit <- knn.reg(train = realestate_train$house_age, y = realestate_train$price, k = 39)
ggplot(realestate_train, aes(x=house_age, y=price)) + geom_point(size=2, color = "#F8766D", shape=19) + geom_line(aes(x = house_age, y = knnfit$pred), color = "red")

# Test data
knnfit <- knn.reg(age_train, age_test, price_train, k = 39)
pred_val_test <- knnfit$pred
realestate_test$pred_val_test <- pred_val_test
ggplot(realestate_test, aes(x=house_age, y=price)) + geom_point(size=2, color = "#00BFC4", shape=19) + geom_line(aes(x = house_age, y = pred_val_test), color = "red")







