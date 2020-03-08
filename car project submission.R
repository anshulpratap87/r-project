### loading the required libraries
require(car)
require(ggplot2)
require(dplyr)
require(dummies)
require(MASS)

### loading the car data in R.
car <- read.csv('car_prices.csv',stringsAsFactors = TRUE)
str(car)

## removing the column car_ID from the dataset
car <- car[,-1]

############ DATA PREPARATION
### checking for any dupicated rows
sum(duplicated(car))
car <- unique(car)

### checking for NA's if any
sum(is.na(car))

### removing the outliers from horsepower
plot(quantile(car$horsepower,seq(0,1,0.01)))
quantile(car$horsepower,seq(0,1,0.01))
car$horsepower[which(car$horsepower > 184)] <- 184

### creating dummy variables
car = dummy.data.frame(car)

### LR
set.seed(100)
index<-sample(1:nrow(car),0.85*nrow(car))
train_car<-car[index,]
test_car <-car[-index,]

# executing the linear model
model_1<-lm(price~.,data = train_car)
summary(model_1)

model_2 <- step(model_1, direction = "both")
summary(model_2)

model_3 <- lm(formula = price ~ symboling + fueltypediesel + aspirationstd + 
                carbodyconvertible + carbodyhardtop + carbodyhatchback + 
                carbodysedan + drivewheel4wd + wheelbase + curbweight + enginesize + 
                horsepower + peakrpm, data = train_car)
summary(model_3)
sort(vif(model_3))

model_4 <- lm(formula = price ~ symboling + fueltypediesel + aspirationstd + 
                carbodyconvertible + carbodyhardtop + carbodyhatchback + 
                carbodysedan + drivewheel4wd + wheelbase + enginesize + 
                horsepower + peakrpm, data = train_car)
summary(model_4)
sort(vif(model_4))

model_5 <- lm(formula = price ~ symboling + fueltypediesel + aspirationstd + 
                carbodyconvertible + carbodyhardtop + carbodyhatchback + 
                carbodysedan + drivewheel4wd + wheelbase + 
                horsepower + peakrpm, data = train_car)
summary(model_5)
sort(vif(model_5))

### price prediction
Predict_price <- predict(model_5,test_car[,-27])
test_car$test_price <- Predict_price

#correlation
cor(test_car$price,test_car$test_price)

cor(test_car$price,test_car$test_price)^2