#Data PreProcessing
train=read.csv('train_friday.csv')
test=read.csv('test_friday.csv')

#Categorical Data
train$Gender=factor(train$Gender,levels=c('F','M'),labels=c(1,2))
test$Gender=factor(test$Gender,levels=c('F','M'),labels=c(1,2))
train$City_Category=factor(train$City_Category,levels=c('A','B','C'),labels = c(1,2,3))
test$City_Category=factor(test$City_Category,levels=c('A','B','C'),labels = c(1,2,3))

#Data Not required
train$Product_ID=NULL
test$Product_ID=NULL
train$Age=NULL
test$Age=NULL

#Missing Data
train$Product_Category_2=ifelse(is.na(train$Product_Category_2),ave(train$Product_Category_2,FUN = function(x) mean(x,na.rm = T)),train$Product_Category_2)
test$Product_Category_2=ifelse(is.na(test$Product_Category_2),ave(test$Product_Category_2,FUN = function(x) mean(x,na.rm = T)),test$Product_Category_2)
train$Product_Category_3=ifelse(is.na(train$Product_Category_3),ave(train$Product_Category_3,FUN = function(x) mean(x,na.rm = T)),train$Product_Category_3)
test$Product_Category_3=ifelse(is.na(test$Product_Category_3),ave(test$Product_Category_3,FUN = function(x) mean(x,na.rm = T)),test$Product_Category_3)



#Converting integer data to double for feature scaling
train$User_ID=as.numeric(train$User_ID)
train$Gender=as.numeric(train$Gender)
train$Marital_Status=as.numeric(train$Marital_Status)
train$Product_Category_1=as.numeric(train$Product_Category_1)
train$City_Category=as.numeric(train$City_Category)
train$Occupation=as.numeric(train$Occupation)
test$User_ID=as.numeric(test$User_ID)
test$Gender=as.numeric(test$Gender)
test$Marital_Status=as.numeric(test$Marital_Status)
test$Product_Category_1=as.numeric(test$Product_Category_1)
test$City_Category=as.numeric(test$City_Category)
test$Occupation=as.numeric(test$Occupation)

#Feature Scaling
train[-9] = scale(train[-9])
test = scale(test)

#Applying regressor for backward elimination
regressor = lm(formula = Purchase ~ .,
               data = training_set)

#After backward elimination removal of useless columns
train$Stay_In_Current_City_Years=NULL
test$Stay_In_Current_City_Years=NULL


#Fiting the model
library(randomForest)
set.seed(1234)
regressor_rf = randomForest(x = train[-9],
                         y = train$Purchase,
                         ntree = 500)


# Predicting a new result with Random Forest Regression
y_pred = predict(regressor_rf, newdata=test)
a=data.frame(y_pred)


#Visualizing the results
library(ggplot2)
x_grid = seq(min(train$User_ID), max(train$User_ID), 0.01)
ggplot() +
  geom_point(aes(x = train$User_ID, y = train$Purchase),
             colour = 'red') +
  geom_line(aes(x = train$User_ID, y = predict(regressor_rf, newdata=test,
            colour = 'blue'))) +
  ggtitle('Truth or Bluff (Random Forest Regression)') +
  xlab('User_ID') +
  ylab('Purchase')