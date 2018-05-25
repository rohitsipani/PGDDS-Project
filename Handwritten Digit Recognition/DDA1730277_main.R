# install.packages("caret")
library(caret)

# install.packages("kernlab")
library(kernlab)

# install.packages("dplyr")
library(dplyr)

# install.packages("readr")
library(readr)

# install.packages("ggplot2")
library(ggplot2)

# install.packages("gridExtra")
library(gridExtra)

####### Business Understanding #############

# The business is in the field of pattern recognintion

###### Problem Statement ###############

# A classic problem in the field of pattern recognition is that of handwritten digit recognition. 
# Suppose there is image of a digit submitted by a user via a scanner, a tablet, or other digital devices.

####### Goals of the assignment #######

# The goal is to develop a model using that can correctly identify the digit (between 0-9) written
# in an image based on pixel values given as features

####### Understanding of Pixel Values ###########

# For a gray scale images, the pixel value is single number that represents the brightness of the pixel.
# The most common pixel format is the byte image, where this number is stored as 8-bit integer
# giving a range of possible values from 0 to 255. 
# Typically zero is taken to be black and 255 is taken to be white

####################### Data Understanding#####################################

########### Train Data Set ##############

# loading the train dataset
mnist_train <- read_csv("mnist_train.csv", col_names = F, col_types = cols())

# Understanding Dimensions
dim(mnist_train)
# 60000 observations with 784 pixel values (independent variables) and 1 target variable having digits (0-9)

# Structure of the dataset
str(mnist_train)
# Data set do not have headers for all the columns and all the columns are given in integer data type

# printing first few rows
head(mnist_train)

# NA Values
sum(is.na(mnist_train)) # None na values observed

# Duplicated rows
sum(duplicated(mnist_train)) # None duplicated rows observed

# Checking target variable
summary(mnist_train$X1) # So all the values in this column lies between 0-9. Seems okay

# Checking the distribution of the target variable 
ggplot(mnist_train,aes(X1)) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title = "Distribution of Digit Variable", x= "Digit", y= "Count") + geom_text(stat='count',aes(label=..count..))
# The data is almost equally distributed with the range of 5400-6800

# Pixel values could be between 0-255. So lets verify that
sum(mnist_train[,-1]>255|mnist_train[,-1]<0) # No values are higher than 255 and lower of 0

# Checking independent variables
summary(mnist_train[,2:100])
summary(mnist_train[,101:200])
summary(mnist_train[,201:300])
summary(mnist_train[,301:400])
summary(mnist_train[,401:500])
summary(mnist_train[,501:600])
summary(mnist_train[,601:700])
summary(mnist_train[,701:785])
# As the range of value for all the pixels is between 0-255, it could be derived as an array of numbers between 
# 0 and 1 describing how dark each pixel is.

# Checking the image of one digit
digit <- matrix(as.numeric(mnist_train[8,-1]), nrow=28)
image(digit, col= grey.colors(255))
# Could observe a value 3

## Checking the intensity of each digit
mnist_train$intensity <- rowMeans(mnist_train[,-1])
mnist_train_intensity_grouped <- setNames(aggregate(mnist_train$intensity, by=list(mnist_train$X1), FUN = mean), c("Digit","Mean"))

# Plotting bar chart for intensity
ggplot(mnist_train_intensity_grouped,aes(x=Digit,y=Mean)) + geom_bar(stat="identity", fill = "green", col= "red", alpha=0.5) + labs(title = "Bar Chart of Intensity", x= "Digit", y= "Mean")
# Digit 1 has lower intensity and 0 has higher intensity

# removing intensity variable
mnist_train$intensity <- NULL

############ Test Data Set ###############

# loading the test dataset
mnist_test <- read_csv("mnist_test.csv", col_names = F, col_types = cols())

# Understanding Dimensions
dim(mnist_test)
# 10000 observations with 784 pixel values (independent variables) and 1 target variable having digits (0-9)

# Structure of the dataset
str(mnist_test)
# Data set do not have headers for all the columns and all the columns are given in integer data type

# printing first few rows
head(mnist_test)

# NA Values
sum(is.na(mnist_test)) # None na values observed

# Duplicated rows
sum(duplicated(mnist_test)) # None na values observed

# Checking target variable
summary(mnist_test$X1) # So all the values in this column lies between 0-9. Seems okay

# Checking the distribution 
ggplot(mnist_test,aes(x=X1)) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title = "Distribution of Digit Variable", x= "Digit", y= "Count") + geom_text(stat='count',aes(label=..count..))
# The data is almost equally distributed with the range of 890-1150

# Pixel values could be between 0-255. So lets verify that
sum(mnist_test[,-1]>255|mnist_test[,-1]<0) # No values are higher than 255 and lesser than 0

# Checking independent variables
summary(mnist_test[,2:100])
summary(mnist_test[,101:200])
summary(mnist_test[,201:300])
summary(mnist_test[,301:400])
summary(mnist_test[,401:500])
summary(mnist_test[,501:600])
summary(mnist_test[,601:700])
summary(mnist_test[,701:785])
# As the range of value for all the pixels is between 0-255, it could be derived as an array of numbers between 
# 0 and 1 describing how dark each pixel is.

## Checking the intensity of each digit
mnist_test$intensity <- rowMeans(mnist_test[,-1])
mnist_test_intensity_grouped <- setNames(aggregate(mnist_test$intensity, by=list(mnist_test$X1), FUN = mean), c("Digit","Mean"))

# Plotting bar chart for intensity
ggplot(mnist_test_intensity_grouped,aes(x=Digit,y=Mean)) + geom_bar(stat="identity", fill = "green", col= "red", alpha=0.5) + labs(title = "Bar Chart of Intensity", x= "Digit", y= "Mean")
# Digit 1 has lower intensity and 0 has higher intensity

# removing intensity variable
mnist_test$intensity <- NULL

############################ Data Preparation ###########################

########### Train Data Set ##############

# Removing the columns that has zero variance or near zero variance as it would not be useful in finding the predictor
near_zero_variance_train <- nearZeroVar(mnist_train) # 535 variables are having near to zero variance. Hence removing it from the train dataset.
mnist_train <- mnist_train[,-near_zero_variance_train] # 250 variables including target variable is left

# Changing the header of target variable to digit
colnames(mnist_train)[1] <- "digit"

# Changing the digit variable to factor
mnist_train$digit <- as.factor(mnist_train$digit)

# Normalizing the data
normalize <- function(i) {
  numerator <- i - min(i)
  denominator <- max(i) - min(i)
  return(numerator/denominator)
}

mnist_train[,-1] <- normalize(mnist_train[,-1])

############ Test Data Set ###############

# Removing the columns that was removed from training data set for zero variance or near zero variance
mnist_test <- mnist_test[,-near_zero_variance_train] # 535 variables are removed and 250 variables are left

# Changing the header of target variable to digit
colnames(mnist_test)[1] <- "digit"

# Changing the digit variable to factor
mnist_test$digit <- as.factor(mnist_test$digit)

# Normalizing the data
mnist_test[,-1] <- normalize(mnist_test[,-1])

########## Creating Sampled Data Set ##################
# As we have many observations in the train dataset which would take lot of time.
# Creating a sample from the train dataset 50% observations are kept for the sample.

# set the seed to 1, let's run it 
set.seed(1)

sample_dataset_indices = sample(1:nrow(mnist_train), 0.5*nrow(mnist_train))

# generate the sampled train data set
mnist_train_sampled = mnist_train[sample_dataset_indices,]

# Checking the distribution 
ggplot(mnist_train_sampled,aes(x=digit)) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title = "Distribution of Digit Variable", x= "Digit", y= "Count") + geom_text(stat='count',aes(label=..count..))
# The data is almost equally distributed with the range of 2700-3350 and similar to the entire train dataset

########### Splitting the sampled train data set to train and valdaition data ###########################
# As we have many observations in the train dataset which would take lot of time. 
# Diving the train data set for train and validation purpose.

# set the seed to 1, let's run it 
set.seed(1)

indices = sample(1:nrow(mnist_train_sampled), 0.7*nrow(mnist_train_sampled))

# generate the train data set
train = mnist_train_sampled[indices,] # 21000 observations

# Similarly store the rest of the observations into an object "validation".
validation = mnist_train_sampled[-indices,] # 9000 observations

############# Model Building & Model Evaluation ######################

###################### Linear Kernel Model ##########################################

# Using Linear Kernel
model_linear <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "vanilladot")

# printing the model
print(model_linear)
# C = 1
# Support Vectors : 4878
# Training Error : 0.28905

## Predicting the model results in validation dataset ##

evaluation_linear_1<- predict(model_linear, validation)

#confusion matrix - Linear Kernel
confusionMatrix(evaluation_linear_1,validation$digit)

# Accuracy : 0.8969
# Kappa : 0.8854
# Sensitivity (0-9) : 0.82161 - 0.9827
# Specificity (0-9) : 0.98256 - 0.99286
# Approx 90% of accuracy is observed in the validation dataset. Some class has lower sensitivity and some has higher.
# specificity is mostly same across all the classes

## Predicting the model results in test dataset ##

evaluation_linear_2<- predict(model_linear, mnist_test)

#confusion matrix - Linear Kernel
confusionMatrix(evaluation_linear_2,mnist_test$digit)

# Accuracy : 0.9007
# Kappa : 0.8896
# Sensitivity (0-9) : 0.8195 - 0.9859
# Specificity (0-9) : 0.9808 - 0.9926
# Approx 90% of accuracy is observed in the test dataset. Some class has lower sensitivity and some has higher.
# specificity is mostly same across all the classes

############# Hyperparameter tuning and Cross Validation  - Linear - SVM  #################### 

trainControl_linear <- trainControl(method="cv", number=3)
# Number - Number of folds 
# Method - cross validation

set.seed(100)

# making a grid of C values. 
grid_linear <- expand.grid(C=c(0.01,0.1,1,10,100))

# Performing 3-fold cross validation
fit.svm_linear <- train(digit~., data=train, method="svmLinear", metric="Accuracy", 
                 tuneGrid=grid_linear, trControl=trainControl_linear)

# Printing cross validation result
print(fit.svm_linear)
# Best tune at C=0.01 
# Accuracy : 0.9206194
# Kappa : 0.9117597

# Plotting "fit.svm_linear" results
plot(fit.svm_linear)

## Validating the model after cross validation on validation dataset ##

evaluate_linear_validation <- predict(fit.svm_linear, validation)

#confusion matrix
confusionMatrix(evaluate_linear_validation, validation$digit)

# Accuracy : 0.9214
# Kappa : 0.9127
# Sensitivity (0-9) : 0.87060 - 0.9806
# Specificity (0-9) : 0.98769 - 99347
# Approx 92% of accuracy is observed in the validation dataset. Some class has lower sensitivity and some has higher.
# specificity is mostly same across all the classes. The results are better than the linear model before tuning the hyperparameters

## Valdiating the model after cross validation on test data ##

evaluate_linear_test<- predict(fit.svm_linear, mnist_test)

#confusion matrix
confusionMatrix(evaluate_linear_test, mnist_test$digit)

# Accuracy : 0.9262
# Kappa : 0.918
# Sensitivity (0-9) : 0.8747 - 0.9885
# Specificity (0-9) : 0.9889 - 9951
# Approx 93% of accuracy is observed in the test dataset. Some class has lower sensitivity and some has higher.
# specificity is mostly same across all the classes. The results are better than the linear model before tuning the hyperparameters

# Lets check if we get better results by adding non linearity 

###################### RBF Kernel Model ##########################################

# Using RBF Kernel
model_RBF <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot")

# Printing the model
print(model_RBF)
# C = 1
# Sigma : 0.00223
# Support Vectors : 6305
# Training Error : 0.022238

## Predicting the model results in validation dataset ##

evaluation_RBF_1<- predict(model_RBF, validation)

#confusion matrix - RBF Kernel
confusionMatrix(evaluation_RBF_1,validation$digit)

# Accuracy : 0.9614
# Kappa : 0.9571
# Sensitivity (0-9) : 0.93757 - 0.9878
# Specificity (0-9): 0.99456 - 0.99741 
# Approx 96% of accuracy is observed in the validation dataset. Some class has lower sensitivity and some has higher.
# specificity is mostly same across all the classes. The results are better than the linear model

## Predicting the model results in test dataset ##

evaluation_RBF_2<- predict(model_RBF, mnist_test)

#confusion matrix - RBF Kernel
confusionMatrix(evaluation_RBF_2,mnist_test$digit)

# Accuracy : 0.9647
# Kappa : 0.9608
# Sensitivity (0-9) : 0.9484 - 0.9912 
# Specificity (0-9): 0.9940 - 0.9976 
# Approx 96% of accuracy is observed in the test dataset. Some class has lower sensitivity and some has higher.
# specificity is mostly same across all the classes. The results are better than the linear model

############ Registering cluster for parrellel processing #####################

# install.packages("doParallel")
library(doParallel)

registerDoParallel(makeCluster(detectCores()))

############   Hyperparameter tuning and Cross Validation #####################

# Train control function
trainControl_non_linear <- trainControl(method="cv", number=3)

# Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(100)
grid_non_linear <- expand.grid(.sigma = c(0.001, 0.003, 0.006, 0.012),.C=seq(2,10,2))

# Performing 3-fold cross validation

fit.svm_non_linear <- train(digit~., data=train, method="svmRadial", metric="Accuracy", 
                            tuneGrid=grid_non_linear, trControl=trainControl_non_linear)

# Printing cross validation result
print(fit.svm_non_linear)
# C = 4
# sigma : 0.006
# Kappa : 0.9673413

# Plotting "fit.svm_non_linear" results
plot(fit.svm_non_linear)

## Validating the model after cross validation on validation dataset ##

evaluate_non_linear_validation <- predict(fit.svm_non_linear, validation)

# confusion matrix
confusionMatrix(evaluate_non_linear_validation, validation$digit)

# Accuracy : 0.9741
# Kappa : 0.9712
# Sensitivity (0-9): 0.95809 - 0.9867
# Specificity (0-9): 0.99641 - 0.9978
# Approx 97% of accuracy is observed in the validation dataset. Some class has lower sensitivity and some has higher.
# specificity is mostly same across all the classes. The results are better than the non linear model before tuning the hyperparameters

## Valdiating the model after cross validation on test data ##

evaluate_non_linear_test<- predict(fit.svm_non_linear, mnist_test)

# confusion matrix
confusionMatrix(evaluate_non_linear_test, mnist_test$digit)

# Accuracy : 0.977
# Kappa : 0.9744
# Sensitivity (0-9): 0.9633 - 0.9930
# Specificity (0-9): 0.9962 - 0.9985
# Approx 98% of accuracy is observed in the test dataset. Some class has lower sensitivity and some has higher but the difference is reduced.
# specificity is mostly same across all the classes. The results are better than the non linear model before tuning the hyperparameters

################### Stopping Cluster ###################

stopCluster(makeCluster(detectCores()))

##################### Final Conclusion ############################

# Based on the above result, it is concluded that the dataset is having non linearity.
# The results of non linear model is better the linear model. 
# Also considering the lower sigma value, the non linearity in the model is not high and that is the result that
# even in linear model the accuracy is above 90%.

### Final model ###

# Model = fit.svm_non_linear

### Final Hyperparameters ###

# C = 4
# Sigma = 0.006

# With the final hyperameters the accuracy % for the all the dataset (train, validation, test) is very good
# Less variation has been observed between the accuracy of all these datasets and hence it is eliminating the chances of overfitting the train dataset
# Sensivity and specificity is also good across all the classes