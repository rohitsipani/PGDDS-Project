# install.packages("dplyr")
library(dplyr)

# install.packages("ggplot2")
library(ggplot2)

# install.packages("caret")
library(caret)

# install.packages("caTools")
library(caTools)

# install.packages("dummies")
library(dummies)

# install.packages("MASS")
library(MASS)

# install.packages("car")
library(car)


####### Business Understanding #############

# A Portuguese Bank had conducted a telemarketing campaign for a term-deposit product somewhere around late 2010. 
# Term deposit is very similar to a fixed deposit where you deposit money for a fixed period of time.  

# Through the campaign, they had collected data about the prospects' demographics, other financial products they have purchased in the past (loans, deposits etc.), the number of times they were called etc. 
# They also recorded the response data, i.e. whether the person had subscribed to the term-deposit product, which is the target variable. 

# The marketing team of the bank wants to launch yet another telemarketing campaign for the same product. 

####### Problem Statement ############

# To reiterate the response model problem we wanted to predict the probability of response of each prospect and target the ones most likely to respond to the next telemarketing campaign. 
# The steps were as follows:
  
# Identify the relevant predictor variables for response using EDA
# Build predictive models and choose the best one
# Sort the prospects in decreasing probability of response (predicted by the best model) and target the top X% (or top Y deciles), where X would be determined by your business objective (e.g. maximising the overall response rate/the number of responders at a fixed marketing cost)

# When we look at the important variables included in the final model, we will see a variable ‘duration’ — the duration of the phone call in seconds. 
# When we present this model to the Chief Marketing Officer (CMO), she would note that ‘duration’ has a positive correlation with the response, which is a problem for the marketing team. As the duration increases, the cost of telemarketing increases linearly, and that is the last thing they want.

# There are two problems with having the variable ‘duration’ in the model:
  
# When the marketing team procures prospect data, ‘duration’ is not present in it, since the call hasn’t been made yet
# In your analysis of marketing cost and response, you had assumed that the cost of a phone call is independent of duration (₹1 per call) — which is not true

######### Goals of Assignement #############

# To build another model without the variable ‘duration’. 
# That will help understanding the relationship of other variables with the response.

# The business objective is to achieve 80% of total responders at the minimum possible cost. 
# The total number of responders is the total number of prospects who responded, from the available data of about 45,000 data points.

######### Data Understanding & Data Preparation #############

# Loading bank marketing data in the working directory. 
bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 
str(bank_data)

# Summary of dataset
summary(bank_data)

# Summary of dataset
dim(bank_data)
# 41188 rows, 20 independent variables and 1 dependent variable

# Checking response rate of prospect customer
response <- (4640/(36548+4640))*100
response
# response rate 11.26542 %

# Checking missing values
sum(is.na(bank_data)) # No missing values observed

# Adding a unique ID column in the dataset
bank_data <- bank_data %>% mutate(ID= row_number())

# Bringing ID to the first column
bank_data <- bank_data %>% dplyr::select(ID, everything())

############################## Client Related Data ######################################

############## Checking Each Individual Columns #######################

## Age

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 
quantile(bank_data$age,seq(0,1,0.01))
# Outliers could be observed at 99% to 100%

# Box plot 
boxplot(bank_data$age)
# Same result as above outliers could be observed

# Capping the upper values of age with 71.
bank_data[(which(bank_data$age>71)),]$age <- 71

# Rechecking the Box plot 
boxplot(bank_data$age)
# Outliers are reduced

# Binning the age variable and store it into "binning.age".
bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"
bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket
agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
# 16 to 20, 60 to 70 and 70 to 80 has higher response rate but number of prospect is less.

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

# View the dataset
View(Bank_data_age20)

# checking the summary
summary(Bank_data_age20)

#------------------------------------------------------------------------------------

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
``  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

#--------------------------------------------------------------------------------------

## Job

# Checking the summary
summary(bank_data$job)

# Checking structure 
str(bank_data$job) #  12 levels observed

# Checking the levels
levels(bank_data$job)

# Plotting bar graph for job variable.
plot_response(bank_data$job, "job")
# Student is having the highest response rate, followed by retired. Rest all category lies between 0.07 to 0.14

#--------------------------------------------------------------------------------------

## Marital Status

# Checking the summary
summary(bank_data$marital)

# Checking structure 
str(bank_data$marital) #  4 levels observed

# Checking the levels
levels(bank_data$marital)

# Let's replace Unknown level to married
levels(bank_data$marital)[4] <- "married"

# Plotting marital status
plot_response(bank_data$marital,"marital")
# Not much difference in the response rate in different levels

#--------------------------------------------------------------------------------------

## Education

# Checking the summary
summary(bank_data$education)

# Checking structure 
str(bank_data$education) #  8 levels observed

# Checking the levels
levels(bank_data$education)

# Reducing the levels of education variable
levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's check the education plot
plot_response(bank_data$education,"Education_levels")
# Not much difference in the response rate in different levels

#--------------------------------------------------------------------------------------

## Default

# Checking the summary
summary(bank_data$default)

# Checking structure 
str(bank_data$default) #  3 levels observed

# Checking the levels
levels(bank_data$default)

# Plotting the default
plot_response(bank_data$default, "Default")
# As there are very less prospects with less it is not a good attribute for analysis

# Removing it from the dataset
bank_data <- bank_data[,-6]

#--------------------------------------------------------------------------------------

## Housing

# Checking the summary
summary(bank_data$housing)

# Checking structure 
str(bank_data$housing) #  3 levels observed

# Checking the levels
levels(bank_data$housing)

# Plotting housing
plot_response(bank_data$housing, "Housing")
# Not much difference in the response rate in different levels

#--------------------------------------------------------------------------------------

## Loan

# Checking the summary
summary(bank_data$loan)

# Checking structure 
str(bank_data$loan) #  3 levels observed

# Checking the levels
levels(bank_data$loan)

# Plotting loan 
plot_response(bank_data$loan, "Loan Status")
# No difference in the response rate in different levels

#--------------------------------------------------------------------------------------

############################## Campaign Related Data ######################################

############## Checking Each Individual Columns #######################

## Contact

# Checking the summary
summary(bank_data$contact)

# Checking structure 
str(bank_data$contact) #  2 levels observed

# Checking the levels
levels(bank_data$contact)

# Plotting Contact
plot_response(bank_data$contact,"Contact_mode")
# Cellular has high response rate than the telephone. It is an important factor

#--------------------------------------------------------------------------------------

## Month

# Checking the summary
summary(bank_data$month)

# Checking structure 
str(bank_data$month) #  10 levels observed

# Checking the levels
levels(bank_data$month)

# Plotting contact month 
plot_response(bank_data$month,"Contact_month")
# Mar, Sep, Oct, Dec has higher response rate than the other months which could be because of investment and tax planning

#--------------------------------------------------------------------------------------

## Day_of_week

# Checking the summary
summary(bank_data$day_of_week)

# Checking structure 
str(bank_data$day_of_week) #  5 levels observed

# Checking the levels
levels(bank_data$day_of_week)

# Plotting day_of_week
plot_response(bank_data$day_of_week,"day_of_week")
# Not much difference in the response rate in different levels

#--------------------------------------------------------------------------------------

# Duration

# Checking the summary
summary(bank_data$duration)

# Checking structure 
str(bank_data$duration)

# Let's check the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

# Removing the factor response variable
bank_data <- bank_data[,-23]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 
quantile(bank_data$duration,seq(0,1,0.01))
# outliers could be observed between 99% to 100%

# So, capping the duration seconds at 99% which is 1271.3sec 
bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()
# high number count at duration 100 to 300 seconds 

#--------------------------------------------------------------------------------------

# Campaign

# Checking the summary
summary(bank_data$campaign)

# Checking structure 
str(bank_data$campaign)

# Box plot
boxplot(bank_data$campaign)
# high outliers could be observed

# Let's see the percentile distribution of this variable
quantile(bank_data$campaign,seq(0,1,0.01))
# outliers could be observed between 99% to 100% 

# Capping this at 99% which the value is 14
bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Rechecking box plot
boxplot(bank_data$campaign)
# Many outliers removed

# Visualizing it with plot
ggplot(bank_data,aes(campaign))+geom_histogram()
# Higher count at the campaign of 1 & 2

#--------------------------------------------------------------------------------------

# Pdays

# Checking the summary
summary(bank_data$pdays)

# Checking structure 
str(bank_data$pdays)

# Converting this into factor type
bank_data$pdays<- as.factor(bank_data$pdays)

# Rechecking summary
summary(bank_data$pdays)

# Checking levels
levels(bank_data$pdays)

# Reducing the levels of this variable to 3.
levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"

# Rechecking summary
summary(bank_data$pdays)

# Plotting pday 
plot_response(bank_data$pday,"Pday")
# Contacted in first 10 days and after 10 days has higher response rate than first contacted
#--------------------------------------------------------------------------------------

# Previous

# Checking the summary
summary(bank_data$previous)

# Checking structure 
str(bank_data$previous)

# Converting it to factor type
bank_data$previous <- as.factor(bank_data$previous)

# Rechecking the summary
summary(bank_data$previous)

# Reducing the levels of this variable to 3.
levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"

# Rechecking the summary
summary(bank_data$previous)

# Plotting previous
plot_response(bank_data$previous,"Previous_contacts")
# More than 3 times has higher response rate

#--------------------------------------------------------------------------------------

# Poutcome

# Checking the summary
summary(bank_data$poutcome)

# Checking structure 
str(bank_data$poutcome) # 3 levels

# Checking levels
levels(bank_data$poutcome)

# Plotting poutcome
plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")
# Success response rate was higher than other categories
#--------------------------------------------------------------------------------------
############################## Economic Related Data ######################################

############## Checking Each Individual Columns #######################

## emp.var.rate 

# Checking summary
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()
#--------------------------------------------------------------------------------------

## cons.price.idx

# Checking summary 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()
#--------------------------------------------------------------------------------------

## cons.conf.idx

# Checking summary 
summary(bank_data$cons.conf.idx)

# Histogram of consumer confidence index variable
ggplot(bank_data,aes(cons.conf.idx))+geom_histogram()

#--------------------------------------------------------------------------------------

## euribor3m

# Checking summary 
summary(bank_data$euribor3m)

# Histogram of Euribor 3 Months variable
ggplot(bank_data,aes(euribor3m))+geom_histogram()

#--------------------------------------------------------------------------------------

## nr.employed

# Checking summary 
summary(bank_data$nr.employed)

# Histogram of number of employed variable
ggplot(bank_data,aes(nr.employed))+geom_histogram()
#--------------------------------------------------------------------------------------

# Removing age variable as binning variable is created 
bank_data <- bank_data[, -2]

# Bringing binning variable to the second column
bank_data <- bank_data %>% dplyr::select(ID, binning.age, everything())

#creating dummy variables
bank_data$response <- as.integer(bank_data$response)

k1 <- bank_data

bank_data <- dummy.data.frame(bank_data)

# Checking the structure
str(bank_data)

# Changing the response variable to factor
bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))
#--------------------------------------------------------------------------------------

######################################### Model Building #########################################

# splitting into train and test data
set.seed(1)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data) # 70%

nrow(test)/nrow(bank_data) # 30%

#--------------------------------------------------------------------------------------

# Logistic Regression: 

# Initial model
model_1 <- glm(response ~ ., family = "binomial", data = train[,c(-1,-52)])
summary(model_1)
# AIC 16010
# NullDev 20299
# ResDev 15906

# Calculationg Pseudo R square
1 - (model_1$deviance/model_1$null.deviance) # 0.2164258

# Now, lets see how to use stepAIC
model_2<- stepAIC(model_1, direction="both")

# Let us look at the summary of the model
summary(model_2)
# AIC 15980
# NullDev 20299
# ResDev 15918

# Calculationg Pseudo R square
1 - (model_2$deviance/model_2$null.deviance) # 0.2158328

## Let us check for multicollinearity 
sort(vif(model_2))

# removing `binning.age(20,30]` with 5.518731 VIF and no star significance
model_3 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                 `binning.age(50,60]` + jobretired + 
                 jobstudent + jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthjul + monthjun + monthmar + monthmay + monthnov + monthoct + 
                 day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + nr.employed + jobunknown + 
                 `previousMore than_3_times`, family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_3)
# AIC 15981
# NullDev 20299
# ResDev 15921

# Calculationg Pseudo R square
1 - (model_3$deviance/model_3$null.deviance) # 0.2156887

## Let us check for multicollinearity 
sort(vif(model_3))

# There are no more high VIF with less than 3 star significance
# Hence removing the variables with high VIFs with three star significance due to high collinearity

# removing emp.var.rate with 139.764450 VIF and three star significance
model_4 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                 `binning.age(50,60]` + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthjul + monthjun + monthmar + monthmay + monthnov + monthoct + 
                 day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +  
                 cons.price.idx + cons.conf.idx + nr.employed + jobunknown + 
                 `previousMore than_3_times`, family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_4)
# AIC 16083
# NullDev 20299
# ResDev 16025

# Calculationg Pseudo R square
1 - (model_4$deviance/model_4$null.deviance) # 0.2105495

## Let us check for multicollinearity 
sort(vif(model_4))

# removing monthapr with 2.492001 VIF and with two star significance
model_5 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                 `binning.age(50,60]` + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular +  
                 monthjul + monthjun + monthmar + monthmay + monthnov + monthoct + 
                 day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +  
                 cons.price.idx + cons.conf.idx + nr.employed + jobunknown + 
                 `previousMore than_3_times`, family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_5)
# AIC 16089
# NullDev 20299
# ResDev 16033

# Calculationg Pseudo R square
1 - (model_5$deviance/model_5$null.deviance) # 0.210163

## Let us check for multicollinearity 
sort(vif(model_5))

# There are no more variables with high VIFs. Hence removing the variables with less significance

# removing monthoct with no star significance
model_6 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                 `binning.age(50,60]` + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular +  
                 monthjul + monthjun + monthmar + monthmay + monthnov + 
                 day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +  
                 cons.price.idx + cons.conf.idx + nr.employed + jobunknown + 
                 `previousMore than_3_times`, family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_6)
# AIC 16087
# NullDev 20299
# ResDev 16033

# Calculationg Pseudo R square
1 - (model_6$deviance/model_6$null.deviance) # 0.2101579

## Let us check for multicollinearity 
sort(vif(model_6))

# removing d`previousMore than_3_times` with no star significance
model_7 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                 `binning.age(50,60]` + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular +  
                 monthjul + monthjun + monthmar + monthmay + monthnov + 
                 day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +  
                 cons.price.idx + cons.conf.idx + nr.employed + jobunknown, 
                 family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_7)
# AIC 16087
# NullDev 20299
# ResDev 16035

# Calculationg Pseudo R square
1 - (model_7$deviance/model_7$null.deviance) # 0.210068

## Let us check for multicollinearity 
sort(vif(model_7))

# removing day_of_weekfri with no star significance
model_8 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                 `binning.age(50,60]` + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular +  
                 monthjul + monthjun + monthmar + monthmay + monthnov + 
                 day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +  
                 cons.price.idx + cons.conf.idx + nr.employed + jobunknown, 
               family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_8)
# AIC 16087
# NullDev 20299
# ResDev 16037

# Calculationg Pseudo R square
1 - (model_8$deviance/model_8$null.deviance) # 0.209977

## Let us check for multicollinearity 
sort(vif(model_8))

# removing jobunknown with no star significance
model_9 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                 `binning.age(50,60]` + jobretired + jobstudent + 
                 jobtechnician + maritaldivorced + educationPrimary_Education + 
                 educationTertiary_Education + contactcellular +  
                 monthjul + monthjun + monthmar + monthmay + monthnov + 
                 day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                 pdaysContacted_after_10days + poutcomefailure +  
                 cons.price.idx + cons.conf.idx + nr.employed, 
               family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_9)
# AIC 16087
# NullDev 20299
# ResDev 16039

# Calculationg Pseudo R square
1 - (model_9$deviance/model_9$null.deviance) # 0.2098819

## Let us check for multicollinearity 
sort(vif(model_9))

# removing maritaldivorced with no star significance
model_10 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                  `binning.age(50,60]` + jobretired + jobstudent + 
                  jobtechnician + educationPrimary_Education + 
                  educationTertiary_Education + contactcellular +  
                  monthjul + monthjun + monthmar + monthmay + monthnov + 
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  
                  cons.price.idx + cons.conf.idx + nr.employed, 
                family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_10)
# AIC 16087
# NullDev 20299
# ResDev 16041

# Calculationg Pseudo R square
1 - (model_10$deviance/model_10$null.deviance) # 0.2097914

## Let us check for multicollinearity 
sort(vif(model_10))

# removing cons.conf.idx with no star significance
model_11 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                  `binning.age(50,60]` + jobretired + jobstudent + 
                  jobtechnician + educationPrimary_Education + 
                  educationTertiary_Education + contactcellular +  
                  monthjul + monthjun + monthmar + monthmay + monthnov + 
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  
                  cons.price.idx + nr.employed, 
                family = "binomial", data = train[, c(-1, -52)])
  
# Let us look at the summary of the model
summary(model_11)
# AIC 16087
# NullDev 20299
# ResDev 16043

# Calculationg Pseudo R square
1 - (model_11$deviance/model_11$null.deviance) # 0.2096844

## Let us check for multicollinearity 
sort(vif(model_11))

# removing `binning.age(50,60]` with no star significance
model_12 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                  jobretired + jobstudent + jobtechnician + educationPrimary_Education + 
                  educationTertiary_Education + contactcellular +  
                  monthjul + monthjun + monthmar + monthmay + monthnov + 
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  
                  cons.price.idx + nr.employed, 
                family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_12)
# AIC 16088
# NullDev 20299
# ResDev 16046

# Calculationg Pseudo R square
1 - (model_12$deviance/model_12$null.deviance) # 0.2095487

## Let us check for multicollinearity 
sort(vif(model_12))

# removing jobtechnician with no star significance
model_13 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                  jobretired + jobstudent + educationPrimary_Education + 
                  educationTertiary_Education + contactcellular +  
                  monthjul + monthjun + monthmar + monthmay + monthnov + 
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  
                  cons.price.idx + nr.employed, 
                family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_13)
# AIC 16089
# NullDev 20299
# ResDev 16049

# Calculationg Pseudo R square
1 - (model_13$deviance/model_13$null.deviance) # 0.2094024

## Let us check for multicollinearity 
sort(vif(model_13))

# removing educationTertiary_Education with no star significance
model_14 <- glm(formula = response ~ `binning.age(30,40]` + `binning.age(40,50]` + 
                  jobretired + jobstudent + educationPrimary_Education + 
                  contactcellular + monthjul + monthjun + monthmar + monthmay + monthnov + 
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  
                  cons.price.idx + nr.employed, 
                family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_14)
# AIC 16089
# NullDev 20299
# ResDev 16051

# Calculationg Pseudo R square
1 - (model_14$deviance/model_14$null.deviance) # 0.2092664

## Let us check for multicollinearity 
sort(vif(model_14))

# removing `binning.age(30,40]` with no star significance
model_15 <- glm(formula = response ~ `binning.age(40,50]` + 
                  jobretired + jobstudent + educationPrimary_Education + 
                  contactcellular + monthjul + monthjun + monthmar + monthmay + monthnov + 
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  
                  cons.price.idx + nr.employed, 
                family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_15)
# AIC 16091
# NullDev 20299
# ResDev 16055

# Calculationg Pseudo R square
1 - (model_15$deviance/model_15$null.deviance) # 0.2090979

## Let us check for multicollinearity 
sort(vif(model_15))

# removing `binning.age(40,50]` with no star significance
model_16 <- glm(formula = response ~ jobretired + jobstudent + educationPrimary_Education + 
                  contactcellular + monthjul + monthjun + monthmar + monthmay + monthnov + 
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  
                  cons.price.idx + nr.employed, 
                family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_16)
# AIC 16090
# NullDev 20299
# ResDev 16056

# Calculationg Pseudo R square
1 - (model_16$deviance/model_16$null.deviance) # 0.2090346

## Let us check for multicollinearity 
sort(vif(model_16))

# removing jobstudent with two star significance
model_17 <- glm(formula = response ~ jobretired + educationPrimary_Education + 
                  contactcellular + monthjul + monthjun + monthmar + monthmay + monthnov + 
                  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  
                  cons.price.idx + nr.employed, 
                family = "binomial", data = train[, c(-1, -52)])

# Let us look at the summary of the model
summary(model_17)
# AIC 16097
# NullDev 20299
# ResDev 16065

# Calculationg Pseudo R square
1 - (model_17$deviance/model_17$null.deviance) # 0.2085917

## Let us check for multicollinearity 
sort(vif(model_17))

model_final <- model_17
#--------------------------------------------------------------------------------------

# Predicting probabilities of responding for the test data
predictions_model <- predict(model_final, newdata = test[, -68], type = "response")
summary(predictions_model)

test$prob <- predictions_model

# Sort the data points in decreasing order of probability of response
test <- test[order(-test$prob),]
#--------------------------------------------------------------------------------------

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

test$predicted_response <- factor(ifelse(test$prob >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(test$predicted_response, test$response, positive = "yes")

conf
# Accuracy - 0.899
# Sensitivity - 0.21408
# Specificity - 0.98595
#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  test$predicted_response <- factor(ifelse(test$prob >= cutoff, "yes", "no"))
  conf <- confusionMatrix(test$predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.011 to 0.90 for plotting and initiallizing a matrix of 150 X 3.

s = seq(.011,.90,length=150)

OUT = matrix(0,150,3)


for(i in 1:150)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 7.66% for final model

test$predicted_response <- factor(ifelse(test$prob >= cutoff, "yes", "no"))

conf_final <- confusionMatrix(test$predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
# 0.7207025
sens
# 0.7133621
spec
# 0.7216344
#---------------------------------------------------------    

########################### Model Evaluation ###############################################

# Creating new dataframe "test_predictions"

test_predictions <- test[, c("ID","response", "predicted_response","prob", "duration")]

#---------------------------------------------------------    

# Creating new column cost of call
test_predictions$cost_of_call <- (0.033*test_predictions$duration) + 0.8

#---------------------------------------------------------    

summary(test_predictions$response)
summary(test_predictions$predicted_response)

#---------------------------------------------------------    

# Response rate for test dataset
response_rate <- table(test$response)[2]/(table(test$response)[1] + table(test$response)[2])

#---------------------------------------------------------    

### KS -statistic - Test Data ######

# install.packages("ROCR")
library(ROCR)

test_predictions$response <- ifelse(test_predictions$response=="yes",1,0)
test_predictions$predicted_response <- ifelse(test_predictions$predicted_response=="yes",1,0)

#on testing  data
pred_object_test<- prediction(test_predictions$predicted_response, test_predictions$response)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

######### Plot Receiver Operating Characteristics (ROC) Curve #####################

plot(performance_measures_test, type = "b", col = "red", lwd=1.5,
     main = "ROC Curve",
     ylab = "Sensitivity:TPR", 
     xlab = "(1 - Specificity):FPR")
abline(0,1, lty = 8, col = "grey", untf = T)
auc<-performance(pred_object_test,"auc")
auc.value <- unlist(auc@y.values)
text(0.8, 0.23, labels=sprintf("AUC: %0.3f", auc.value)) # 0.717

######## ks_table_test ##############

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 0.4349965

#---------------------------------------------------------    

# Creating lift function 

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

#---------------------------------------------------------    

# Create a Table of cumulative gain and lift 

LG = lift(test_predictions$response, test_predictions$prob, groups = 10)

#---------------------------------------------------------    

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Find the number of top X% prospects you should target to meet the business objective
# Report the average call duration for targeting the top X% prospects to the CMO (report this as a comment in the R file)

#  Business objective is to achieve 80% of total responders at the minimum possible cost.
# To attain 80% of total responders top 50% of prospects needs to be targeted as per the above gain chart

## In the entire test dataset ##

# Total cost in test dataset
sum(test_predictions$cost_of_call) # 112725 INR

# Total duration
sum(test_predictions$duration) # 3116371 seconds

# Total prospects
length(test_predictions$ID) # 12356 prospect

# Total responders in test dataset
sum(test_predictions$response) # 1392 responders

# Average cost per prospect
sum(test_predictions$cost_of_call)/length(test_predictions$ID) # 9.123102 per responders

# Average cost per responder
sum(test_predictions$cost_of_call)/sum(test_predictions$response) # 80.98064 per responders

# Average duration per prospect
sum(test_predictions$duration)/length(test_predictions$ID) # 252.2152 seconds per prospect

## Top 50% of prospect ##

# Total prospects in top 50%
1236+1236+1235+1236+1235
# 6178 Propects

# Creating a data frame with top 50% of prospects
test_predictions_top_50_percent <- test_predictions[1:6178,]

# Total cost
sum(test_predictions_top_50_percent$cost_of_call) # 58739.86 INR

# Total duration
sum(test_predictions_top_50_percent$duration) # 1630226 seconds

# Total prospects
length(test_predictions_top_50_percent$ID) # 6178 prospect

# Total responders
sum(test_predictions_top_50_percent$response) # 1118 responders

# Average cost per prospect
sum(test_predictions_top_50_percent$cost_of_call)/length(test_predictions_top_50_percent$ID) # 9.507909 per responders

# Average cost per responder
sum(test_predictions_top_50_percent$cost_of_call)/sum(test_predictions_top_50_percent$response) # 52.54013 per responders

# Average duration per prospect
sum(test_predictions_top_50_percent$duration)/length(test_predictions_top_50_percent$ID) # 263.876 seconds per prospect

# Reduction in average cost per responder could be observed with a model and without a model.
# Without a model - 80.98064
# With a model - 52.54013

# Lift as per cost
80.98064/52.54013 # 1.54131
#---------------------------------------------------------    

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")

# The Cumulative Lift of 1.6 for top 5 deciles,
# means that when selecting 50% of the records based on the model, 
# one can expect 1.6 times the total number of targets (events) found by randomly 
# selecting 50%-of-records without a model.

### Analyzing the Charts: Cumulative gains and lift charts are a graphical 
# representation of the advantage of using a predictive model to choose which 
# customers to contact. The lift chart shows how much more likely we are to receive
# respondents than if we contact a random sample of customers. For example,
# by contacting only 50% of customers based on the predictive model we will reach 
# 1.6 times as many respondents as if we use no model.