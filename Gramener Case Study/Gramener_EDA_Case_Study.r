# install.packages("dplyr")
library(dplyr)

# install.packages("tidyr")
library(tidyr)

# install.packages("stringr")
library(stringr)

# install.packages("zoo")
library(zoo)

# install.packages("lubridate")
library(lubridate)

# install.packages("e1071")
library(e1071)

# install.packages("ggplot2")
library(ggplot2)

# install.packages("corrplot")
library(corrplot)

# install.packages("gridExtra")
library(gridExtra)

# loading loan data set into R
loan <- read.csv("loan.csv", header=T, na.strings = c("","NA"))

# summary of df
summary(loan)

# Data Cleaning

# removing variables having same one  unique value as we need to find the variables that are strong indicators of default and one unique value cannot be a indicator
loan <- loan[sapply(loan, function(x) length(unique(x))>1)] # 60 variables were have same unique value hence removed and left with 51 variables

#  NA Values
sum(is.na(loan)) # 117563 na values observed
sapply(loan, function(x) (length(which(is.na(x))))) # checking for NA values for each column; 
# NA observed for emp_title, desc,  title, mths_since_last_delinq, mths_since_last_record, revol_util, last_pymnt_d, next_pymnt_d,  last_credit_pull_d, collections_12_mths_ex_med, chargeoff_within_12_mths, pub_rec_bankruptcies,  tax_liens
# Action would be taken while analyzing each column separately

# Dividing the dataset into three kind of variables

# i. Loan Characteristics / Customer Information / Customer Demographics / Spend Behaviour

# id
# member_id
# loan_amnt
# funded_amnt
# funded_amnt_inv
# term
# int_rate
# installment
# purpose
# title
# dti
# verification_status
# issue_d
# url
# desc
# delinq_2yrs
# earliest_cr_line
# inq_last_6mths
# mths_since_last_delinq
# mths_since_last_record
# open_acc
# pub_rec
# revol_bal
# revol_util
# total_acc
# chargeoff_within_12_mths
# pub_rec_bankruptcies
# tax_liens
# emp_title
# emp_length
# home_ownership
# annual_inc
# zip_code
# addr_state
# grade
# sub_grade

# 2. Customer Payment Behaviour

# out_prncp
# out_prncp_inv
# total_pymnt
# total_pymnt_inv
# total_rec_prncp
# total_rec_int
# total_rec_late_fee
# recoveries
# collection_recovery_fee
# last_pymnt_d
# last_pymnt_amnt
# next_pymnt_d
# last_credit_pull_d
# collections_12_mths_ex_med

# The company wants to understand the deriving factors leading to customer default. Hence, 3rd category 
# will not be relevant for EDA, as payment information will not be available with the bank at the time it makes a decision
# Dropping all these variables

loan$out_prncp <- NULL
loan$out_prncp_inv <- NULL
loan$total_pymnt <- NULL
loan$total_pymnt_inv <- NULL
loan$total_rec_prncp <- NULL
loan$total_rec_int <- NULL
loan$total_rec_late_fee <- NULL
loan$recoveries <- NULL
loan$collection_recovery_fee <- NULL
loan$last_pymnt_d <- NULL
loan$last_pymnt_amnt <- NULL
loan$next_pymnt_d <- NULL
loan$last_credit_pull_d <- NULL
loan$collections_12_mths_ex_med <- NULL

# Data Cleaning
# Check individual columns

# 1. id
summary(loan$id)

# i. Looking for duplicate value
sum(duplicated(loan$id)) # No duplicate ID's here

# ii. converting to character
loan$id <- as.character(loan$id)

# As each row is unique it would not be helpful for analysis, but it is unique and could be used for merging or joining with other dataset. Hence not removing

# 2. member_id
summary(loan$member_id)

# i. Looking for duplicate value
sum(duplicated(loan$member_id)) # No duplicate ID's here

# ii. As each row is unique it would not be helpful for analysis and already we do have one unique key, hence removing
loan$member_id <- NULL

# 3. loan_amnt
summary(loan$loan_amnt) # seems okay

# 4. funded_amnt
summary(loan$funded_amnt) # seems okay

# verifying funded_amt cannot be more than loan amount
sum(loan$funded_amnt > loan$loan_amnt) # zero observed so its okay

# 5. funded_amnt_inv
summary(loan$funded_amnt_inv)

# i. rounding off
loan$funded_amnt_inv <- round(loan$funded_amnt_inv)

# verifying funded_amt_inv cannot be more than funded_amt amount
sum(loan$funded_amnt_inv > loan$funded_amnt) # zero observed so its okay

# 6. term
summary(loan$term)

# i. removing word "months" from column
loan$term <- str_trim(str_replace(loan$term, pattern = "months",""), side = "both")
summary(loan$term)

# 7. int.rate
summary(loan$int_rate)

# i. remove % from int.rate column
loan$int_rate <- str_replace(loan$int_rate, pattern = "[%]", "")

# ii. converting to numeric
loan$int_rate <- as.numeric(loan$int_rate)
summary(loan$int_rate)

# 8. Installment
summary(loan$installment)

# i. rounding off
loan$installment <- round(loan$installment)

# 9. grade
summary(loan$grade) # seems okay

# 10. subgrade
summary(loan$sub_grade) # seems okay

# 11. emp_title
summary(loan$emp_title)

# i. trim the column to remove trailing and leading whitespace
loan$emp_title <- str_trim(loan$emp_title, side = "both")

# ii. changing everything to uppercase
loan$emp_title <- toupper(loan$emp_title) 

# iii. how many unique values are there
length(unique(loan$emp_title))

# As there are lot of unique values it would not be helpful for analysis, hence removing this variable
loan$emp_title <- NULL

# 12. emp_length
summary(loan$emp_length)

# i. replacing n/a with NA as it could have any values between o to 10, hence considering as missing values
loan$emp_length[loan$emp_length == "n/a"] <- NA 

# ii. removing year and years fromthe column
loan$emp_length <- str_replace(loan$emp_length, "[[:space:]]years","")
loan$emp_length <- str_replace(loan$emp_length, "[[:space:]]year","")


# iii. replace < 1 to 0 and 10+ to 10
loan$emp_length[loan$emp_length == "< 1"] <- 0
loan$emp_length[loan$emp_length == "10+"] <- 10

# iv. changing it to numeric
loan$emp_length <- as.numeric(loan$emp_length)
summary(loan$emp_length)

# 13. home_ownership
summary(loan$home_ownership)

# i. Replacing none as NA
loan$home_ownership[loan$home_ownership == "NONE"] <- NA
summary(loan$home_ownership)

# 14. annual_inc
summary(loan$annual_inc)

# i. rounding off
loan$annual_inc <- round(loan$annual_inc)

# 15. verification_status
summary(loan$verification_status) # seems okay

# 16. issue_d
summary(loan$issue_d)
str(loan$issue_d)

# i. changing to date data type
loan$issue_d <- as.Date(as.yearmon(loan$issue_d, "%b-%y"))
loan$issue_d <- as.Date(ifelse(loan$issue_d > "2018-12-31", format(loan$issue_d, "19%y-%m-%d"),format(loan$issue_d)))
str(loan$issue_d)

# ii. deriving month & year variable
loan$issue_year <- format(loan$issue_d, "%Y")
loan$issue_month <- format(loan$issue_d, "%m")
  
# 17. loan_status
summary(loan$loan_status) # seems okay

# 18. url
summary(loan$url)

# i. check number of unique values
length(unique(loan$url))

# ii. As url have all the unique values it would not be helpful for analysis, removing that variable
loan$url <- NULL

# 19. desc

# i. check number of unique values
length(unique(loan$desc))

# ii. As there are lot of unique values it is not useful for analysis, hence removing the column
loan$desc <- NULL

# 20. purpose
summary(loan$purpose) # seems okay

# 21. title
summary(loan$title)

# i. Similar to purpose variable, hence removing
loan$title <- NULL

# 22. zipcode
summary(loan$zip_code) # seems okay

# 23. addr_state
summary(loan$addr_state) # seems okay

# 24. dti
summary(loan$dti)

# i. rounding off
loan$dti <- round(loan$dti)

# 25. deling_2yrs
summary(loan$delinq_2yrs) # not useful for analysis as most of the values are zero
sum(loan$delinq_2yrs == 0) / length(loan$delinq_2yrs) # 89% of the values are zero

# i. As 89% values are zero, it is not useful for analysis, hence removing
loan$delinq_2yrs <- NULL

# 26. earliest_cr_line
summary(loan$earliest_cr_line)
str(loan$earliest_cr_line)

# i. changing to date data type
loan$earliest_cr_line <- as.Date(as.yearmon(loan$earliest_cr_line, "%b-%y"))
loan$earliest_cr_line <- as.Date(ifelse(loan$earliest_cr_line > "2018-12-31", format(loan$earliest_cr_line, "19%y-%m-%d"),format(loan$earliest_cr_line)))
str(loan$earliest_cr_line)

# ii. deriving month and year variable
loan$earliest_cr_line_year <- format(loan$earliest_cr_line, "%Y")
loan$earliest_cr_line_month <- format(loan$earliest_cr_line, "%m")

# 27.inq_last_6mths
summary(loan$inq_last_6mths) # seems okay

# 28. mths_since_last_delinq
summary(loan$mths_since_last_delinq) # many of values are NA's
sum(is.na(loan$mths_since_last_delinq)) / length(loan$mths_since_last_delinq) # approx 65% values are NA's

# As 65% of values are NA's it is not useful for analysis, hence removing
loan$mths_since_last_delinq <- NULL

# 29. mths_since_last_record
summary(loan$mths_since_last_record) # many of values are NA's
sum(is.na(loan$mths_since_last_record)) / length(loan$mths_since_last_record) # approx 93% values are NA's

# As 93% of values are NA's it is not useful for analysis, hence removing
loan$mths_since_last_record <- NULL

# 30. open_acc
summary(loan$open_acc) # seems okay

# 31. pub_rec
summary(loan$pub_rec) # many of the values are zero
sum(loan$pub_rec == 0) / length(loan$pub_rec) # 95% values are zero

# i. As 95% values are zero it is not useful for analysis, hence removing
loan$pub_rec <- NULL

# 32. revol_bal 
summary(loan$revol_bal) # seems okay

# 33. revol_util
summary(loan$revol_util)

# removing % sign from the column
loan$revol_util <- str_replace(loan$revol_util, pattern = "[%]", "")

# ii. converting to numeric
loan$revol_util <- as.numeric(loan$revol_util)
summary(loan$revol_util)

# 34. total_acc
summary(loan$total_acc) # seems okay

# 35. chargeoff_within_12_mths
summary(loan$chargeoff_within_12_mths) # only zero and NA are there, hence not useful for analysis

# i. removing from analysis
loan$chargeoff_within_12_mths <- NULL

# 36. pub_rec_bankruptcies
summary(loan$pub_rec_bankruptcies) # many zero and NA are there
(sum(loan$pub_rec_bankruptcies == 0, na.rm = T) + sum(is.na(loan$pub_rec_bankruptcies)))/ length(loan$pub_rec_bankruptcies) # 96% of values are either NA's or Zero

# i. As 96% of values are either NA's or Zero so it is not useful for analysis, hence removing
loan$pub_rec_bankruptcies <- NULL

# 37. tax_liens
summary(loan$tax_liens) # only zero and NA are there, hence not useful for analysis

# i. tax_liens
loan$tax_liens <- NULL

# Creating a new data frame with loan status = Charged Off for univariate analysis
loan_charged_off <- subset(loan, loan$loan_status == "Charged Off")

# Creating a new variable having defaulters as 1 (Loan Status = Charged Off) and non-defaulters as 0 (Loan Status !=  Charged Off)  for bivariate and segmented variate
loan$defaulters <- factor(ifelse(loan$loan_status == "Charged Off", 1,0))


# univariate Analysis

# 1. Analysis of loan_amt

# ploting histogram for loan amount
ggplot(loan_charged_off, aes(x=loan_amnt)) + geom_histogram(stat="bin", binwidth = 3500, bins = 10, breaks = seq(0,35000,by=3500), col="red", fill="green",alpha=0.5) + labs(title="Histogram for Loan Amount") + geom_text(stat='bin', binwidth = 3500, bins = 10, breaks = seq(0,35000,by=3500), aes(label=..count..)) + scale_x_continuous(breaks = seq(0,35000,by=3500))

# Listed amount of the loan applied by the borrower within 3500 to 7000 range were defaulted higher than other ranges  

# ploting box plot for loan amount
ggplot(loan_charged_off, aes(x= "", y= loan_amnt)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Loan Amount") + ggtitle("Boxplot of Loan Amount")

summary(loan_charged_off$loan_amnt) # summary

skewness(loan_charged_off$loan_amnt) # skewness

# Observations
# i. Outliers are observed, that needs to be removed
# ii. The data is skewed to the right

# 2. Analysis of funded_amt

# ploting histogram for funded amount
ggplot(loan_charged_off, aes(x=funded_amnt)) + geom_histogram(stat="bin", binwidth = 3500, bins = 10, breaks = seq(0,35000,by=3500), col="red", fill="green",alpha=0.5) + labs(title="Histogram for Funded Amount") + geom_text(stat='bin', binwidth = 3500, bins = 10, breaks = seq(0,35000,by=3500), aes(label=..count..)) + scale_x_continuous(breaks = seq(0,35000,by=3500))

# Total amount committed to that loan at that point in time within 3500 to 7000 range were defaulted higher than other ranges  

# ploting box plot for funded amount
ggplot(loan_charged_off, aes(x= "", y= funded_amnt)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Funded Amount") + ggtitle("Boxplot of Funded Amount")

summary(loan_charged_off$funded_amnt) # summary

skewness(loan_charged_off$funded_amnt) # skewness

# Observations
# i. Outliers are observed, that needs to be removed
# ii. The data is skewed to the right

# 3. Analysis of funded_amt_inv

# ploting histogram for funded amount investor
ggplot(loan_charged_off, aes(x=funded_amnt_inv)) + geom_histogram(stat="bin", binwidth = 3500, bins = 10, breaks = seq(0,35000,by=3500), col="red", fill="green",alpha=0.5) + labs(title="Histogram for Funded Amount (Investor)") + geom_text(stat='bin', binwidth = 3500, bins = 10, breaks = seq(0,35000,by=3500), aes(label=..count..)) + scale_x_continuous(breaks = seq(0,35000,by=3500))

# Total amount committed by investors for that loan at that point in time within 3500 to 7000 range were defaulted higher than other ranges  

# ploting box plot for funded amount investor
ggplot(loan_charged_off, aes(x= "", y= funded_amnt_inv)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Funded Amount (Investor)") + ggtitle("Boxplot of Funded Amount (Investor)")

summary(loan_charged_off$funded_amnt_inv) # summary

skewness(loan_charged_off$funded_amnt_inv) # skewness

# Observations
# i. Outliers are observed, that needs to be removed
# ii. The data is skewed to the right

# 4. Analysis of term

# ploting bar chart for term
ggplot(loan_charged_off, aes(x=reorder(term,term,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Term") + xlab("Term") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Number of loan defualted are higher for the borrower who had number of payments as 36 months

# 5. Analysis of interest rate

# ploting histogram for interest rate
ggplot(loan_charged_off, aes(x=int_rate)) + geom_histogram(stat="bin", binwidth = 5, bins=5, breaks = seq(0,25,by=5),col="red", fill="green",alpha=0.5) + labs(title="Histogram for Interest Rate") + geom_text(stat='bin', binwidth = 5, bins=5, breaks = seq(0,25,by=5), aes(label=..count..)) + scale_x_continuous(breaks = seq(0,25,by=5))

# Number of loan defaulted was higher at the range of 10 to 15 interest rate

# ploting box plot for int_rate
ggplot(loan_charged_off, aes(x= "", y= int_rate)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Interest  Rate") + ggtitle("Boxplot of Interest Rate")

summary(loan_charged_off$int_rate) # summary

skewness(loan_charged_off$int_rate) # skewness

# Observations
# i. Few Outliers are observed, that needs to be removed
# ii. The data is skewed slightly to the right

# 6. Analysis of installment

# ploting box plot for installment
ggplot(loan_charged_off, aes(x= "", y= installment)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Installment") + ggtitle("Boxplot of Installment")

summary(loan_charged_off$installment) # summary

skewness(loan_charged_off$installment) # skewness

# Observations
# i. Many Outliers are observed, that needs to be removed
# ii. The data is skewed to the right

# 7. Analysis of grade
ggplot(loan_charged_off, aes(x=reorder(grade,grade,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Grade") + xlab("Grade") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Number of loan defaulted with LC assigned loan grade as B was higher

# 8. Analysis of Sub grade
ggplot(loan_charged_off, aes(x=reorder(sub_grade,sub_grade,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Sub Grade") + xlab("Sub Grade") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Number of loan defaulted with LC assigned loan sub grade as B was higher

# 9. Analysis on emp.length
ggplot(subset(loan_charged_off, !is.na(emp_length)), aes(x=reorder(emp_length,emp_length,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Employee Length") + xlab("Employment length") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Number of loans defaulted was higher when borrowers was having employment length  of 10 Years

# 10. Analysis on home_ownership
ggplot(subset(loan_charged_off, !is.na(home_ownership)), aes(x=reorder(home_ownership,home_ownership,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Home Ownership") + xlab("Home Ownership") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Number of loans defaulted was higher when borrower home ownership status during registration was rent

# 11. Analysis on annual_inc

# ploting box plot for annual inc
ggplot(loan_charged_off, aes(x= "", y= annual_inc)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Annual Income") + ggtitle("Boxplot of Annual Income")

summary(loan_charged_off$annual_inc) # summary

skewness(loan_charged_off$annual_inc) # skewness

# Observations
# i. Many outliers are observed, that needs to be removed
# ii. The data is skewed more to the right

# 12. Analysis on verification_status
ggplot(loan_charged_off, aes(x=reorder(verification_status,verification_status,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Verification Status") + xlab("Verification Status") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Number of loans defaulted was higher when borrowers income source was not verified

# 13. Analysis on issue_d
ggplot(loan_charged_off, aes(x=issue_d)) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Month Which the Loan was Funded") + xlab("Month_Year") + ylab("Count")

# The number of loans defaulted were higher when it was funded in the month December 2011 and increasing trend could be observed from 2009 

# plotting bar chart for issue_year
ggplot(loan_charged_off, aes(x=issue_year)) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Year Which Loan was Funded") + xlab("Year") + ylab("Count")

# The number of loans defaulted were higher when it was funded in the year 2011 and increasing trend could be observed from 2007

# plotting bar chart for issue_month
ggplot(loan_charged_off, aes(x=issue_month)) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Month Which Loan was Funded") + xlab("Month") + ylab("Count")

# The number of loans defaulted were higher when it was funded in the month of December and increasing trend could be observed from 2007

# 14. Analysis on purpose
ggplot(loan_charged_off, aes(x=reorder(purpose,purpose,function(x) length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Purpose") + xlab("Purpose") + ylab("Count") + geom_text(stat='count',aes(label=..count..)) + coord_flip()

# The number of lonas defaulted was higher when borrower provided the category for the loan request as debt consolidation 

# 15. Analysis on Addr_state
ggplot(loan_charged_off, aes(x=reorder(addr_state,addr_state,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Address State") + xlab("State") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Top 3 States of Borrowers where the number of loans defaulted was higher are CA, FL and NY 

# 15. Analysis on zipcode

# Filtering the records for the top three states
loan_charged_Off_top_3_states <- subset(loan_charged_off, loan_charged_off$addr_state == "CA"|loan_charged_off$addr_state == "FL"|loan_charged_off$addr_state == "NY")

# creating frequency dataframe for zip code and limiting it to 20.

loan_charged_off_top_20_zipcode_top_3_state <- row.names(as.data.frame(summary(loan_charged_Off_top_3_states$zip_code, max=21)))
loan_charged_off$zip_code <- as.character(loan_charged_off$zip_code)
loan_charged_off$top_20_zipcode_top_3_state <- ifelse(loan_charged_off$zip_code %in% loan_charged_off_top_20_zipcode_top_3_state,loan_charged_off$zip_code,"other")

# ploting bar chart for zipcode
ggplot(subset(loan_charged_off, top_20_zipcode_top_3_state != "other"), aes(x=reorder(top_20_zipcode_top_3_state,top_20_zipcode_top_3_state,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Zip Code") + xlab("Zip Code") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Number of loans defaulted was higher for the borrowers staying at zip code (first three numbers) of (945xx)

# 17. Analysis on dti

# ploting histogram for dti
dtip1 <- ggplot(loan_charged_off, aes(x=dti)) + geom_histogram(stat="bin", binwidth = 3, bins=10 , breaks= seq(0,30,by=3), col="red", fill="green",alpha=0.5) + labs(title="Histogram for DTI") + geom_text(stat='bin', binwidth = 3, bins=10 , breaks= seq(0,30,by=3),  aes(label =..count..)) + scale_x_continuous(breaks= seq(0,30,by=3))
dtip1

# Number of loans defaulted was higher for borrower having debt to income ratio as 12-15

# ploting box plot for dti
ggplot(loan_charged_off, aes(x= "", y= dti)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Debt to Income Ratio") + ggtitle("Boxplot of DTI")

summary(loan_charged_off$dti) # summary

skewness(loan_charged_off$dti) # skewness

# Observations
# i. No Outliers are observed
# ii. The data is skewed to the left

# 18. Analysis on earliest_cr_line

# plotting bar chart for earliest_cr_line
ggplot(loan_charged_off, aes(x=earliest_cr_line)) + geom_bar(fill = "green", col= "green", alpha=0.5) + labs(title= "Bar Chart for Month Borrower's Earliest Reported Credit Line Opened") + xlab("Month_Year") + ylab("Count")

# Number of loans defaulted was higher for borrower's having earliest reported credit line opened on  Nov 1998

# plotting bar chart for earliest_cr_line_year
ggplot(loan_charged_off, aes(x=earliest_cr_line_year)) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Year Borrower's Earliest Reported Credit Line Opened") + xlab("Year") + ylab("Count") + scale_x_discrete(breaks = seq(1946,2008,by=10))

# Number of loans defaulted was higher for borrower's having earliest reported credit line opened at year 2000

# plotting bar chart for earliest_cr_line_month
ggplot(loan_charged_off, aes(x=earliest_cr_line_month)) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Month Borrower's Earliest Reported Credit Line Opened") + xlab("Month") + ylab("Count")

# Number of loans defaulted was higher for borrower's having earliest reported credit line opened at month December

# 19. Analysis on inq_last_6mths

# plotting bar chart for inq_last_6mths
ggplot(loan_charged_off, aes(x=reorder(inq_last_6mths,inq_last_6mths,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Inquiries in Last 6 Months") + xlab("Inquiries in Last 6 Months") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Number of loans was defaulted was higher when number of inquiries in past 6 months was 0 (excluding auto and mortgage inquiries)

# 20.  Analysis on open_acc

# ploting histogram for open_acc
ggplot(loan_charged_off, aes(x=open_acc)) + geom_histogram(stat="bin", binwidth = 6, bins=6 , breaks= seq(2,38,by=6), col="red", fill="green",alpha=0.5) + labs(title="Histogram for Open Account") + geom_text(stat='bin', binwidth = 6, bins=6 , breaks= seq(2,38,by=6),  aes(label =..count..)) + scale_x_continuous(breaks= seq(2,38,by=6))

# Number of loans defaulted was higher for borrower having number of open credit lines at the range of 2 to 8.

# 21. revol_bal

# ploting box plot for revol_bal
ggplot(loan_charged_off, aes(x= "", y= revol_bal)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Revolving Balance") + ggtitle("Boxplot of Credit Revolving Balance")

summary(loan_charged_off$revol_bal) # summary

skewness(loan_charged_off$revol_bal) # skewness

# Observations
# i. Many Outliers are observed, that needs to be removed
# ii. The data is skewed more to the right

# 22. Analysis of revol_util

# ploting histogram for revol_util
ggplot(subset(loan_charged_off, !is.na(revol_util)), aes(x=revol_util)) + geom_histogram(stat="bin", binwidth = 10, bins=10, breaks = seq(0,100,by=10),col="red", fill="green",alpha=0.5) + labs(title="Histogram for Revolving Line Utilization Rate") + geom_text(stat='bin', binwidth = 10, bins=10, breaks = seq(0,100,by=10), aes(label=..count..)) + scale_x_continuous(breaks = seq(0,100,by=10))

# The number of loan defaulted was higher when the revolving utilization rate was at the range of 70 to 80

# ploting box plot for revol_util
ggplot(subset(loan_charged_off, !is.na(revol_util)), aes(x= "", y= revol_util)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Revolving Line Utilization Rate") + ggtitle("Boxplot of Revolving Line Utilization Rate")

summary(loan_charged_off$revol_util) # summary

skewness(loan_charged_off$revol_util, na.rm = T) # skewness

# Observations
# i. No Outliers are observed
# ii. The data is skewed to the left

# 23.  Analysis on total_acc

# ploting histogram for total_acc
ggplot(loan_charged_off, aes(x=total_acc)) + geom_histogram(stat="bin", binwidth = 9, bins=8 , breaks= seq(2,74,by=9), col="red", fill="green",alpha=0.5) + labs(title="Histogram for Total Number Of Credit Lines") + geom_text(stat='bin', binwidth = 9, bins=8 , breaks= seq(2,74,by=9),  aes(label =..count..)) + scale_x_continuous(breaks= seq(2,74,by=9))

# The number of loans defaulted was higher when borrowers having total number of credit lines at the range of 11 to 20.

# Correlation

# Filtering the records with numerical value
loan_numerical <- Filter(is.numeric, loan)

# creating correlation matrix
loan_numerical <- cor(loan_numerical, use="pairwise.complete.obs")

# Ploting correlation matrix
corrplot(loan_numerical, method = "square",order="FPC")

# Higher correlation couble be observed between loan_amount, funder_amount, funded_amout_invesment and Installment. Hence, all these variables would give same insight.

# Segmented univariate & Bi-variate Analysis (After Correlation)

# 1. Loan amount by defaulters

# plotting box plot for loan amount by defaulters
ggplot(loan, aes(x=defaulters, y=loan_amnt)) + geom_boxplot(aes(fill = defaulters), alpha = 0.5) + scale_y_continuous(name = "Loan Amount") + ggtitle("Boxplot of Loan Amount by Defaulters")  + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))

# Higher amount of loan are most likely to get defaulted

# 2. Term by defaulters

# plotting bar chart for term by defaulters
ggplot(loan, aes(defaulters, group=term)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3) + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = .8) + labs(y= "percent", fill = "Defaulters") + facet_grid(~term) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Term by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))

# The loan are more likely to default for term 60 years

# 3. Interest rate by defaulters

# plotting box plot for int_rate by defaulters
ggplot(loan, aes(x= defaulters, y= int_rate)) + geom_boxplot(aes(fill = defaulters), alpha = 0.5) + scale_y_continuous(name = "Interest  Rate") + ggtitle("Boxplot of Interest Rate by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))
 
# Higher interest rate are most likely to get defaulted

# 4. Grade by Defaulters

# plotting bar chart for grade by defaulters
gp <- ggplot(loan, aes(defaulters, group=grade)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3) + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = .8) + labs(y= "percent", fill = "Defaulters") + facet_wrap(~grade, ncol =4) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Grade by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))
gp

# The loan are more likely to default for the Grade G and the increasing trend could be observed on the default as the grade is moving from A to G

# 5. Sub grade by defaulters

# plotting bar chart for sub_grade by defaulters
sgp <- ggplot(loan, aes(defaulters, group=sub_grade)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3) + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = .8) + labs(y= "percent", fill = "Defaulters") + facet_wrap(~sub_grade,ncol=9) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Sub Grade by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))
sgp

# The loan are more likely to default for the Sub_Grade F5 and if we only cosider Grade G then it is G3 which is more likely to default

# 6. emp.length by defaulters

# plotting bar chart for emp_length by defaulters
elp <- ggplot(subset(loan, !is.na(emp_length)), aes(defaulters, group=emp_length)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3) + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = 0.8) + labs(y= "percent", fill = "Defaulters") + facet_wrap(~emp_length, ncol=4) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Employment Length by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))
elp

# The loan are more likely to default for the borrowers having employment length of 10 years or more

# 7. home_ownership by defaulters

# plotting bar chart for home ownership by defaulters
hop <- ggplot(subset(loan, !is.na(home_ownership)), aes(defaulters, group=home_ownership)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3)  + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = .8)+ labs(y= "percent", fill = "Defaulters") + facet_grid(~home_ownership) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Home Ownership by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))
hop

# The loan are more likely to default for the borrowers having home ownership as Other category

# 8. annual_inc by defaulters

# ploting box plot for annual inc
ggplot(loan, aes(x= defaulters, y= annual_inc)) + geom_boxplot(aes(fill = defaulters), alpha = 0.5) + scale_y_continuous(name = "Annual Income", breaks = seq(0,150000,by=25000), limits = c(0,150000)) + ggtitle("Boxplot of Annual Income by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))

# Lower Annual Income for Borrowers are most likely to make the loan get defaulted

# 9. Verification_status by defaulters

# plotting bar chart for verification_status by defaulters
ggplot(loan, aes(defaulters, group=verification_status)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3)  + labs(y= "percent", fill = "Defaulters") + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = .8) + facet_grid(~verification_status) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Verification Status by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))

# The loan are more likely to default for the borrowers whose income are verified

# 10. Purpose by Defaulters

# plotting bar chart for purpose by defaulters
ggplot(loan, aes(defaulters, group=purpose)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3) + geom_text(aes(label= ..count.., y= ..prop..),stat = "count", vjust = .8) + labs(y= "percent", fill = "Defaulters") + facet_wrap(~purpose, ncol = 7) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Purpose by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))

# The loan are more likely to default for the borrowers whose purpose is for small business

# 11. Addr_state by Defaulters

# plotting bar chart for Address State by defaulters
ggplot(loan, aes(defaulters, group=addr_state)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3)  + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = .8) + labs(y= "percent", fill = "Defaulters") + facet_wrap(~addr_state, ncol = 10) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for State by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))

# Borrower from state NE are most likely to get defaulted but the number of loans are too small to conclude

# Borrower from top 3 states with highest default and good number of default ratio are CA, FL, NY

# 12. Zip_Code by Defaulters

# Filtering the records for the top three states
loan_top_3_states <- subset(loan, loan$addr_state == "CA"|loan$addr_state == "FL"|loan$addr_state == "NY")

# creating frequency dataframe for zip code and limiting it to 20.
loan_top_20_zipcode_top_3_state <- row.names(as.data.frame(summary(loan_top_3_states$zip_code, max=21)))
loan$zip_code <- as.character(loan$zip_code)
loan$top_20_zipcode_top_3_state <- ifelse(loan$zip_code %in% loan_top_20_zipcode_top_3_state,loan$zip_code,"other")

# plotting bar chart for Zip code by defaulters
ggplot(subset(loan, top_20_zipcode_top_3_state != "other"), aes(defaulters, group=top_20_zipcode_top_3_state)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3)  + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = .8) + labs(y= "percent", fill = "Defaulters") + facet_wrap(~top_20_zipcode_top_3_state, ncol = 10) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Zip Code for Top 3 by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))

# Borrower from top 3 zip codes with highest default and good number of default ratio are 917xx, 331xx, 330xx

# 13. DTI by defaulters

# ploting box plot for dti by defaulters
dtip <- ggplot(loan, aes(x= defaulters, y= dti)) + geom_boxplot(aes(fill = defaulters), alpha = 0.5) + scale_y_continuous(name = "Debt to Income Ratio") + ggtitle("Boxplot of DTI by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))
dtip

# Borrowers with Higher debt to income ratio are most likely to make the loan get defaulted

# 14. inq_last_6mths by Defaulters

# plotting bar chart for inq_last_6mths by defaulters
ggplot(loan, aes(defaulters, group=inq_last_6mths)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3)  + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = .8) + labs(y= "percent", fill = "Defaulters") + facet_grid(~inq_last_6mths) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Inquires Last 6mths by Defaulters")  + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))

# Increasing trend of more likely to default has been observed and 7 inquries in last 6 months are more likely to get defaulted 

# 15.  Analysis on open_acc

# creating bin
loan$open_acc_bin <- ifelse(loan$open_acc <= 8, "1. 2-8",
                     ifelse(loan$open_acc <= 14, "2. 9-14",
                     ifelse(loan$open_acc <= 20, "3. 15-20",
                     ifelse(loan$open_acc <= 26, "4. 21-26",
                     ifelse(loan$open_acc <= 32, "5. 27-32", 
                     ifelse(loan$open_acc <= 38, "6. 33-38","7. 39-44"))))))

ggplot(loan, aes(defaulters, group=open_acc_bin)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3)  + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = .8) + labs(y= "percent", fill = "Defaulters") + facet_grid(~open_acc_bin) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Open Account by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))

# Borrowers with 33-38 accounts opened are most likely to make the loan get defaulted followed by borrowers with 27-32 accounts opened

# 16. revol_bal by defaulters

# ploting box plot for revol_bal
rbp1 <- ggplot(loan, aes(x= defaulters, y= revol_bal)) + geom_boxplot(aes(fill = defaulters), alpha = 0.5) + scale_y_continuous(name = "Revolving Balance") + ggtitle("Boxplot of Credit Revolving Balance by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))
rbp1

# Higher the revolving balance are more likely to get defaulted

# 17. revol_util by defaulters

# ploting box plot for revol_uti by defaulters
rup1 <- ggplot(subset(loan, !is.na(revol_util)), aes(x= defaulters, y= revol_util)) + geom_boxplot(aes(fill = defaulters), alpha = 0.5) + scale_y_continuous(name = "Revolving Line Utilization Rate") + ggtitle("Boxplot of Revolving Line Utilization Rate by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))
rup1
# Borrowes with Higher utilization rate are more likely to get defaulted

# 18.  total_acc by defaulters

# creating bin
loan$total_acc_bin <- ifelse(loan$total_acc <= 10, "1. 2-10",
                      ifelse(loan$total_acc <= 18, "2. 11-18",
                      ifelse(loan$total_acc <= 26, "3. 19-26",
                      ifelse(loan$total_acc <= 34, "4. 27-34",
                      ifelse(loan$total_acc <= 42, "5. 35-42", 
                      ifelse(loan$total_acc <= 50, "6. 43-50",
                      ifelse(loan$total_acc <= 58, "7. 51-58",
                      ifelse(loan$total_acc <= 66, "8. 59-66",
                      ifelse(loan$total_acc <= 74, "9. 67-74",
                      ifelse(loan$total_acc <= 82, "10. 75-82", "11. 83-90"))))))))))

ggplot(loan, aes(defaulters, group=total_acc_bin)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") + geom_text(aes(label = scales::percent(..prop..), y= ..prop..),stat = "count", vjust = -.3)  + geom_text(aes(label = ..count.., y= ..prop..),stat = "count", vjust = .8) + labs(y= "percent", fill = "Defaulters") + facet_grid(~total_acc_bin) + scale_y_continuous(labels = scales::percent) + ggtitle("Bar Chart for Total Account by Defaulters") + scale_fill_discrete(labels=c("0 = Non-Defaulter","1 = Defaulter"))

# Borrowers with 67-74 total accounts are most likely to make the loan get defaulted followed by borrowers with 2-10 accounts opened

# Creating a dashboard for pasting 
grid.arrange(gp,sgp,elp,hop,ncol =2, nrow=2)
grid.arrange(dtip,dtip1,rbp1,rup1,ncol=2,nrow=2)