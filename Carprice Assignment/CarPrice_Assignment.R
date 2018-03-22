# Installing and loading the packages to perform modelling

# install.packages("MASS") for StepAIC
library(MASS)

# install.packages("car") for VIF
library(car)

# install.packages("stringr") for text processing
library(stringr)

# install.packages("ggplot2") for plots
library(ggplot2)

# install.packages("e1071") for skewness
library(e1071)

# install.packages("gridExtra") for plots
library(gridExtra)

####### Business Understanding #############

# A Chinese automobile company Geely Auto aspires to enter the US market by setting up their 
# manufacturing unit there and producing cars locally to give competition to their US and European counterparts. 

# They have contracted an automobile consulting company to understand the factors on which the pricing of a car depends.


####### Problem Statement ############

# They want to understand the factors affecting the pricing of cars in the American marketing, 
# since those may be very different from the Chinese market.
# Essentially, the company wants to know:
# 1. Which variables are significant in predicting the price of a car
# 2. How well those variables describe the price of a car

######### Goals of Assignement #############

# To model the price of cars with the available independent variables. 
# It will be used by the management to understand how exactly the prices vary with the independent variables. 
# They can accordingly manipulate the design of the cars, the business strategy etc. to meet certain price levels. 
# Further, the model will be a good way for the management to understand the pricing dynamics of a new market.

######### Data Understanding & Data Preparation #############

# loading the dataset 
carprice <- read.csv("CarPrice_Assignment.csv", header = T)

# Viewing carprice dataset
View(carprice)

# Structure of Dataset
str(carprice)

#  NA Values
sum(is.na(carprice)) # 0 na values observed

# Blank Values
sapply(carprice, function(x) length(which(x == ""))) # checking for blank "" values; there are none

# find variables having same one  unique value
carprice[sapply(carprice, function(x) length(unique(x))==1)] # 0 variables having same unique value

######## Checking each variable separately ##########

## Integer Variable

# 1. Car_ID
summary(carprice$car_ID)

# i. Looking for duplicate value
sum(duplicated(carprice$car_ID)) # No duplicate ID's here

# ii. As each row is unique it would not be helpful for analysis, but it is unique and could be used for merging or joining with other dataset
carprice$car_ID <- NULL

## B. Categorical Variable
## This is only to check and fix the data quality issues. Dummy variables are created after the univariate and bivariate analysis

# Symboling 		
# carCompany		
# fueltype		
# aspiration		
# doornumber		
# carbody		
# drivewheel		
# enginelocation		
# enginetype		
# cylindernumber		
# fuelsystem		

# 1. Symboling
summary(carprice$symboling)

# i. changing to factor
carprice$symboling <- as.factor(carprice$symboling)
summary(carprice$symboling) # now okay

# 2. Car Name
summary(carprice$CarName)

# i. Extracting Car Company Name
carprice$car_company_name <- word(carprice$CarName,1,sep = " ")

# ii. Removing the Car Name Variable as a new variable Car company name would be used
carprice$CarName <- NULL

# iii. summary car_company_name
summary(factor(carprice$car_company_name)) # casing issue, same company with multiple names issues could be observed

# iv. Changing the casing to lower case
carprice$car_company_name <- tolower(carprice$car_company_name)

# v. summary car_company_name
summary(factor(carprice$car_company_name)) # same company with multiple names issues could be observed
# alfa-romero to be changed to alfa-romeo
# maxda to be changed to mazda
# procshce to be changed to porsche
# toyouta to be changed to toyota
# vokswagen and vw to be changed to volkswagen

# vi. changing
carprice$car_company_name[which(carprice$car_company_name == "alfa-romero")] <- "alfa-romeo"
carprice$car_company_name[which(carprice$car_company_name == "maxda")] <- "mazda"
carprice$car_company_name[which(carprice$car_company_name == "porcshce")] <- "porsche"
carprice$car_company_name[which(carprice$car_company_name == "toyouta")] <- "toyota"
carprice$car_company_name[which(carprice$car_company_name == "vokswagen" | carprice$car_company_name == "vw")] <- "volkswagen"

# vii. summary car_company_name
summary(factor(carprice$car_company_name)) # seems okay now

# viii. creating origin of cars
european_cars <- c("alfa-romeo","audi","bmw","jaguar","peugeot","porsche","renault","saab","volkswagen","volvo")
american_cars <- c("buick","chevrolet","dodge","mercury","plymouth")
asian_cars <- c("honda","isuzu","mazda","mitsubishi","nissan","subaru","toyota")

carprice$origin <- as.factor(ifelse(carprice$car_company_name %in% european_cars, "european", 
                         ifelse(carprice$car_company_name %in% american_cars, "american","asian")))

# 3. fueltype
summary(carprice$fueltype) # seems okay

# 4. aspiration
summary(carprice$aspiration) # seems okay

# 5. doornumber
summary(carprice$doornumber) # seems okay

# 6. carbody
summary(carprice$carbody) # seems okay

# 7. drivewheel
summary(carprice$drivewheel) # seems okay

# 8. enginelocation
summary(carprice$enginelocation) # seems okay

# 9. enginetype
summary(carprice$enginetype) # seems okay

# 10. cylindernumber
summary(carprice$cylindernumber) # seems okay

# 11. fuelsystem
summary(carprice$fuelsystem) # seems okay

## C. Numeric Variable
# This is to check and fix the data type quality issues, outliers are checked and performed at the univariate analysis below

# wheelbase
# carlength
# carwidth
# carheight
# curbweight
# enginesize
# boreratio
# stroke
# compressionratio
# horsepower
# peakrpm
# citympg
# highwaympg

# 1. Wheelbase
summary(carprice$wheelbase) # seems okay

# 2. carlength
summary(carprice$carlength) # seems okay

# 3. carwidth
summary(carprice$carwidth) # seems okay

# 4. carheight
summary(carprice$carheight) # seems okay

# 5. curbweight
summary(carprice$curbweight)

#i. Changing to numeric
carprice$curbweight <- as.numeric(carprice$curbweight) # seems okay

# 6. enginesize
summary(carprice$enginesize) # seems okay

# 7. boreratio
summary(carprice$boreratio) # seems okay

# 8. stroke
summary(carprice$stroke) # seems okay

# 9. compressionratio
summary(carprice$compressionratio) # seems okay

# 10. horsepower
summary(carprice$horsepower) # seems okay

# 11. peakrpm
summary(carprice$peakrpm) # seems okay

# 12. citympg
summary(carprice$citympg) # seems okay

# 13. highwaympg
summary(carprice$highwaympg) # seems okay

# 14.price
summary(carprice$price) # seems okay

##### univariate analysis #####

# 1. symboling

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(symboling,symboling,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Symboling") + xlab("Symboling") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Many cars falls under the medium range (symboling as 0,1) followed by risky and safe

# ii. creating new levels for symboling
levels(carprice$symboling)[1:2] <- "safe" # considerinf symboling of -2,-1 as safe
levels(carprice$symboling)[2:3] <- "medium" # considerinf symboling of 0,1 as medium
levels(carprice$symboling)[3:4] <- "risky" # considerinf symboling of 2,3 as risky

# Plotting a bar chart after the new levels created
ggplot(carprice, aes(x=reorder(symboling,symboling,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Symboling") + xlab("Symboling") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# 2. car_company_name

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(car_company_name,car_company_name,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Car Company Name") + xlab("Car Company Name") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars belongs to toyato company in the data set

# 3. fueltype

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(fueltype,fueltype,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Fuel Type") + xlab("Fuel Type") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars fuel type is gas

# 4. aspiration

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(aspiration,aspiration,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Aspiration") + xlab("Aspiration") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars aspiration is standard

# 5. doornumber

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(doornumber,doornumber,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Door Number") + xlab("Door Number") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars has four doors

# 6. carbody

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(carbody,carbody,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Car Body") + xlab("Car Body") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars are sedan body type

# 7. drivewheel

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(drivewheel,drivewheel,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Drive Wheel") + xlab("Drive Wheel") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars has front wheel drive

# 8. enginelocation

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(enginelocation,enginelocation,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Engine Location") + xlab("Drive Wheel") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars has front engine location

# 9. enginetype

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(enginetype,enginetype,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Engine Type") + xlab("Engine Type") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars has ohc engine type

# 10. cylindernumber

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(cylindernumber,cylindernumber,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Cylinder Number") + xlab("Cylinder Number") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars has four cylinders
# i. creating new levels for cylindernumber
levels(carprice$cylindernumber)[c(7,5)]<- "low"# Cylinder number with two and three classified as low
levels(carprice$cylindernumber)[c(3,2,4)]<- "medium"# Cylinder number with three, four and five classified as medium
levels(carprice$cylindernumber)[c(1,4)]<- "high"# Cylinder number with eight and twelve classified as high

# Plotting a bar chart after creating new levels
ggplot(carprice, aes(x=reorder(cylindernumber,cylindernumber,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Cylinder Number") + xlab("Cylinder Number") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# 11. fuelsystem

# Plotting a bar chart
 ggplot(carprice, aes(x=reorder(fuelsystem,fuelsystem,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Fuel System") + xlab("Fuel System") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars has four cylinders

# 12. origin

# Plotting a bar chart
ggplot(carprice, aes(x=reorder(origin,origin,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=0.5) + labs(title= "Bar Chart for Car Origin") + xlab("Car Origin") + ylab("Count") + geom_text(stat='count',aes(label=..count..))

# Most of the cars are from asian

## Numerical Variables

# outlier treatment function by capping

outlier_adjustment <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  x
}

# 1. Wheelbase

# i. Checking outliers
ggplot(carprice, aes(x= "", y= wheelbase)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Wheel Base") + ggtitle("Boxplot of Wheel Base")
# outliers observed

# ii . capping the outliers
carprice$wheelbase <- outlier_adjustment(carprice$wheelbase)

# iii. re-checking outliers
ggplot(carprice, aes(x= "", y= wheelbase)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Wheel Base") + ggtitle("Boxplot of Wheel Base")
# outliers fixed

skewness(carprice$wheelbase) # skewness towards right

# 2. carlength

# i. Checking outliers
ggplot(carprice, aes(x= "", y= carlength)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Car Length") + ggtitle("Boxplot of Car Length")
# outliers observed

# ii . capping the outliers
carprice$carlength <- outlier_adjustment(carprice$carlength)

# iii. re-checking outliers
ggplot(carprice, aes(x= "", y= carlength)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Car Length") + ggtitle("Boxplot of Car Length")
# outliers fixed

skewness(carprice$carlength) # skewness towards right

# 3. carwidth

# i. Checking outliers
ggplot(carprice, aes(x= "", y= carwidth)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Car Width") + ggtitle("Boxplot of Car Width")
# outliers observed

# ii . capping the outliers
carprice$carwidth <- outlier_adjustment(carprice$carwidth)

# iii. re-checking outliers
ggplot(carprice, aes(x= "", y= carwidth)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Car Width") + ggtitle("Boxplot of Car Width")
# outliers fixed

skewness(carprice$carwidth) # skewness towards right

# 4. carheight

# i. Checking outliers
ggplot(carprice, aes(x= "", y= carheight)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Car Height") + ggtitle("Boxplot of Car Height")
# no outliers observed

skewness(carprice$carheight) # skewness slightly towards right

# 5. curbweight

# i. Checking outliers
ggplot(carprice, aes(x= "", y= curbweight)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Curb Weight") + ggtitle("Boxplot of Curb Weight")
# no outliers observed

skewness(carprice$curbweight) # skewness towards right

# 6. enginesize

# i. Checking outliers
ggplot(carprice, aes(x= "", y= enginesize)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Engine Size") + ggtitle("Boxplot of Engine Size")
# outliers observed

# ii . capping the outliers
carprice$enginesize <- outlier_adjustment(carprice$enginesize)

# iii. re-checking outliers
ggplot(carprice, aes(x= "", y= enginesize)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Engine Size") + ggtitle("Boxplot of Engine Size")
# outliers fixed

skewness(carprice$enginesize) # skewness towards right

# 7. boreratio

# i. Checking outliers
ggplot(carprice, aes(x= "", y= boreratio)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Bore Ratio") + ggtitle("Boxplot of Bore Ratio")
# no outliers observed

skewness(carprice$boreratio) # skewness slightly towards right

# 8. stroke

# i. Checking outliers
ggplot(carprice, aes(x= "", y= stroke)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Stroke") + ggtitle("Boxplot of Stroke")
# outliers observed

# ii . capping the outliers
carprice$stroke <- outlier_adjustment(carprice$stroke)

# iii. re-checking outliers
ggplot(carprice, aes(x= "", y= stroke)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Stroke") + ggtitle("Boxplot of Stroke")
# most of outliers fixed

skewness(carprice$stroke) # skewness slightly towards left

# 9. compressionratio

# i. Checking outliers
ggplot(carprice, aes(x= "", y= compressionratio)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Compression Ratio") + ggtitle("Boxplot of Compression Ratio")
# outliers observed

# ii . capping the outliers
carprice$compressionratio <- outlier_adjustment(carprice$compressionratio)

# iii. re-checking outliers
ggplot(carprice, aes(x= "", y= compressionratio)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Compression Ratio") + ggtitle("Boxplot of Compression Ratio")
# most of outliers fixed

skewness(carprice$compressionratio) # skewness highly towards right

# 10. horsepower

# i. Checking outliers
ggplot(carprice, aes(x= "", y= horsepower)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Horse Power") + ggtitle("Boxplot of Horse Power")
# outliers observed

# ii . capping the outliers
carprice$horsepower <- outlier_adjustment(carprice$horsepower)

# iii. re-checking outliers
ggplot(carprice, aes(x= "", y= horsepower)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Horse Power") + ggtitle("Boxplot of Horse Power")
# outliers fixed

skewness(carprice$horsepower) # skewness highly towards right

# 11. peakrpm

# i. Checking outliers
ggplot(carprice, aes(x= "", y= peakrpm)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Peak RPM") + ggtitle("Boxplot of Peak RPM")
# outliers observed

# ii . capping the outliers
carprice$peakrpm <- outlier_adjustment(carprice$peakrpm)

# iii. re-checking outliers
ggplot(carprice, aes(x= "", y= peakrpm)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Peak RPM") + ggtitle("Boxplot of Peak RPM")
# outliers fixed

skewness(carprice$peakrpm) # skewness towards left

# 12. citympg

# i. Checking outliers
ggplot(carprice, aes(x= "", y= citympg)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "City MPG") + ggtitle("Boxplot of City MPG")
# outliers observed

# ii . capping the outliers
carprice$citympg <- outlier_adjustment(carprice$citympg)

# iii. re-checking outliers
ggplot(carprice, aes(x= "", y= citympg)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "City MPG") + ggtitle("Boxplot of City MPG")
# outliers fixed

skewness(carprice$citympg) # skewness towards right

# 13. highwaympg

# i. Checking outliers
ggplot(carprice, aes(x= "", y= highwaympg)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Highway MPG") + ggtitle("Boxplot of Highway MPG")
# outliers observed

# ii . capping the outliers
carprice$highwaympg <- outlier_adjustment(carprice$highwaympg)

# iii. re-checking outliers
ggplot(carprice, aes(x= "", y= highwaympg)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Highway MPG") + ggtitle("Boxplot of Highway MPG")
# outliers fixed

skewness(carprice$highwaympg) # skewness towards right

# 14. price

# i. Checking outliers
ggplot(carprice, aes(x= "", y= price)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "Price") + ggtitle("Boxplot of Price")
# outliers observed but it is dependent variable for which modifying the values that are required to learn and predict would require business understanding.
# Hence, outliers are not treated for price variable.

####### Segmented Variate #############

# 1. Price by Symboling

# Ploting a boxplot
ggplot(carprice, aes(x=symboling, y=price)) + geom_boxplot(aes(fill = symboling), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Symboling") + ggtitle("Boxplot of Price by Symboling")

# Cars with safe symboling are considered to have higher price but the number of observations are small

# 2. Price by Car Company Name

# Ploting a boxplot
ggplot(carprice, aes(x=car_company_name, y=price)) + geom_boxplot(aes(fill = car_company_name), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Car Company Name") + ggtitle("Boxplot of Price by Car Company Name")

# Cars of buick company are considered to have higher price but number of observations are small

# 3. Price by Fuel type

# Ploting a boxplot
ggplot(carprice, aes(x=fueltype, y=price)) + geom_boxplot(aes(fill = fueltype), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Fuel Type") + ggtitle("Boxplot of Price by Fuel Type")

# Cars with diesel fuel types are considered to have higher price but the number of observations are small

# 4. Price by Aspiration

# Ploting a boxplot
ggplot(carprice, aes(x=aspiration, y=price)) + geom_boxplot(aes(fill = aspiration), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Aspiration") + ggtitle("Boxplot of Price by Aspiration")

# Cars with turbo aspiration are considered to have higher price but the number of observations are small

# 5. Price by Door Number

# Ploting a boxplot
ggplot(carprice, aes(x=doornumber, y=price)) + geom_boxplot(aes(fill = doornumber), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Door Number") + ggtitle("Boxplot of Price by Door Number")

# Cars with four doors are considered to have higher price

# 6. Price by Car Body

# Ploting a boxplot
ggplot(carprice, aes(x=carbody, y=price)) + geom_boxplot(aes(fill = carbody), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Car Body") + ggtitle("Boxplot of Price by Car Body")

# Cars with hardtop are considered to have higher price but the number of observations are small

# 7. Price by Drive Wheel

# Ploting a boxplot
ggplot(carprice, aes(x=drivewheel, y=price)) + geom_boxplot(aes(fill = drivewheel), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Drive Wheel") + ggtitle("Boxplot of Price by Drive Wheel")

# Cars with drive wheel of rwd are considered to have higher price

# 8. Price by Engine Location

# Ploting a boxplot
ggplot(carprice, aes(x=enginelocation, y=price)) + geom_boxplot(aes(fill = enginelocation), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Engine Location") + ggtitle("Boxplot of Price by Engine Location")

# Cars with rear engine location are considered to have higher price but the number of observations are small

# 9. Price by Engine Type

# Ploting a boxplot
ggplot(carprice, aes(x=enginetype, y=price)) + geom_boxplot(aes(fill = enginetype), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Engine Type") + ggtitle("Boxplot of Price by Engine Type")

# Cars with ohcv engine type are considered to have higher price but the number of observations are small

# 10. Price by Cylinder Number

# Ploting a boxplot
ggplot(carprice, aes(x=cylindernumber, y=price)) + geom_boxplot(aes(fill = cylindernumber), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Cylinder Number") + ggtitle("Boxplot of Price by Cylinder Number")

# Cars with high cylinders are considered to have higher price but the number of observations are small

# 11. Price by Fuel System

# Ploting a boxplot
ggplot(carprice, aes(x=fuelsystem, y=price)) + geom_boxplot(aes(fill = fuelsystem), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Fuel System") + ggtitle("Boxplot of Price by Fuel System")

# Cars with mpfi and idi are considered to have higher price

# 11. Price by Origin

# Ploting a boxplot
ggplot(carprice, aes(x=origin, y=price)) + geom_boxplot(aes(fill = origin), alpha = 0.5) + scale_y_continuous(name = "Price") + scale_x_discrete(name = "Cars Origin") + ggtitle("Boxplot of Price by Cars Origin")

# Cars with american origin are considered to have higher price

###### Creating dummy variables for categorical variables ##############
 
# 1. Symboling
# i. creating dummy variables for symboling
dummy_1 <- data.frame(model.matrix( ~symboling, data = carprice))

# ii. This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "symboling". 
dummy_1 <- dummy_1[,-1]

# iii. Combine the dummy variables to the main data set, after removing the original categorical "symboling" column
carprice <- cbind(carprice[,-1], dummy_1)
View(carprice)

# 2. Car Name
# i. creating dummy variables for car_company_name
dummy_2 <- data.frame(model.matrix( ~car_company_name, data = carprice))

# ii. This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "car_company_name". 
dummy_2 <- dummy_2[,-1]

# iii. Combine the dummy variables to the main data set, after removing the original categorical "car_company_name" column
carprice <- cbind(carprice[,-24], dummy_2)
View(carprice)

# 3. fueltype
# i. One simple way to convert fueltype variable to numeric is to replace the levels- diesel and gas with 0 and 1 is:
levels(carprice$fueltype)<-c(0,1)

# ii. Now store the numeric values in the same variable
carprice$fueltype<- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

# 4. aspiration
# i. One simple way to convert aspiration variable to numeric is to replace the levels- std and turbo with 1 and 0 is:
levels(carprice$aspiration)<-c(1,0)

# ii. Now store the numeric values in the same variable
carprice$aspiration<- as.numeric(levels(carprice$aspiration))[carprice$aspiration]

# 5. doornumber
# i. One simple way to convert doornumber variable to numeric is to replace the levels- four and two with 1 and 0 is:
levels(carprice$doornumber)<-c(1,0)

# ii. Now store the numeric values in the same variable
carprice$doornumber<- as.numeric(levels(carprice$doornumber))[carprice$doornumber]

# 6. carbody
# i. creating dummy variables for carbody
dummy_3 <- data.frame(model.matrix( ~carbody, data = carprice))

# ii. This column should be removed from the newly created dummy_3 dataframe containing the dummy values for the variable "carbody". 
dummy_3 <- dummy_3[,-1]

# iii. Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
carprice <- cbind(carprice[,-4], dummy_3)
View(carprice)

# 7. drivewheel
# i. creating dummy variables for drivewheel
dummy_4 <- data.frame(model.matrix( ~drivewheel, data = carprice))

# ii. This column should be removed from the newly created dummy_4 dataframe containing the dummy values for the variable "drivewheel". 
dummy_4 <- dummy_4[,-1]

# iii. Combine the dummy variables to the main data set, after removing the original categorical "drivewheel" column
carprice <- cbind(carprice[,-4], dummy_4)
View(carprice)

# 8. enginelocation
# i. One simple way to convert enginelocation variable to numeric is to replace the levels- front and rear with 1 and 0 is:
levels(carprice$enginelocation)<-c(1,0)

# ii. Now store the numeric values in the same variable
carprice$enginelocation<- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

# 9. enginetype
# i. creating dummy variables for enginetype
dummy_5 <- data.frame(model.matrix( ~enginetype, data = carprice))

# ii. This column should be removed from the newly created dummy_5 dataframe containing the dummy values for the variable "enginetype". 
dummy_5 <- dummy_5[,-1]

# iii. Combine the dummy variables to the main data set, after removing the original categorical "enginetype" column
carprice <- cbind(carprice[,-10], dummy_5)
View(carprice)

# 10. cylindernumber
# i. creating dummy variables for cylindernumber
dummy_6 <- data.frame(model.matrix( ~cylindernumber, data = carprice))

# ii. This column should be removed from the newly created dummy_6 dataframe containing the dummy values for the variable "cylindernumber". 
dummy_6 <- dummy_6[,-1]

# iv. Combine the dummy variables to the main data set, after removing the original categorical "cylindernumber" column
carprice <- cbind(carprice[,-10], dummy_6)
View(carprice)

# 11. fuelsystem
# i. creating dummy variables for fuelsystem
dummy_7 <- data.frame(model.matrix( ~fuelsystem, data = carprice))

# ii. This column should be removed from the newly created dummy_7 dataframe containing the dummy values for the variable "fuelsystem". 
dummy_7 <- dummy_7[,-1]

# iii. Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
carprice <- cbind(carprice[,-11], dummy_7)
View(carprice)

# 12. origin
# i. creating dummy variables for origin
dummy_8 <- data.frame(model.matrix( ~origin, data = carprice))

# ii. This column should be removed from the newly created dummy_8 dataframe containing the dummy values for the variable "origin". 
dummy_8 <- dummy_8[,-1]

# iii. Combine the dummy variables to the main data set, after removing the original categorical "origin" column
carprice <- cbind(carprice[,-19], dummy_8)
View(carprice)

############## Bi-Variate Analysis ###############
## Correlation

# creating correlation matrix
carprice_correlation <- as.data.frame(round(cor(carprice),2))
View(carprice_correlation)

############### Model Building #####################

# Model Creation

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carprice), 0.7*nrow(carprice))

# generate the train data set
train = carprice[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = carprice[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)
# R-squared = 0.9761
# Adjusted R-squared = 0.9615

# Now, lets see how to use stepAIC

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 

# We have a total of 54 variables considered into the model 
#Now let;s run the code. 

step <- stepAIC(model_1, direction="both")
#Great, so many iterations have been done through the stepwise command. 

# now we need to know our model equation so lets write the Step command here. 
step

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2
# You can notice that stepAIC removed variables - stroke, fuelsystem2bbl, boreratio, enginetypeohcv
# drivewheelrwd, fuelsystemmfi, peakrpm, compressionratio, fueltype, fuelsystemidi,fuelsystemspdi
# carheight, car_company_namevolvo, car_company_nameporsche, enginetypedohcv, citympg, highwaympg
# enginesize, doornumber, carlength, car_company_nameaudi, fuelsystemmpfi

# Let's execute this model here, 
model_2 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + horsepower + symbolingmedium + symbolingrisky + 
                car_company_namebmw + car_company_namebuick + car_company_namechevrolet + 
                car_company_namedodge + car_company_namehonda + car_company_nameisuzu + 
                car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
                car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
                car_company_nameplymouth + car_company_namerenault + car_company_namesaab + 
                car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetypeohc + enginetyperotor + cylindernumbermedium, 
              data = train)

# Let us look at the summary of the model
summary(model_2)
# R-squared = 0.9748
# Adjusted R-squared = 0.9668
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
# If the VIF is above 2, it is considered as high 
vif(model_2)

# removing horsepower as it has High VIF (>2) (11.274452) and no star

# Let's execute this model here, 
model_3<- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               carwidth + curbweight + symbolingmedium + symbolingrisky + 
               car_company_namebmw + car_company_namebuick + car_company_namechevrolet + 
               car_company_namedodge + car_company_namehonda + car_company_nameisuzu + 
               car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
               car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
               car_company_nameplymouth + car_company_namerenault + car_company_namesaab + 
               car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelfwd + enginetypeohc + enginetyperotor + cylindernumbermedium, 
             data = train)
  
# Let us look at the summary of the model
summary(model_3)
# R-squared = 0.9743
# Adjusted R-squared = 0.9665
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_3)

# removing carbodyhatchback as it has High VIF (>2) (13.932546) and no star

# Let's execute this model here, 
model_4<- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               carwidth + curbweight + symbolingmedium + symbolingrisky + 
               car_company_namebmw + car_company_namebuick + car_company_namechevrolet + 
               car_company_namedodge + car_company_namehonda + car_company_nameisuzu + 
               car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
               car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
               car_company_nameplymouth + car_company_namerenault + car_company_namesaab + 
               car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
               carbodyhardtop + carbodysedan + carbodywagon + 
               drivewheelfwd + enginetypeohc + enginetyperotor + cylindernumbermedium, 
             data = train)
  
# Let us look at the summary of the model
summary(model_4)
# R-squared = 0.9734
# Adjusted R-squared = 0.9656
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_4)

# removing wheelbase as it has High VIF (>2) (11.426875) and no star

# Let's execute this model here, 
model_5<- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + symbolingmedium + symbolingrisky + 
               car_company_namebmw + car_company_namebuick + car_company_namechevrolet + 
               car_company_namedodge + car_company_namehonda + car_company_nameisuzu + 
               car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
               car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
               car_company_nameplymouth + car_company_namerenault + car_company_namesaab + 
               car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
               carbodyhardtop + carbodysedan + carbodywagon + 
               drivewheelfwd + enginetypeohc + enginetyperotor + cylindernumbermedium, 
             data = train)
  
# Let us look at the summary of the model
summary(model_5)
# R-squared = 0.9724
# Adjusted R-squared = 0.9647
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_5)

# removing symbolingrisky as it has High VIF (>2) (5.467351) and no star

# Let's execute this model here, 
model_6<- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + symbolingmedium + 
               car_company_namebmw + car_company_namebuick + car_company_namechevrolet + 
               car_company_namedodge + car_company_namehonda + car_company_nameisuzu + 
               car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
               car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
               car_company_nameplymouth + car_company_namerenault + car_company_namesaab + 
               car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
               carbodyhardtop + carbodysedan + carbodywagon + 
               drivewheelfwd + enginetypeohc + enginetyperotor + cylindernumbermedium, 
             data = train)
  
# Let us look at the summary of the model
summary(model_6)
# R-squared = 0.9724
# Adjusted R-squared = 0.965
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_6)

# removing drivewheelfwd as it has High VIF (>2) (5.279289) and no star

# Let's execute this model here, 
model_7<- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + symbolingmedium + 
               car_company_namebmw + car_company_namebuick + car_company_namechevrolet + 
               car_company_namedodge + car_company_namehonda + car_company_nameisuzu + 
               car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
               car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
               car_company_nameplymouth + car_company_namerenault + car_company_namesaab + 
               car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
               carbodyhardtop + carbodysedan + carbodywagon + 
               enginetypeohc + enginetyperotor + cylindernumbermedium, 
             data = train)
  
# Let us look at the summary of the model
summary(model_7)
# R-squared = 0.9715
# Adjusted R-squared = 0.9642
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_7)

## All the variables with High VIF (>2) and p value as no star has been removed
## There are no variables with high VIF (>2) and p value has one star to be removed
## Hence moving to the variables with high VIF (>2) and p value has two star for removing

# removing car_company_namehonda as it has High VIF (>2) (2.468065) and having 2 star significance

# Let's execute this model here, 
model_8<- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + symbolingmedium + 
               car_company_namebmw + car_company_namebuick + car_company_namechevrolet + 
               car_company_namedodge + car_company_nameisuzu + 
               car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
               car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
               car_company_nameplymouth + car_company_namerenault + car_company_namesaab + 
               car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
               carbodyhardtop + carbodysedan + carbodywagon + 
               enginetypeohc + enginetyperotor + cylindernumbermedium, 
             data = train)
  
# Let us look at the summary of the model
summary(model_8)
# R-squared = 0.9694
# Adjusted R-squared = 0.9618
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_8)

# There are no more variables which has high VIF and less than 3 star significance
# Hence removing the variable based on the p-value

# removing carbodysedan with high p value of 0.939831 and no star

# Let's execute this model here, 
model_9<- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + symbolingmedium + 
               car_company_namebmw + car_company_namebuick + car_company_namechevrolet + 
               car_company_namedodge + car_company_nameisuzu + 
               car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
               car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
               car_company_nameplymouth + car_company_namerenault + car_company_namesaab + 
               car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
               carbodyhardtop + carbodywagon + enginetypeohc + enginetyperotor + 
               cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_9)
# R-squared = 0.9694
# Adjusted R-squared = 0.9622
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_9)

# removing car_company_namesaab with high p value of 0.919718 and no star

# Let's execute this model here, 
model_10<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + symbolingmedium + 
                car_company_namebmw + car_company_namebuick + car_company_namechevrolet + 
                car_company_namedodge + car_company_nameisuzu + 
                car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
                car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
                car_company_nameplymouth + car_company_namerenault + 
                car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
                carbodyhardtop + carbodywagon + enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_10)
# R-squared = 0.9694
# Adjusted R-squared = 0.9625
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_10)

# removing symbolingmedium with high p value of 0.662638 and no star

# Let's execute this model here, 
model_11<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namedodge + car_company_nameisuzu + 
                car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
                car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
                car_company_nameplymouth + car_company_namerenault + 
                car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
                carbodyhardtop + carbodywagon + enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_11)
# R-squared = 0.9693
# Adjusted R-squared = 0.9628
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_11)

# removing carbodyhardtop with high p value of 0.659976 and no star

# Let's execute this model here, 
model_12<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namedodge + car_company_nameisuzu + 
                car_company_namejaguar + car_company_namemazda + car_company_namemercury + 
                car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
                car_company_nameplymouth + car_company_namerenault + 
                car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
                carbodywagon + enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_12)
# R-squared = 0.9693
# Adjusted R-squared = 0.963
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_12)

# removing car_company_namemercury with high p value of 0.368808 and no star

# Let's execute this model here, 
model_13<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namedodge + car_company_nameisuzu + 
                car_company_namejaguar + car_company_namemazda + 
                car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
                car_company_nameplymouth + car_company_namerenault + 
                car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
                carbodywagon + enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_13)
# R-squared = 0.969
# Adjusted R-squared = 0.9631
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_13)

# removing car_company_nameisuzu with high p value of 0.349916 and no star

# Let's execute this model here, 
model_14<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namedodge + 
                car_company_namejaguar + car_company_namemazda + 
                car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
                car_company_nameplymouth + car_company_namerenault + 
                car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
                carbodywagon + enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_14)
# R-squared = 0.9688
# Adjusted R-squared = 0.9631
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_14)

# removing carbodywagon with high p value of 0.073125 and no star

# Let's execute this model here, 
model_15<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namedodge + 
                car_company_namejaguar + car_company_namemazda + 
                car_company_namemitsubishi + car_company_namenissan + car_company_namepeugeot + 
                car_company_nameplymouth + car_company_namerenault + 
                car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
                enginetypeohc + enginetyperotor + cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_15)
# R-squared = 0.968
# Adjusted R-squared = 0.9624
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_15)

# removing car_company_namenissan with high p value of 0.05152 and no star

# Let's execute this model here, 
model_16<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namedodge + 
                car_company_namejaguar + car_company_namemazda + 
                car_company_namemitsubishi + car_company_namepeugeot + 
                car_company_nameplymouth + car_company_namerenault + 
                car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
                enginetypeohc + enginetyperotor + cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_16)
# R-squared = 0.9669
# Adjusted R-squared = 0.9615
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_16)

# removing car_company_namedodge with high p value of 0.025638 and one star

# Let's execute this model here, 
model_17<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namejaguar + car_company_namemazda + 
                car_company_namemitsubishi + car_company_namepeugeot + 
                car_company_nameplymouth + car_company_namerenault + 
                car_company_namesubaru + car_company_nametoyota + car_company_namevolkswagen + 
                enginetypeohc + enginetyperotor + cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_17)
# R-squared = 0.9656
# Adjusted R-squared = 0.9602
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_17)

# removing car_company_nameplymouth with high p value of 0.042379 and one star

# Let's execute this model here, 
model_18<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namejaguar + car_company_namemazda + 
                car_company_namemitsubishi + car_company_namepeugeot + 
                car_company_namerenault + car_company_namesubaru + car_company_nametoyota + 
                car_company_namevolkswagen + enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_18)
# R-squared = 0.9644
# Adjusted R-squared = 0.9592
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_18)

# removing car_company_namevolkswagen with high p value of 0.062542 and no star

# Let's execute this model here, 
model_19<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namejaguar + car_company_namemazda + 
                car_company_namemitsubishi + car_company_namepeugeot + 
                car_company_namerenault + car_company_namesubaru + car_company_nametoyota + 
                enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_19)
# R-squared = 0.9634
# Adjusted R-squared = 0.9584
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_19)

# removing car_company_namemazda with high p value of 0.059614 and with no star

# Let's execute this model here, 
model_20<-  lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                 car_company_namechevrolet + car_company_namejaguar + 
                 car_company_namemitsubishi + car_company_namepeugeot + 
                 car_company_namerenault + car_company_namesubaru + car_company_nametoyota + 
                 enginetypeohc + enginetyperotor + 
                 cylindernumbermedium, data = train)
  
# Let us look at the summary of the model
summary(model_20)
# R-squared = 0.9623
# Adjusted R-squared = 0.9575
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_20)

# removing car_company_namerenault with high p value of 0.022664 and with one star

# Let's execute this model here, 
model_21<- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namejaguar + 
                car_company_namemitsubishi + car_company_namepeugeot + 
                car_company_namesubaru + car_company_nametoyota + 
                enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)

# Let us look at the summary of the model
summary(model_21)
# R-squared = 0.9607
# Adjusted R-squared = 0.9561
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_21)

# removing aspiration with high p value of 0.01075 and with one star

# Let's execute this model here, 
model_22<- lm(formula = price ~ enginelocation + carwidth + 
                curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namejaguar + 
                car_company_namemitsubishi + car_company_namepeugeot + 
                car_company_namesubaru + car_company_nametoyota + 
                enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)

# Let us look at the summary of the model
summary(model_22)
# R-squared = 0.9586
# Adjusted R-squared = 0.9541
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_22)

# removing car_company_namemitsubishi with high p value of 0.02452 and with one star

# Let's execute this model here, 
model_23<- lm(formula = price ~ enginelocation + carwidth + 
                curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namechevrolet + car_company_namejaguar + 
                car_company_namepeugeot + car_company_namesubaru + car_company_nametoyota + 
                enginetypeohc + enginetyperotor + cylindernumbermedium, data = train)

# Let us look at the summary of the model
summary(model_23)
# R-squared = 0.957
# Adjusted R-squared = 0.9526
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_23)

# removing car_company_namechevrolet with high p value of 0.009057 and with two star

# Let's execute this model here, 
model_24<- lm(formula = price ~ enginelocation + carwidth + 
                curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namejaguar + car_company_namepeugeot + car_company_namesubaru + 
                car_company_nametoyota +  enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)

# Let us look at the summary of the model
summary(model_24)
# R-squared = 0.9546
# Adjusted R-squared = 0.9504
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_24)

# removing car_company_nametoyota with p value of 0.004269 with two star significant

# Let's execute this model here, 
model_25<- lm(formula = price ~ enginelocation + carwidth + 
                curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namejaguar + car_company_namepeugeot + car_company_namesubaru + 
                enginetypeohc + enginetyperotor + 
                cylindernumbermedium, data = train)

# Let us look at the summary of the model
summary(model_25)
# R-squared = 0.9517
# Adjusted R-squared = 0.9476
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_25)

# removing enginetypeohc having high VIF (>2) (2.917842) with two star significant

# Let's execute this model here, 
model_26<- lm(formula = price ~ enginelocation + carwidth + 
                curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namejaguar + car_company_namepeugeot + car_company_namesubaru + 
                enginetyperotor + cylindernumbermedium, data = train)

# Let us look at the summary of the model
summary(model_26)
# R-squared = 0.9476
# Adjusted R-squared = 0.9436
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_26)

# removing car_company_namesubaru with pmvalue as 0.24 and No star significant

# Let's execute this model here, 
model_27<- lm(formula = price ~ enginelocation + carwidth + 
                curbweight + car_company_namebmw + car_company_namebuick + 
                car_company_namejaguar + car_company_namepeugeot + 
                enginetyperotor + cylindernumbermedium, data = train)

# Let us look at the summary of the model
summary(model_27)
# R-squared = 0.947
# Adjusted R-squared = 0.9434
# Less change observed in adjusted R-squared

## Let us check for multicollinearity 
vif(model_27)

## There are few variables with hgh VIF's but has three star significance, hence did not removed those variables.

# predicting the results in test dataset
Predict_1 <- predict(model_27,test[,-18])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared # 0.8563

# Plotting scattered plot between the actual values and predicted values to understand the accuracy
ggplot(test, aes(x=test_price,y=price)) + geom_point(col="blue") + geom_smooth(method = "lm")

# There is a strong relations between the actual result and predicted values except few outliers.

# Plotting the model_27 to evaluate the model
layout(matrix(c(1,2,3,4),2,2))
plot(model_27)

# The errors are randomly distributed

# Significant Predictors for the final model (model_27) are 
# 1. enginelocation
# 2. car_company_namejaguar
# 3.car_company_namebuick
# 4.car_company_namepeugeot
# 5. enginetyperotor
# 6. carwidth
# 7. car_company_namebmw
# 8. curbweight
# 9. cylindernumbermedium
