# install.packages("dplyr")
library(dplyr)

# install.packages("tidyr")
library(tidyr)

# install.packages("lubridate")
library(lubridate)

# loading and reading the uber data file
uber_data <- read.csv("Uber Request Data.csv",header = T)

# Structure of dataset
str(uber_data)

# Data Cleaning

# 1. Missing values
sum(is.na(uber_data)) # 6564 NA values under Driver.Id and Drop.timestamp. Seems okay because of the cab cancellation and no car available that values would be NA only.
sapply(uber_data, function(x) length(which(x == ""))) # checking for blank "" values; there are none

# 2. Check individual columns

# a. Request.Id

# look for duplicate values
duplicated(uber_data)
sum(duplicated(uber_data)) # No duplicate IDs here

# finding which request ID are missing from the sequence
full.list <- seq(from = 1, to = 6766, by = 1)
setdiff(full.list,uber_data$Request.id) # ignoring the missing request ids as there is no information about those request ids from external source

# b. Pickup.point
summary(uber_data$Pickup.point) # seems okay

# c. Driver.id
summary(uber_data$Driver.id)
# convert to factor
uber_data$Driver.id <- factor(uber_data$Driver.id)
summary(uber_data$Driver.id)

# d. Stats
summary(uber_data$Status) # seems okay

# e. Request.timestamp

# converting to Request.timestamp column to date time format
uber_data$Request.timestamp <- parse_date_time(uber_data$Request.timestamp, c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S"), tz = "")

# Extract date, time, day, hour
uber_data$Request_date <- as.Date(uber_data$Request.timestamp)
uber_data$Request_time <- strftime(uber_data$Request.timestamp, format = "%H:%M:%S")
uber_data$Request_day <- format(uber_data$Request_date, "%d")
uber_data$Request_hour <- format(uber_data$Request.timestamp, "%H")

# converting request_hour to numeric
uber_data$Request_hour <- as.numeric(uber_data$Request_hour)

# converting to Drop.timestamp column to date time format
uber_data$Drop.timestamp <- parse_date_time(uber_data$Drop.timestamp, c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S"), tz = "")

# Extract date, time, day, hour
uber_data$Drop_date <- as.Date(uber_data$Drop.timestamp)
uber_data$Drop_time <- strftime(uber_data$Drop.timestamp, format = "%H:%M:%S")
uber_data$Drop_day <- format(uber_data$Drop_date, "%d")
uber_data$Drop_hour <- format(uber_data$Drop.timestamp, "%H")

# converting request_hour to numeric
uber_data$Drop_hour <- as.numeric(uber_data$Drop_hour)

# identifying timeslots based on the frequency of requests made
hour_wise_trip_request <- uber_data %>% group_by(Request_hour) %>% summarise(Number_of_requests = length(Request.id)) # time_slot was derived based on the frequency

# Creating time_solts
uber_data$Time_slot <- ifelse(uber_data$Request_hour < 5, "Early_Morning_Time", 
                              ifelse(uber_data$Request_hour < 11, "Morning_Time", 
                                     ifelse(uber_data$Request_hour < 17, "Day_Time", 
                                            ifelse(uber_data$Request_hour < 22, "Evening_Time", "Late_Night_Time"))))


# Results expected

# 1. Identify most pressing problems for uber

# count of request based on status
summary(uber_data$Status) # only 2831 trips were completed out of 6745 trips

# percentage of request based on status
round(summary(uber_data$Status)/length(uber_data$Status)*100, digits = 2) # only approx 42% of the trips were completed

# creating a dataframe by grouping time_slot and status and calculating number of requests for Airport
Number_of_requests_Airport <- uber_data %>% filter(uber_data$Pickup.point == "Airport") %>% group_by(Time_slot, Status) %>% summarise(Number_of_requests = length(Request.id))

# spreading the dataframe for viewing easily for Airport
Number_of_requests_Airport <- spread(Number_of_requests_Airport, Status, Number_of_requests) # Large number of requests had the status as "No Cars Available" during  "Evening_Time"  time slot for "Airport to City" pickup
                                     
# creating a dataframe by grouping time_slot and status and calculating number of requests for City
Number_of_requests_City <- uber_data %>% filter(uber_data$Pickup.point == "City") %>% group_by(Time_slot, Status) %>% summarise(Number_of_requests = length(Request.id))

# spreading the dataframe for viewing easily for City
Number_of_requests_City <- spread(Number_of_requests_City, Status, Number_of_requests) # Large number of trips was cancelled  during  "Morning_Time"  time slot for "City to Airport" pickup

# 2. Gap between supply and demand

# Identifying the time slots with highest gap

# creating data frame for trips requested at various time slots
time_slot_trip_requested <- uber_data %>% group_by(Time_slot) %>% summarise(Number_of_requests = length(Request.id)) 

# creating data frame for trips completed at various time slots
time_slot_trip_completed <- uber_data %>% group_by(Time_slot) %>% summarise(Number_of_trip_completed = length(which(Status == "Trip Completed"))) 

# merge
time_slot_gap <- merge(time_slot_trip_requested,time_slot_trip_completed, by= "Time_slot")

# Create a variable showing the difference between the trip reqested and trips completed
time_slot_gap$Difference <- time_slot_gap$Number_of_requests - time_slot_gap$Number_of_trip_completed            

# Identifying the types of requests for which gap is the most severe during "Morning_Time" & "Evening Time" Slot

# creating data frame for trips requested at morning and evening time slots
types_of_requests_trip_requested <- uber_data %>% filter(Time_slot == "Morning_Time" | Time_slot == "Evening_Time")  %>% group_by(Pickup.point, Time_slot) %>% summarise(Number_of_requests = length(Request.id)) 

# creating data frame for trips completed at morning and evening time slots
types_of_requests_trip_completed <- uber_data %>% filter(Time_slot == "Morning_Time" | Time_slot == "Evening_Time") %>% group_by(Pickup.point, Time_slot) %>% summarise(Number_of_trip_completed = length(which(Status == "Trip Completed"))) 

# merge
types_of_request_gap <- merge(types_of_requests_trip_requested, types_of_requests_trip_completed, by = c("Pickup.point", "Time_slot"))

# Create a variable showing the difference between the trip reqested and trips completed
types_of_request_gap$Difference <- types_of_request_gap$Number_of_requests - types_of_request_gap$Number_of_trip_completed 

# High supply demand exists during "Evening_Time" time slot for "Airport to City" pickup
# High supply demand exists during "Morning_Time" time slot for "City to Airport" pickup