# Installing and loading dplyr
# install.packages("dplyr")
library(dplyr)

# Installing and loading tidyr
# install.packages("tidyr")
library(tidyr)

# Installing and loading stringr
# install.packages("stringr")
library(stringr)

# Checkpoints - Part 1
# Checkpoint 1: Data Cleaning 1

# load the companies data
companies <- read.table("companies.txt", header = TRUE, sep = "\t", quote = "\"", fill = TRUE, comment.char = "")

# load the rounds data
rounds2 <- read.csv("rounds2.csv",header = TRUE)

# values are in different case in each data frame. So convert them to upper case in order to merge based on that column. 
rounds2$company_permalink <- toupper(rounds2$company_permalink)
companies$permalink <- toupper(companies$permalink)

# Table 1.1: Understand the Data Set 
# How many unique companies are present in rounds2?
length(unique(rounds2$company_permalink))

# How many unique companies are present in companies?
length(unique(companies$permalink))

# Are there any companies in the rounds2 file which are not present in companies?
ifelse(sum((companies$permalink %in% rounds2$company_permalink) != TRUE), "Y", "N") 

# Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame.
master_frame <- merge(rounds2, companies, by = 1)

# How many observations are present in master_frame?
nrow(master_frame)

# Checkpoint 2: Funding Type Analysis
# Table 2.1:  Average Values of Investments for Each of these Funding Types
avg_fund_amount = c()
# Average funding amount of venture type
avg_fund_amount["venture"] <- with(master_frame, mean(raised_amount_usd[funding_round_type == "venture"], na.rm = TRUE))
avg_fund_amount["venture"]

# Average funding amount of angel type
avg_fund_amount["angel"] <- with(master_frame, mean(raised_amount_usd[funding_round_type == "angel"], na.rm = TRUE))
avg_fund_amount["angel"]

# Average funding amount of seed type
avg_fund_amount["seed"] <- with(master_frame, mean(raised_amount_usd[funding_round_type == "seed"], na.rm = TRUE))
avg_fund_amount["seed"]

# Average funding amount of private equity type
avg_fund_amount["private_equity"] <- with(master_frame, mean(raised_amount_usd[funding_round_type == "private_equity"], na.rm = TRUE))
avg_fund_amount["private_equity"]

# Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it?
chosen_investment <- names(avg_fund_amount[between(avg_fund_amount, 5000000, 15000000)])
chosen_investment


# checkpoints - Part 2
# Checkpoint 3: Country Analysis
# Create data frame from master data frame having funding round type as chosen investment
chosen_investment_frame <- master_frame[master_frame$funding_round_type == chosen_investment,]

# Excluding rows having blank values in the country code column
chosen_investment_frame <- chosen_investment_frame[!(chosen_investment_frame$country_code == ""),]

# Aggregating the total funding based on the country code and arranging it in descending order based on the funding
chosen_investment_by_country <- arrange(aggregate(chosen_investment_frame$raised_amount_usd, by=list (chosen_investment_frame$country_code),FUN = sum, na.rm = TRUE),desc(x))

# Making a data frame name top 9 with the top 9 countries (based on the total investment amount each country has received)
top9 <- setNames(head(chosen_investment_by_country,9), c("country_code", "raised_amount_usd"))

# English-speaking countries
countries_with_english_lang <- c("BWA","CMR","ETH","ERI","GMB","GHA","KEN","LSO","LBR","MWI","MUS","NAM","NGA","RWA","SYC","SLE","ZAF","SSD","SDN","SWZ","TZA","UGA","ZMB","ZWE","ATG","BHS","BRB","BLZ","CAN","DMA","GRD","GUY","JAM","KNA","LCA","VCT","TTO","USA","IND","PAK","PHL","SGP","AUS","FJI","KIR","MHL","FSM","NRU","NZL","PLW","PNG","WSM","SLB","TON","TUV","VUT","IRL","MLT","GBR")

# Identify the top three English-speaking countries in the data frame top9
top_3_english_countries <- head(as.character(top9[top9$country_code %in% countries_with_english_lang, 1]), n = 3)
top_3_english_countries


# Checkpoint 4: sector Analysis 1
# load the mapping data
mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE)

# removing blanks from the category list as it not a main sector
mapping <- mapping[mapping$category_list != "",]

# Alter the category list to "na" that are provided as "0"
mapping$category_list <- str_replace_all(mapping$category_list, pattern = "0", "na")

# Alter "2.na" as "2.0" to represent Enterprise 2.0
mapping$category_list <- str_replace_all(mapping$category_list, pattern = "2.na", "2.0")

# values are in different case in mapping database. So converting them to upper case in order to merge based on that column.
mapping$category_list <- toupper(mapping$category_list)

# changing the wide data to a long data
mapping_long <- gather(mapping, main_sector, my_val,  Automotive...Sports:Social..Finance..Analytics..Advertising)

# removing assymentric values
mapping_long <- mapping_long[!(mapping_long$my_val == 0),]

# removing my_val column
mapping_long <- mapping_long[,-3]

# Creating primary sector in master_frame with upper case
master_frame_including_primary_sector <- separate(master_frame, category_list, into = "primary_sector", sep = "\\|", remove = FALSE, extra = "drop")

# values are in different case in master_frame_including_primary_sector database. So converting them to upper case in order to merge based on that column.
master_frame_including_primary_sector$primary_sector <- toupper(master_frame_including_primary_sector$primary_sector)

# merging through inner join for master_frame_including_primary_sector and mapping_long so that it excludes the primary sector which doesnt have a main sector.
master_frame_including_primary_main_sector <- merge(master_frame_including_primary_sector,mapping_long,by.x = "primary_sector", by.y = "category_list")

# Checkpoint 5: Sector Analysis 2
# Call three countries 'Country 1','Country 2','Country 3' and the funding type 'FT'. 
Country_1 <- top_3_english_countries[1]
Country_2 <- top_3_english_countries[2]
Country_3 <- top_3_english_countries[3]
FT <- chosen_investment

# creating D1 data frame
D1 <- filter(master_frame_including_primary_main_sector, funding_round_type == FT & country_code == Country_1 &  between(raised_amount_usd, 5000000, 15000000))
D1 <- D1 %>% group_by(main_sector) %>% mutate(number_of_investments = length(raised_amount_usd)) %>% mutate(amount_of_investments = sum(raised_amount_usd)) 

# creating D2 data frame
D2 <- filter(master_frame_including_primary_main_sector, funding_round_type == FT & country_code == Country_2 &  between(raised_amount_usd, 5000000, 15000000))
D2 <- D2 %>% group_by(main_sector) %>% mutate(number_of_investments = length(raised_amount_usd)) %>% mutate(amount_of_investments = sum(raised_amount_usd))

# creating D3 data frame
D3 <- filter(master_frame_including_primary_main_sector, funding_round_type == FT & country_code == Country_3 &  between(raised_amount_usd, 5000000, 15000000))
D3 <- D3 %>% group_by(main_sector) %>% mutate(number_of_investments = length(raised_amount_usd)) %>% mutate(amount_of_investments = sum(raised_amount_usd))

# Table 5.1 - Sector-wise Investment Analysis

# Total number of Investments (count)
# C1
length(D1$raised_amount_usd)
# C2
length(D2$raised_amount_usd)
# C3
length(D3$raised_amount_usd)

# Total amount of investment (USD)
# C1
sum(D1$raised_amount_usd)
# C2
sum(D2$raised_amount_usd)
# C3
sum(D3$raised_amount_usd)

# Top Sector name (no. of investment-wise) & Number of investments in top sector (3)
# C1
D1_number_of_investments_sector <- setNames(aggregate(D1$raised_amount_usd,by=list(D1$main_sector), FUN = length),c("main_sector", "number_of_investments"))
D1_number_of_investments_sector[which(D1_number_of_investments_sector$number_of_investments == sort(D1_number_of_investments_sector$number_of_investments, TRUE)[1]),]
# C2
D2_number_of_investments_sector <- setNames(aggregate(D2$raised_amount_usd,by=list(D2$main_sector), FUN = length),c("main_sector", "number_of_investments"))
D2_number_of_investments_sector[which(D2_number_of_investments_sector$number_of_investments == sort(D2_number_of_investments_sector$number_of_investments, TRUE)[1]),]
# C3
D3_number_of_investments_sector <- setNames(aggregate(D3$raised_amount_usd,by=list(D3$main_sector), FUN = length),c("main_sector", "number_of_investments"))
D3_number_of_investments_sector[which(D3_number_of_investments_sector$number_of_investments == sort(D3_number_of_investments_sector$number_of_investments, TRUE)[1]),]

# Second Sector name (no. of investment-wise) & Number of investments in second sector (4)
# C1
D1_number_of_investments_sector[which(D1_number_of_investments_sector$number_of_investments == sort(D1_number_of_investments_sector$number_of_investments, TRUE)[2]),]
# C2
D2_number_of_investments_sector[which(D2_number_of_investments_sector$number_of_investments == sort(D2_number_of_investments_sector$number_of_investments, TRUE)[2]),]
# C3
D3_number_of_investments_sector[which(D3_number_of_investments_sector$number_of_investments == sort(D3_number_of_investments_sector$number_of_investments, TRUE)[2]),]

# Third Sector name (no. of investment-wise) & Number of investments in third sector (5)
# C1
D1_number_of_investments_sector[which(D1_number_of_investments_sector$number_of_investments == sort(D1_number_of_investments_sector$number_of_investments, TRUE)[3]),]
# C2
D2_number_of_investments_sector[which(D2_number_of_investments_sector$number_of_investments == sort(D2_number_of_investments_sector$number_of_investments, TRUE)[3]),]
# C3
D3_number_of_investments_sector[which(D3_number_of_investments_sector$number_of_investments == sort(D3_number_of_investments_sector$number_of_investments, TRUE)[3]),]

# For point 3 (top sector count-wise), which company received the highest investment?
# C1
D1_top_sector_filter <- filter(D1, main_sector == "Others")
D1_top_sector_company_wise <- D1_top_sector_filter %>% group_by(company_permalink, name) %>% summarise(amount =  sum(raised_amount_usd))
D1_top_sector_company_wise[which(D1_top_sector_company_wise$amount == sort(D1_top_sector_company_wise$amount,TRUE)[1]),2]
# C2
D2_top_sector_filter <- filter(D2, main_sector == "Others")
D2_top_sector_company_wise <- D2_top_sector_filter %>% group_by(company_permalink, name) %>% summarise(amount =  sum(raised_amount_usd))
D2_top_sector_company_wise[which(D2_top_sector_company_wise$amount == sort(D2_top_sector_company_wise$amount,TRUE)[1]),2]
# C3
D3_top_sector_filter <- filter(D3, main_sector == "Others")
D3_top_sector_company_wise <- D3_top_sector_filter %>% group_by(company_permalink, name) %>% summarise(amount =  sum(raised_amount_usd))
D3_top_sector_company_wise[which(D3_top_sector_company_wise$amount == sort(D3_top_sector_company_wise$amount,TRUE)[1]),2]

# For point 4 (second best sector count-wise), which company received the highest investment?
# C1
D1_top_2_sector_filter <- filter(D1, main_sector == "Social..Finance..Analytics..Advertising")
D1_top_2_sector_company_wise <- D1_top_2_sector_filter %>% group_by(company_permalink, name) %>% summarise(amount =  sum(raised_amount_usd))
D1_top_2_sector_company_wise[which(D1_top_2_sector_company_wise$amount == sort(D1_top_2_sector_company_wise$amount,TRUE)[1]),2]
# C2
D2_top_2_sector_filter <- filter(D2, main_sector == "Social..Finance..Analytics..Advertising")
D2_top_2_sector_company_wise <- D2_top_2_sector_filter %>% group_by(company_permalink, name) %>% summarise(amount =  sum(raised_amount_usd))
D2_top_2_sector_company_wise[which(D2_top_2_sector_company_wise$amount == sort(D2_top_2_sector_company_wise$amount,TRUE)[1]),2]
# C3
D3_top_2_sector_filter <- filter(D3, main_sector == "Social..Finance..Analytics..Advertising")
D3_top_2_sector_company_wise <- D3_top_2_sector_filter %>% group_by(company_permalink, name) %>% summarise(amount =  sum(raised_amount_usd))
D3_top_2_sector_company_wise[which(D3_top_2_sector_company_wise$amount == sort(D3_top_2_sector_company_wise$amount,TRUE)[1]),2]