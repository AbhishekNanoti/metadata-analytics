# Installing the required packages
# install.packages("tidyverse")
# install.packages("srtingr")
# install.packages("maps")
# install.packages("ggmap")
# install.packages("readxl")
# install.packages("writexl")
# install.packages("countrycode")

# Loading the packages
library(tidyverse)
library(dplyr)
library(ggmap)
library(maps)
library(stringr)
library(readxl)
library(writexl)
library(countrycode)


# set working directory
setwd('/groups/sebratt/ICSSI23')

# read the total cost csv
totalCost <- read.csv(file = 'GB_NIH_TOTAL_COST_subset.csv')

# structure of totalCost
str(totalCost)

# read the sub project cost csv
subPrjCost <- read.csv(file = 'GB_NIH_SUBPROJ_COST_subset.csv')

# structure of subPrjCost
str(subPrjCost)

# checking the type of the csv
class(totalCost)

# combining both the csv's into new data frame combCost
combCost <- rbind(totalCost , subPrjCost)

# displaying total  number of rows on the console
print(nrow(combCost))

# Finding all the countries from world.cities
all_countries <- str_c(unique(world.cities$country.etc), collapse = "|")

# Creating a new column country in the combCost dataframe
combCost$country <- sapply(str_extract_all(combCost$journal.x,
                                           all_countries), toString)

# Creating a new column for country codes
combCost$countrycode <- countrycode(sourcevar = combCost$country,
                                    origin = "country.name",
                                    destination = "iso3c")

# Checking the datatype of the country and countrycode column
class(combCost$country)
class(combCost$countrycode)

# Changing the datatype of country column from character to factor
combCost$countrycode <- as.factor(combCost$countrycode)
combCost$country <- as.factor(combCost$country)

# Summary of combCost
summary(combCost)

# Number of rows having country information is 1272710

# percentage of rows having country information is:
print((1272710/1303286)*100)

# Storing sci_tech_capacity in scitech dataframe
scitech <- read.csv(file = 'sci_tech_capacity.csv')

# Reading the CLASS.xlsx data in 'data'
data <- read_excel("CLASS.xlsx")

# Writing the data into CLASS.csv 
# ------> for now change the working directory at this step to 
# setwd("~/metadata-analytics")
write_csv(data,"CLASS.csv")

# Storing CLASS in class dataframe
class <- read.csv(file = "CLASS.csv")

# Adjusting the column names for consistency
colnames(class)[4] <- c('incomegroup')
colnames(class)[2] <- c('countrycode')
colnames(scitech)[2] <- c('countrycode')

# Creating the income dataframe from class for joining with combCost
income_df <- class %>% select('countrycode', 'incomegroup')

# Creating the stc dataframe from scitech for joining with combCost
stc_df <- scitech %>% select('countrycode', 'sci_capacity')

# Merge the income and S&T capacity data with the combCost dataset 
# based on the 'countrycode' column
merged_df <- merge(combCost, income_df, by = "countrycode", all.x = TRUE)
GB_NIH_FINAL_WITH_ALL_COLUMNS <- merge(merged_df, stc_df, 
                                       by = "countrycode", all.x = TRUE)

# Checking the datatypes of new columns added to final csv
class(GB_NIH_FINAL_WITH_ALL_COLUMNS$incomegroup)
class(GB_NIH_FINAL_WITH_ALL_COLUMNS$sci_capacity)

# Adjusting the datatypes to easily understand the changes
GB_NIH_FINAL_WITH_ALL_COLUMNS$incomegroup <- 
  as.factor(GB_NIH_FINAL_WITH_ALL_COLUMNS$incomegroup)

GB_NIH_FINAL_WITH_ALL_COLUMNS$sci_capacity <- 
  as.factor(GB_NIH_FINAL_WITH_ALL_COLUMNS$sci_capacity)

# Summary of the final dataset
summary(GB_NIH_FINAL_WITH_ALL_COLUMNS)

# Creating an new csv file and updating the latest details
GB_NIH_FINAL_WITH_ALL_COLUMNS.file <-
  paste0("~/metadata-analytics/GB_NIH_FINAL_COST.csv")
write.csv(GB_NIH_FINAL_WITH_ALL_COLUMNS, GB_NIH_FINAL_WITH_ALL_COLUMNS.file,
          row.names = FALSE)