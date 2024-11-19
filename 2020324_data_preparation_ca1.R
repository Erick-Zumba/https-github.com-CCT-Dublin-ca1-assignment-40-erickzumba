#getting the work directory
getwd()
#setting the working directory and using the download folder
setwd("C:/Users/Erick/Downloads")

#load necessary libraries
library(dplyr)
library(tidyr)
#reading the data set from the csv file
dataset <- read.csv("Electric_Vehicle_Population_Data.csv")
head(dataset)
#summarizing the dataset
summary(dataset)
dataset_clean <- na.omit(dataset)

# Remove columns with any NA
dataset_clean <- dataset[, colSums(is.na(dataset)) == 0]

# Check if there are still NA values
sum(is.na(dataset_clean))

# Replace NA with mean in numerical columns
dataset_clean <- dataset %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Check if there are still missing values
sum(is.na(dataset_clean))

# View cleaned dataset
head(dataset_clean)

