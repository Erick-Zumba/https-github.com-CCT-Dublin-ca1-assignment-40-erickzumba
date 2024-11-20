#getting the work directory
getwd()
#setting the working directory and using the download folder
setwd("C:/Users/Erick/Downloads")

#load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
#reading the data set from the csv file
dataset <- read.csv("Electric_Vehicle_Population_Data.csv")
head(dataset)
#summarizing the dataset
summary(dataset)

# Replace NA with mean in numerical columns
dataset_clean <- dataset %>%  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Check if there are still missing values
sum(is.na(dataset_clean))

# View cleaned dataset
head(dataset_clean)

# Scatterplot to identify relationships and potential outliers between Electric.Range and Base.MSRP
ggplot(dataset, aes(x = Electric.Range, y = Base.MSRP)) +
  geom_point(color = "blue", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Scatterplot: Electric Range vs Base MSRP",
       x = "Electric Range (miles)",
       y = "Base MSRP ($)")
