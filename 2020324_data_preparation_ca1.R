#getting the work directory
getwd()

#setting the working directory and using the download folder
setwd("C:/Users/Erick/Downloads")

#load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)  

#reading the data set from the csv file
dataset <- read.csv("Electric_Vehicle_Population_Data.csv")
head(dataset)


# Replace NA with mean in numerical columns
dataset_clean <- dataset %>%  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Check if there are still missing values
sum(is.na(dataset_clean))


# Handle outliers in Electric Range using IQR
Q1 <- quantile(dataset_clean$Electric.Range, 0.25)
Q3 <- quantile(dataset_clean$Electric.Range, 0.75)
IQR <- Q3 - Q1
dataset_clean <- dataset_clean %>%
  filter(Electric.Range >= (Q1 - 1.5 * IQR) & Electric.Range <= (Q3 + 1.5 * IQR))


# View cleaned dataset
head(dataset_clean)
#summarizing the dataset
summary(dataset_clean)


# Boxplot
ggplot(dataset, aes(x = Electric.Vehicle.Type, y = Electric.Range)) +
  geom_boxplot(fill ="red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Electric Range by Vehicle Type", x = "Vehicle Type", y = "Electric Range (miles)")


# Bar plot for vehicle types after the data was clean
ggplot(dataset_clean, aes(x = Electric.Vehicle.Type)) +
  geom_bar(fill = "green") +
  theme_minimal() +
  labs(title = "Frequency of Electric Vehicle Types", x = "Vehicle Type", y = "Count")

# Scatterplot to identify relationships and potential outliers between Electric.Range and Base.MSRP
ggplot(dataset_clean, aes(x = Base.MSRP, y = Electric.Range, color = Electric.Vehicle.Type)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Base MSRP vs Electric Range", x = "Base MSRP", y = "Electric Range")


# Load the dataset for pca 

electric_vehicle <- read.csv("Electric_Vehicle_Population_Data.csv", header = TRUE, sep = ",", na.strings = "NA", stringsAsFactors = FALSE)

# Select numeric columns for PCA
numeric_columns <- c("Electric.Range", "Base.MSRP", "Legislative.District", "X2020.Census.Tract")
pca_data <- electric_vehicle[, numeric_columns]
head(pca_data)
# Handle missing values (e.g., remove rows with NA)
pca_data <- na.omit(pca_data)

# Standardize the data
pca_data_scaled <- scale(pca_data)

# Perform PCA
pca_result <- prcomp(pca_data_scaled, center = TRUE, scale. = TRUE)

# View summary of PCA
summary(pca_result)


# Extract explained variance
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)

#paste0 concatenated two vectors after converting to a string
explained_variance_df <- data.frame(Principal_Component = paste0("PC", 1:length(explained_variance)), Variance_Explained = explained_variance)

#summary of explained variance
summary(explained_variance_df)

# Scree plot using ggplot2
ggplot(explained_variance_df, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(group = 1), color = "red") +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Scree Plot: Explained Variance",
    x = "Principal Components",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal()



