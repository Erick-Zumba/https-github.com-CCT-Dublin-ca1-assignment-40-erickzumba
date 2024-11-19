#getting the work directory
getwd()
#setting the working directory and using the download folder
setwd("C:/Users/Erick/Downloads")

#load necessary libraries
library(dplyr)
library(tidyr)
dataset <- read.csv("Electric_Vehicle_Population_Data.csv")
head(dataset)