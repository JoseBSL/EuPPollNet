#DATASET NUMBER; STEP PROJECT
#This metadataset was sent by A. Magrach
#The dataset of Germany was already sent by other colleagues
#So I won't add it here

#Read templates to compare with
source("Scripts/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/29_STEP/Interaction_data_STEP.csv")

#Check col names with template
compare_variables(check_interaction_data, data)

#Check levels per country
levels(factor(data$Country))


