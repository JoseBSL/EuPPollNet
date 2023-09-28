#DATASET NUMBER 40; Schweiger

#Data sent by Oliver Schweiger. This dataset belongs to the VOODOO project
#https://voodoo-project.eu/about

#Read empty templates to compare with
source("Scripts/Empty_templates.R") 

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/40_Schweiger/Interaction_data.csv")

#Compare vars
#compare_variables(check_interaction_data, data)

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))


#check sites
unique(factor(data$Site_id))
unique(factor(data$Longitude))

data %>% filter(is.na(Longitude))


#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)


