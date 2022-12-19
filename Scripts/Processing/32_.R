#DATASET NUMBER 32; ALL datasets from RUSSO

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/32_Russo/Interaction_data.csv")

levels(factor(data$Site_id))
data = data %>% separate(Site_id, c("Name", "Site", "Site_number"), remove = F)

levels(factor(data$Name))

