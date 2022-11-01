#DATASET NUMBER 8; Biella

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data = read.csv("Data/Raw_data/6_Biella/interaction_data_biella.csv")

#Standardize data
data = data %>% 
mutate(Plant_species = str_replace(Plant_species, "_", " ")) %>% 
mutate(Pollinator_species = str_replace(Pollinator_species, "_", " ")) %>% 
mutate(Pollinator_species = str_to_sentence(Pollinator_species))

#Split data into different dataframes based on survey name
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
flower_count = read.csv("Data/Raw_data/6_Biella/flower_count_biella.csv")
#Split data into different dataframes based on survey name
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
Metadata <- tibble(
  Doi = "https://doi.org/10.1556/168.2017.18.1.1",
  Dataset_description = "This dataset documents 2 communities on different slopes
  of a mountain in the Northen Apennine",
  Taxa_recorded = "All flower visitors")

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Paolo Biella"),
  Orcid = c("0000-0003-2297-006X"),
  E_mail = c("paolo.biella@unimib.it"))

#Save data ----
#Create list with all dataframes of interest
Biella <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Biella) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 8
saveRDS(Biella, file="Data/Clean_data/8_Biella.rds") 

