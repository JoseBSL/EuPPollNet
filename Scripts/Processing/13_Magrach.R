#DATASET NUMBER 15; Magrach

#Load libraries
library(tidyverse)

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/Raw_data/13_Magrach/interaction_data_magrach.csv", col_names = T)

#Split data into different dataframes based on survey name
InteractionData <- split(InteractionData, InteractionData$Site_id)

#Prepare flower count data ----
flower_count = read_csv("Data/Raw_data/13_Magrach/flower_count_magrach.csv", col_names = T)

#Split data into different dataframes based on survey name
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
Metadata <- tibble(
  Doi = NA,
  Dataset_description = "",
  Taxa_recorded = "All flower visitors")

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Ainhoa Magrach", "Maddi Artamendi", "Paula Dominguez Lapido"),
  Orcid = c("0000-0003-2155-7556", "", ""),
  E_mail = c("ainhoa.magrach@bc3research.org", "maddiart19@gmail.com", 
             "pauladl@enebada.eu"))
