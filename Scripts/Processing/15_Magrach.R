#DATASET NUMBER 15; Magrach

#Load libraries
library(tidyverse)

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/Raw_data/15_Magrach/interaction_data_magrach.csv", col_names = T) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata


#Split data into different dataframes based on survey name
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
flower_count = read_csv("Data/Raw_data/13_Magrach/flower_count_magrach.csv", col_names = T)

#Split data into different dataframes based on survey name
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----

#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata <- tibble(
Doi = "Unpublished",
Dataset_description = NA,
Taxa_recorded = "All floral visitors",
Sampling_year = "2020",
Country = "Spain",
Habitat = "Grassland",
Sampling_sites = "5",
Sampling_rounds = NA,
Sampling_method = "Transect",
Sampling_area_details = NA,
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA,
Sampling_time_details = NA,
Sampling_time_species_round_min = NA,
Sampling_time_total_min = NA,
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Ainhoa Magrach", "Maddi Artamendi", "Paula Dominguez Lapido"),
  Orcid = c("0000-0003-2155-7556", "", ""),
  E_mail = c("ainhoa.magrach@bc3research.org", "maddiart19@gmail.com", 
             "pauladl@enebada.eu"))

#Save data ----
#Create list with all dataframes of interest
Magrach <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Magrach) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 15
saveRDS(Magrach, file="Data/Clean_data/15_Magrach.rds") 


