#DATASET NUMBER 8; Biella

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read.csv("Data/1_Raw_data/8_Biella/Interaction_data.csv")

#Standardize data
data = data %>% 
mutate(Plant_species = str_replace(Plant_species, "_", " ")) %>% 
mutate(Pollinator_species = str_replace(Pollinator_species, "_", " ")) %>% 
mutate(Pollinator_species = str_to_sentence(Pollinator_species)) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) 

#Unify structure of data
data = change_str(data)

#Split data into different dataframes based on survey name
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount = read.csv("Data/1_Raw_data/8_Biella/Flower_count.csv")
#Unify data structure
FlowerCount = change_str2(FlowerCount)
#Split data into different dataframes based on survey name
FlowerCount <- split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)


#Generate metadata
Metadata <- tibble(
Doi = "https://doi.org/10.1556/168.2017.18.1.1",
Dataset_description = "This dataset documents 2 communities on different slopes
of a mountain in the Northen Apennine",
Taxa_recorded = "All flower visitors",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "Weekly sampling",
Sampling_year = 2013,
Country = "Italia",
Sampling_method = "Plots",
Sampling_area_details = "2.5 * 2.5 m plots",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = 2.5 * 2.5 * 6,
Sampling_time_details = "20 minutes / plot and round",
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
saveRDS(Biella, file="Data/2_Processed_data/8_Biella.rds") 

