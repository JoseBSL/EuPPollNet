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

#Select unique cases of polls and plants from the list 
for (i in InteractionData) {
#Generate sum of distinct plants per site
plant_sum <- bind_rows(lapply(InteractionData, function(x) x %>% 
             select(Plant_species, Site_id) %>% 
             group_by(Site_id) %>% 
             summarise(Sum = n_distinct(Plant_species))))
#Total unique cases 
plant_single_cases <- bind_rows(lapply(InteractionData, 
            function(x) x %>% select(Plant_species) %>% distinct(Plant_species)))
pollinator_single_cases <- bind_rows(lapply(InteractionData, 
            function(x) x %>% select(Pollinator_species) %>% distinct(Pollinator_species)))
}

#Plant sum should be the total number of plants sampled
plant_sum = sum(plant_sum$Sum)
plant_single_cases = distinct(plant_single_cases)
pollinator_single_cases = distinct(pollinator_single_cases)

#Generate metadata
Metadata <- tibble(
Doi = "https://doi.org/10.1556/168.2017.18.1.1",
Dataset_description = "This dataset documents 2 communities on different slopes
of a mountain in the Northen Apennine",
Taxa_recorded = "All flower visitors",
Sampling_sites = 6,
Sampling_rounds = NA,
Year = 2013,
Country = "Italia",
Sampling_method = NA,
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

