#DATASET NUMBER 10; Vanbergen

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read.csv("Data/Raw_data/10_Vanbergen/Interaction_data.csv")
#Create cols of Temperature and humidity
data = data %>% 
mutate(Temperature = NA) %>% 
mutate(Humidity = NA) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Unify structure of data
data = change_str(data)

#Split by Site_id
InteractionData <- split(data, data$Site_id)


#Prepare flower count data ----
flower_count = read.csv("Data/Raw_data/10_Vanbergen/Flower_count.csv")

#Filter out one column but saved the data
flower_count = flower_count %>% 
mutate(Units = paste0(Units, "; ", Inflorescence.type)) %>% 
select(!Inflorescence.type)
#Split data into different dataframes based on survey name
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata <- tibble(
Doi = NA,
Dataset_description = "This dataset is part of the VOODOO-project.
It consists of plant-pollinator networks in 12 different landscapes
(4 agricultural, 4 rural, 4 urban), sampled over 3 periods in 2021
(mid april, end may and mid july). Coordinates are the centroids of each landscape.
Transects were stratified in relation with the area of each type of pollinator habitat
within a 500 m radius around the centroid. Habitat is given for each individual catch,
not for the landscape as a whole. Transect length and duration are therefore pooled in
this file, rather than giving the sampling time in each habitat
(in total: 120 min per site visit, so 360 min in total, if the networks are pooled
over the 3 sampling periods). If requested, we can also provide sampling time from
each subtransect in different habitat types.",
Taxa_recorded = "All flying insects, except really small stuff (< 1 mm)",
Sampling_year = 2021,
Country = "France",
Habitat = "4 agricultural, 4 rural, 4 urban",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = 3,
Sampling_method = "Transect",
Sampling_area_details = NA,
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA,
Sampling_time_details = NA,
Sampling_time_species_round_min = 120,
Sampling_time_total_min = 120 * 3 * 12, #120 mins * 3 rounds * 12 sites
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Adam J. Vanbergen", "Willem Proesmans"),
  Orcid = c("0000-0001-8320-5535", "0000-0003-0358-6732"),
  E_mail = c("adam.vanbergen@inrae.fr", "willem.proesmans@gmail.com"))

#Save data ----
#Create metadata list
Vanbergen <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Vanbergen) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 10
saveRDS(Vanbergen, file="Data/Clean_data/10_Vanbergen.rds") 
