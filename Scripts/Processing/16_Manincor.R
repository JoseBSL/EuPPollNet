#DATASET NUMBER 16; Manincor

#Load libraries
library(tidyverse)

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/Raw_data/16_17_Manincor/Interaction_data1.csv", col_names = T)

#Convert coordinates in degree mins and seconds to lat long in decimal degrees
data = data %>%
mutate(Longitude = parzer::parse_lon(Longitude),
         Latitude = parzer::parse_lat(Latitude)) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata


unique(factor(data$Latitude))
unique(factor(data$Longitude))

#Split data into different dataframes based on site id
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
flower_count = read_csv("Data/Raw_data/16_17_Manincor/Flower_count1.csv", col_names = T)

#Delete extra column that indicated that this dataset was number one
flower_count = flower_count %>% 
dplyr::select(!c(Comment...8, Comment...9))

#Check levels, one is different rename
setdiff(levels(factor(flower_count$Site_id)), levels(factor(data$Site_id)))

flower_count = flower_count %>% 
mutate(Site_id = recode_factor(Site_id,  "Noeux-lès-Auxi" = "Riez_2016"))

#Split data into different dataframes based on survey name
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----

#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata <- tibble(
Doi = "https://doi.org/10.1111/oik.07259",
Dataset_description = "These datasets document 6 hoverfly-plant
networks observed in 6 calcareous grasslands in France and have
been published in Oikos. These data are part of the ANR ARSENIC project
(grant no. 14-CE02-0012).",
Taxa_recorded = "Only hoverfly visitors have been used for these datasets",
Sampling_year = "2016",
Country = "France",
Habitat = "Calcareous grassland",
Sampling_sites = 6,
Sampling_rounds = NA,
Sampling_method = "Random walk",
Sampling_area_details = NA,
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA,
Sampling_time_details = NA,
Sampling_time_species_round_min = NA,
Sampling_time_total_min = 164 * 60,
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
Coauthor_name = c("Natasha de Manincor", "Nina Hautekèete", "Yves Piquot", "Bertrand Schatz",
                  "Cédric Vanappelghem", "François Massol"),
Orcid = c("0000-0001-9696-125X", "0000-0002-6071-5601", "0000-0001-9977-8936", "0000-0003-0135-8154",
          "", "0000-0002-4098-955X"),
E_mail = c("natasha.demanincor@gmail.com", "nina.hautekeete@univ-lille.fr", 
             "yves.piquot@univ-lille.fr", "bertrand.schatz@cefe.cnrs.fr",
           "cedric.vanappelghem@espaces-naturels.fr", "francois.massol@univ-lille.fr"))

#Save data ----
#Create list with all dataframes of interest
Manicor <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Manicor) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Manicor, file="Data/Clean_data/16_Manicor.rds")

