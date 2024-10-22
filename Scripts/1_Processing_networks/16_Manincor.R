#DATASET NUMBER 16; de Manincor

#Note that this is the only file saved as CSV and not 
#as MS-DOS CSV comma separated. For issues
#with the degree unicode

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/1_Raw_data/16_Manincor/Interaction_data.csv", col_names = T)

#Convert coordinates in degree mins and seconds to lat long in decimal degrees
data = data %>%
mutate(Longitude = parzer::parse_lon(Longitude),
         Latitude = parzer::parse_lat(Latitude)) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) %>% 
mutate(Coordinate_precision = str_replace(Coordinate_precision,  " m", "")) %>% 
mutate(Flower_data = "No") %>% 
mutate(Flower_data_merger = Site_id) 
  
  
#Unify structure of data
data = change_str(data)

unique(factor(data$Latitude))
unique(factor(data$Longitude))

#Split data into different dataframes based on site id
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
site_id_levels = levels(factor(bind_rows(InteractionData)$Site_id))
FlowerCount = tibble(Day = NA_character_, Month = NA_character_, Year = NA, Site_id = site_id_levels, Plant_species = NA_character_,
                      Flower_count = NA, Units = NA_character_, Comments = NA_character_,
                     Flower_data_merger = site_id_levels)

#Set common structure
FlowerCount = change_str2(FlowerCount)

#Split by Site_id
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata <- tibble(
Doi = "https://doi.org/10.1111/oik.07259; 10.1016/j.actao.2020.103551",
Dataset_description = "This dataset documents plant pollinator
interactions in 6 calcareous grasslands in three different
regions in France, over a period of 7 months in 2016. Most
of the bee data are unpublished, while most part of hoverflies
data have been published in paper #1 (see below for details). 
small portion of bee-visits have been published in paper
#2 (see details below). The flower availability is reported
for the two papers, but it is not exhaustive (unpublished
flower availability data have not been reported here, but
if needed you can contact N. de Manincor). These data are
part of the ANR ARSENIC project (grant no. 14-CE02-0012)",
Taxa_recorded = "Bees and hoverflies",
Sampling_year = "2016",
Country = "France",
Habitat = "Grassland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = NA,
Sampling_method = "Transect_and_random_walk",
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
Coauthor_name = c("Natasha de Manincor", "Nina Hautekèete", 
                  "Yves Piquot", "Bertrand Schatz", "François Massol"),

Orcid = c("0000-0001-9696-125X", "0000-0002-6071-5601", 
          "0000-0001-9977-8936", "0000-0003-0135-8154",
          "0000-0002-4098-955X"),
E_mail = c("natasha.demanincor@gmail.com", "nina.hautekeete@univ-lille.fr", 
             "yves.piquot@univ-lille.fr", "bertrand.schatz@cefe.cnrs.fr",
           "francois.massol@univ-lille.fr"))

#Save data ----
#Create list with all dataframes of interest
Manincor <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Manincor) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Manincor, file="Data/2_Processed_data/16_Manincor.rds")

