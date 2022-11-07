#DATASET NUMBER 17; Manincor

#Load libraries
library(tidyverse)

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/Raw_data/16_17_Manincor/Interaction_data2.csv", col_names = T)

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
flower_count = read_csv("Data/Raw_data/16_17_Manincor/Flower_count2.csv", col_names = T)

#Delete extra column that indicated that this dataset was number one
flower_count = flower_count %>% 
  select(!c(Comment...8, Comment...9))

#Split data into different dataframes based on survey name
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)


Metadata <- tibble(
Doi = "10.1016/j.actao.2020.103551",
Dataset_description = "These datasets document 3 bee-plant networks observed in 3
calcareaous grasslands in France during the month of July 2016.
The current data only present the observed interactions (visit-based network
in the mentioned paper published in Acta Oecologica). These data are part of the
ANR ARSENIC project (grant no. 14-CE02-0012).",
Taxa_recorded = "Only female bee visitors have been used for these datasets",
Sampling_year = "2016",
Country = "France",
Habitat = "Calcareous grasslands",
Sampling_sites = 3,
Sampling_rounds = "3 days",
Sampling_method = "Random walk",
Sampling_area_details = "1 hectare per site",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = "30000",
Sampling_time_details = "4 hours divided in 2 in the moring and 2 in the afternoon each day and site",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = 2 * 2 * 3 * 3 * 60, #2 hours day *2 hours night * 3 days * 3 sites
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")


#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()


#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Natasha de Manincor", "Nina Hautekèete", "Clément Mazoyer",
                    "Paul Moreau", "Yves Piquot", "Bertrand Schatz", "Eric Schmitt",
                    "Marie Zélazny", "François Massol"),
  Orcid = c("0000-0001-9696-125X", "0000-0002-6071-5601", "", "", "0000-0001-9977-8936",
            "0000-0003-0135-8154", "", "", "0000-0002-4098-955X"),
  E_mail = c("natasha.demanincor@gmail.com", "nina.hautekeete@univ-lille.fr", 
             "clement.mazoyer@univ-lille.fr", "p35.moreau@gmail.com",
             "yves.piquot@univ-lille.fr", "bertrand.schatz@cefe.cnrs.fr",
             "eric.schmitt@univ-lille.fr", "marie.zelazny@hotmail.fr",
             "francois.massol@univ-lille.fr"))

#Save data ----
#Create list with all dataframes of interest
Manicor <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Manicor) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Manicor, file="Data/Clean_data/17_Manicor.rds")

