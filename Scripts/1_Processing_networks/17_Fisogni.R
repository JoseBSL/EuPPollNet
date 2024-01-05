#DATASET NUMBER 17; Fisogni and Galloni dataset
#Data sent by Fisogni

#Load libraries
library(tidyverse)

#Read empty templates
source("Scripts/Processing/Functions/Empty_templates.R") 
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/1_Raw_data/17_Fisogni/Interaction_data.csv")

#Compare vars
#compare_variables(check_interaction_data, data)
#Nothing missing!

#Convert coordinates to decimal degrees
data = data %>%
mutate(Longitude = parzer::parse_lon(Longitude),
         Latitude = parzer::parse_lat(Latitude)) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Unify structure of data
data = change_str(data)

#Note that we are missing elevation

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
#We need to create a list of lists too
#Check levels of Site_id
site_id_levels = levels(factor(data$Site_id))

FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = site_id_levels, Plant_species = NA,
                     Flower_count = NA, Units = NA, Comment = NA)

FlowerCount = FlowerCount %>% 
mutate(Day = as.character(Day)) %>% 
mutate(Month = as.character(Month)) %>% 
mutate(Year = as.numeric(Year)) %>% 
mutate(Site_id = as.character(Site_id)) %>% 
mutate(Plant_species = as.character(Plant_species)) %>% 
mutate(Flower_count = as.numeric(Flower_count)) %>% 
mutate(Units = as.character(Units)) %>% 
mutate(Comment = as.character(Comment))

#Split by Site_id
FlowerCount <- split(FlowerCount, FlowerCount$Site_id)
#Prepare metadata data ----

#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata <- tibble(
Doi = "https://zenodo.org/records/3484592",
Dataset_description = "This dataset documents one site near
Bologna within a Natura 2000 protected site. The site presents
a mosaic vegetation growing on clay soil, composed of xeric
woods and shrubs surrounded by grasslands. Each year the
site was visited once a month from March or April to September,
and in each visit we measured both plant-pollinator
interactions along a fixed transect and estimated flower
abundance. The transect included both the grassland and the
wood fringe areas. We covered the full phenology of most plants
and pollinators and measured all floral visitors. Four years
of data are available.",
Taxa_recorded = "Hymenoptera, Coleoptera and Diptera (Bombyliidae and Syrphidae)",
Sampling_year = "2011 to 2014",
Country = "Italy",
Habitat = "Grassland",
Sampling_sites = 1,
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
Floral_counts =  "No")


#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Alessandro Fisogni", "Marta Galloni"),
  Orcid = c("0000-0001-6179-2767", "0000-0001-5304-7820"),
  E_mail = c("a.fisogni@gmail.com", "marta.galloni@unibo.it"))

#Save data ----
#Create list with all dataframes of interest
Karise <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Karise) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 17
saveRDS(Karise, file="Data/2_Processed_data/17_Fisogni.rds") 


