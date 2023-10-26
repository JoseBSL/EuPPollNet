#DATASET NUMBER 35; Mullen
#ALL datasets were gathered by Laura Russo who organised this
#(There are 6 in total)
#One is an unpublished thesis

#Read empty templates to compare with
source("Scripts/Empty_templates.R") 

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)
#Load function to unify structure of data
source("Scripts/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/32_to_37_Russo/Interaction_data.csv")

#Divide col into cols
levels(factor(data$Site_id))
data = data %>% separate(Site_id, c("Name", "Site", "Site_number"), remove = F)
levels(factor(data$Name))

#Recode authors to just their surname
data = data %>% 
  mutate(Name = recode_factor(Name,  "AoifeO" = "O’Rourke",
                              "EileenPower" = "Power",
                              "DaraStanley" = "Stanley",
                              "SarahMullen" = "Mullen",
                              "MichelleLarkin" = "Larkin",
                              "CianWhite" = "White"))

#Select first the one of Power
data = data %>% filter(Name == "Mullen")

#To calculate sampling effort later on
data_time = data %>% select(Sampling_effort_minutes)

#Compare vars
#compare_variables(check_interaction_data, data)

#Prepare data
data = data %>% 
  rename(Plant_species = Plant_Species) %>% 
  mutate(Habitat = "Farming grassland") %>% 
  mutate(Locality = "Farms in south-east Ireland") %>% 
  rename(Latitude = "WGS84/ETRS89") %>% 
  rename(Longitude = "WGS84/ETRS89_2") %>% 
  mutate(Site_id = Site) %>% 
  mutate(Sampling_method = "Transects") 

#Add missing vars
data = add_missing_variables(check_interaction_data, data) 
#Reorder variables
data = drop_variables(check_interaction_data, data) 

#Convert degree minute second to decimal degree coordinates
library(parzer)

#Prepare data before convert coordinates
data = data %>% 
  mutate(Latitude = gsub("\\°.", "° ", Latitude)) %>% 
  mutate(Latitude = gsub("\\'.", "' ", Latitude)) %>% 
  mutate(Longitude = gsub("\\°.", "° ", Longitude)) %>% 
  mutate(Longitude = gsub("\\'.", "' ", Longitude)) 
#Convert to decimal lat/long coordinates
coord = parzer::parse_lon_lat(lon = data$Longitude, lat = data$Latitude)

#Add correct coord col to the interaction dataset
data = data %>% 
  mutate(Longitude = coord$lon) %>% 
  mutate(Latitude = coord$lat)

#Finally drop sampling effort and square area (it would be added on the metadata)
data = data %>%
  select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Try to fix days
data = data %>% 
  mutate(Day = as.Date(Day, origin = "2010-01-01")) %>% 
  mutate(Day = format(as.Date(Day,format="%Y-%m-%d"), format = "%d"))
#it seems ok

#Delete underscore from plant species
data = data %>% 
mutate(Plant_species = str_replace(Plant_species, "_", " "))

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ---- The data wasn't collected
flower_count <- read_csv("Data/Raw_data/32_to_37_Russo/Flower_count.csv")

#Divide col into cols
levels(factor(flower_count$Site_id))
flower_count = flower_count %>% separate(Site_id, c("Name", "Site", "Site_number"), remove = F)
levels(factor(flower_count$Name))

#Recode authors to just their surname
flower_count = flower_count %>% 
  mutate(Name = recode_factor(Name,  "AoifeO" = "O’Rourke",
                              "EileenPower" = "Power",
                              "DaraStanley" = "Stanley",
                              "SarahMullen" = "Mullen",
                              "MichelleLarkin" = "Larkin",
                              "CianWhite" = "White"))

#Select first the one of Power
flower_count = flower_count %>% 
  filter(Name == "Mullen") %>% 
  mutate(Site_id = Site) %>% 
  mutate(Comment = "Also available FloralArea in mm2")

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#Order data as template
flower_count = drop_variables(check_flower_count_data, flower_count) 

#Split interaction data into dataframes within a list
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
  Doi = "Unpublished",
  Dataset_description = NA,
  Taxa_recorded = "Bees, hoverflies and butterflies",
  Sampling_year = "2010 and 2011",
  Country = "Ireland",
  Habitat = NA,
  Sampling_sites = 19,
  Sampling_rounds = NA,
  Sampling_method = "Transects",
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
  Coauthor_name = c("Sara Mullen"),
  Orcid = NA,
  E_mail = c("seramullen@gmail.com"))

#Save data ----
#Create metadata list
Mullen <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Mullen) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Mullen, file="Data/Clean_data/35_Mullen.rds")

