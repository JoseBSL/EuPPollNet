#DATASET NUMBER 33; Power
#ALL datasets from RUSSO (There are 6 in total)
#One is an unpublished thesis

#Read empty templates to compare with
source("Scripts/Empty_templates.R") 

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)

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
data = data %>% filter(Name == "Power")

#To calculate sampling effor later on
data_time = data %>% select(Sampling_effort_minutes)

#Compare vars
compare_variables(check_interaction_data, data)

#Prepare data
data = data %>% 
rename(Plant_species = Plant_Species) %>% 
mutate(Habitat = "Farming grassland") %>% 
mutate(Locality = "Along south-eastern Ireland") %>% 
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
mutate(Day = as.Date(Day, origin = "2009-01-01")) %>% 
mutate(Day = format(as.Date(Day,format="%Y-%m-%d"), format = "%d"))
#it seems ok

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ---- The data wasn't collected
FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = NA, Plant_species = NA,
                      Flower_count = NA, Units = NA, Comment = NA)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
Doi = "",
Dataset_description = "",
Taxa_recorded = "Bees, hoverflies and butterflies",
Sampling_year = "",
Country = "",
Habitat = "",
Sampling_sites = "",
Sampling_rounds = NA,
Sampling_method = "",
Sampling_area_details = "",
Sampling_area_species_m2 = "",
Sampling_area_total_m2 = NA,
Sampling_time_details = "",
Sampling_time_species_round_min = "",
Sampling_time_total_min = sum(as.numeric(data_time$Sampling_effort_minutes)), 
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c(""),
  Orcid = c(""),
  E_mail = c(""))


#Save data ----
#Create metadata list
Power <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Power) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Power, file="Data/Clean_data/33_Power.rds")

