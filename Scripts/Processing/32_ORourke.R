#DATASET NUMBER 32; O’Rourke
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

#Select first the one of O’Rourke
data = data %>% filter(Name == "O’Rourke")

#To calculate sampling effor later on
data_time = data %>% select(Sampling_effort_minutes)

#Compare vars
compare_variables(check_interaction_data, data)

data = data %>% 
rename(Plant_species = Plant_Species) %>% 
mutate(Habitat = "Dune ecosystem") %>% 
mutate(Locality = "Along south-eastern Ireland") %>% 
rename(Latitude = "WGS84/ETRS89") %>% 
rename(Longitude = "WGS84/ETRS89_2") %>% 
mutate(Site_id = Site_number) %>% 
mutate(Sampling_method = "Focal_observations") #From the paper I understand that is focal and not transect

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
mutate(Day = as.Date(Day, origin = "2011-01-01")) %>% 
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
Doi = "https://doi.org/10.26786/1920-7603(2014)14",
Dataset_description = "Plant-pollinator community from fixed dunes",
Taxa_recorded = "Bees, hoverflies and butterflies",
Sampling_year = "2011",
Country = "Ireland",
Habitat = "Dunes",
Sampling_sites = "8",
Sampling_rounds = NA,
Sampling_method = "Focal observations",
Sampling_area_details = "1m2 per plant species at each 30 min round",
Sampling_area_species_m2 = "1",
Sampling_area_total_m2 = NA,
Sampling_time_details = "90 mins over a two day period",
Sampling_time_species_round_min = "30",
Sampling_time_total_min = sum(as.numeric(data_time$Sampling_effort_minutes)), 
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "No")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Aoife O’Rourke", "Una Fitzpatrick", "Jane C. Stout"),
  Orcid = c(NA, NA, "0000-0002-2027-0863"),
  E_mail = c("orourkat@tcd.ie", "ufitzpatrick@biodiversityireland.ie", "STOUTJ@tcd.ie"))


#Save data ----
#Create metadata list
ORourke <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(ORourke) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(ORourke, file="Data/Clean_data/32_ORourke.rds")



