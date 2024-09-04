#DATASET NUMBER 53; Libran
#Dataset sent by  Libran
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)
library(lubridate)
library(anytime)

#Read empty templates to compare with
source("Scripts/Processing/Functions/Empty_templates.R") 
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read_csv("Data/1_Raw_Data/53_Libran-Embid/Interaction_data.csv")
coords = read_csv("Data/1_Raw_Data/53_Libran-Embid/Coordinates.csv")

#Fix coordinates to lat long
library(sf)
# Define the coordinate reference system (CRS) for the input data
# Example: UTM Zone 32n OR 33N which is common in Europe (EPSG:32632, EPSG:32633)
utm_crs <- st_crs(32632) # You need to know the correct CRS of your data
# Convert tibble to an sf object with geometry
sf_object <- st_as_sf(coords, coords = c("X", "Y"), crs = utm_crs)
# Transform coordinates to WGS84 (EPSG:4326) for Latitude/Longitude
sf_transformed <- st_transform(sf_object, crs = 4326)
# Extract the Lat/Long coordinates
lat_long_coords = sf_transformed %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(Longitude = X, Latitude = Y) %>%
  bind_cols(coords) %>% 
rename(Site = Study_site) %>% 
select(Site, Latitude, Longitude)

#Some sites need to be fixed
lat_long_coords = lat_long_coords %>% 
mutate(Site = case_when(
       Site == "Ossenfelder Bahndamm" ~ "Ossenfelder Bahndam",
       Site == "Gieseberg-Süd" ~ "Gieseberg-Sud",
       Site == "Schweineberg" ~ "Schweinberg",
       Site == "Südlicher Riesenberg" ~ "Sudlicher Riesenberg",
       Site == "Mühlenberg" ~ "Muhlenberg",
       Site == "Mühlenberg 2" ~ "Muhlenberg 2",
               T ~ Site))

data = left_join(data, lat_long_coords)

#Check against template
compare_variables(check_interaction_data, data)

data = data %>% 
rename(Site_id = Site,
       Plant_species = Plant,
       Pollinator_species = Visitor) %>% 
mutate(Date = as.Date(Date, format = "%A, %d %B %Y")) %>% 
mutate(Interaction = 1) %>% 
mutate(Sampling_method = "Plots") %>% 
mutate(Sampling_effort_minutes = NA_character_) %>% 
mutate(Sampling_area_square_meters = NA_character_) %>% 
mutate(Habitat = "Calcareous grassland") %>% 
mutate(Country = "Germany") %>% 
mutate(Flower_data = "No") %>% 
mutate(Flower_data_merger = Site_id) %>% 
mutate(Locality = Site_id) %>% 
mutate(Comments = paste0("Plot_", Plot, " Round_", Round)) %>% 
dplyr::mutate(Year = lubridate::year(Date), 
                Month = lubridate::month(Date), 
                Day = lubridate::day(Date)) %>% 
select(!c(Area, Area_2018, Connectivity_Index, Connectivity_Index_2018,
       Included, ID, Family, Genus, Gender, Round, Plot, Date))


data = add_missing_variables(check_interaction_data, data)
data = drop_variables(check_interaction_data, data)

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Let's throw away rows without plants or pollinators
data = data %>% 
filter(!(is.na(Plant_species) | is.na(Pollinator_species)))

#Unify structure of data
data = change_str(data)


#Prepare flower count data ----
#We need to create a list of lists too
#Check levels of Site_id
site_id_levels = levels(factor(data$Site_id))

FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = site_id_levels, Plant_species = NA,
                     Flower_count = NA, Units = NA, Comments = NA,  Flower_data_merger = site_id_levels)

#Set common structure
FlowerCount = change_str2(FlowerCount)

#Split by Site_id
FlowerCount <- split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----

#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata <- tibble(
Doi = "https://doi.org/10.1111/ele.13892",
Dataset_description = "Plant-pollinator interactions from calcareous grasslands located in central Germany.
Observation plots were circular (3 m radius, 28.3 m2) and were established in flower-rich areas",
Taxa_recorded = "Hymenoptera and Lepidoptera",
Sampling_year = "2017 to 2018",
Country = "Germany",
Habitat = "Calcareous grasslands",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = NA,
Sampling_method = "Plots",
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
  Coauthor_name = c("Felipe Librán-Embid", "Ingo Grass", "Teja Tscharntke"),
  Orcid = c("0000-0003-4898-4494", "0000-0001-7788-1940", "0000-0002-4482-3178"),
  E_mail = c("feliem3@gmail.com", "ingo.grass@uni-hohenheim.de", "ttschar@gwdg.de"))

#Save data ----
#Create list with all dataframes of interest
libran_embid <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(libran_embid) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 17
saveRDS(libran_embid, file="Data/2_Processed_data/53_Libran-Embid.rds") 



