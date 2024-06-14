#DATASET NUMBER 52; Hadrava
#Dataset sent by Jiří Hadrava

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)

#Read empty templates to compare with
source("Scripts/Processing/Functions/Empty_templates.R") 
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read_csv("Data/1_Raw_data/52_Hadrava/Interaction_data.csv", locale = locale(encoding = "UTF-8"))
#Select columns of interest
data = data %>% 
select(network_ID, animal, plant, 
       real_pollinator, `number of records`)
#Exclude records that are not pollinators
data = data %>% 
filter(!real_pollinator == 0) %>% 
select(!real_pollinator)
#Rename columns
data = data %>% 
rename(Plant_species = plant,
       Pollinator_species = animal)
#Uncount records
data = data %>% 
uncount(`number of records`)
#Delete underscore on species names
data = data %>% 
mutate(Plant_species = str_replace_all(Plant_species, "_", " ")) %>% 
mutate(Pollinator_species = str_replace_all(Pollinator_species, "_", " "))
#Rename column
data = data %>% 
rename(Site_id = network_ID)
#Add interaction column
data = data %>% 
mutate(Interaction = 1) %>% 
mutate(Sampling_method = "Transect") %>% 
mutate(Flower_data = "No") %>% 
mutate(Flower_data_merger = Site_id)  

#Now recover dates and location
locality_data = read_csv("Data/1_Raw_data/52_Hadrava/Locality_data.csv", locale = locale(encoding = "UTF-8"))

#Rename countries
locality_data = locality_data %>% 
mutate(country = 
  case_when(
  country == "CZ" ~ "Czech Republic",
  country == "AT" ~ "Austria",
  country == "UA" ~ "Ukraine",
  country == "RS" ~ "Serbia",
  country == "MK" ~ "North Macedonia",
  country == "HU" ~ "Hungary",
  country == "BG" ~ "Bulgaria",
  T ~ country)) %>% 
rename(Country = country)
#Rename columns of interest
locality_data = locality_data %>% 
rename(Site_id = network_ID) %>% 
rename(Locality = site) %>% 
rename(Year = year) %>% 
rename(Month = month) %>% 
rename(Day = day) %>% 
rename(Habitat = habitat_type) %>% 
rename(Latitude = lat) %>% 
rename(Longitude = lon) %>% 
mutate(Habitat = str_to_title(Habitat))
#Select columns of interest
locality_data = locality_data %>% 
select(Country, Site_id, Locality, Year, Month, Day,
       Habitat, Latitude, Longitude)
#Data ready for merge with interaction data
data = left_join(data, locality_data)

#Compare vars
#compare_variables(check_interaction_data, data)
#Add missing variables
data = add_missing_variables(check_interaction_data, data) 
data = drop_variables(check_interaction_data, data) 

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Order data by site_id
levels_site_id = unique(as.factor(data$Site_id))

#Unify structure of data
data = change_str(data)
data$Site_id = factor(data$Site_id, levels = levels_site_id)

#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)

#Prepare flower count data ----
#We need to create a list of lists too
#Check levels of Site_id
site_id_levels = levels(factor(data$Site_id))

FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = site_id_levels, Plant_species = NA,
                     Flower_count = NA, Units = NA, Comments = NA, Flower_data_merger = site_id_levels)
#Set common structure
FlowerCount = change_str2(FlowerCount)
FlowerCount$Site_id = factor(FlowerCount$Site_id, levels = site_id_levels)

#Split by Site_id
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Create ordered metadata
Metadata <- tibble(
Doi = "http://hdl.handle.net/20.500.11956/81227",
Dataset_description = "I provide 49 small pollination networks
from various sites across central and
eastern Europe. The idea of the study was to get as
small networks as possible (sampled within small and homogeneous spatial and temporal scale),
so the structure of the network is not affected by spatial or
temporal turnover of species -there are no missing links sensu Olesen et al. 2011-.
Each network was sampled in standardised area of 200 m^2.
The area was delimited by 50 m long rope and plant-pollinator interactions
were sampled on all flowering plants within distance of 2 meters
from the rope (on both sides from the rope).
The interactions from each side of the rope were recorded
separatedly, so we can divide each network into 2 sub-networks
and use them for cross-validation. Terms of use:
The data have not been published yet (I have many more samples where species
have not been identified yet and I’d like to carry out
the analyses once I’ll have the whole dataset.
If the data would be useful for some other project,
I’m willing to provide them, but as they are not published yet,
I would like to be informed about the purpose for which they will
be used and I would like to get an opportunity to become co-author of any
publication where the data are used.",
Taxa_recorded = "All flower visitors",
Sampling_year = 2015,
Country = "Many",
Habitat = "Many",
Sampling_sites = nlevels(factor(data$Site_id)),
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
Floral_counts =  "No")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- tibble(
Coauthor_name = c("Jiří Hadrava"),
Orcid = c("0000-0003-3247-6399"),
E_mail = c("HadravaJirka@seznam.cz"))

#Save data ----
#Create metadata list
Hadrava <- list(InteractionData, FlowerCount, Metadata, Authorship)
names(Hadrava) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Hadrava, file="Data/2_Processed_data/52_Hadrava.rds")



