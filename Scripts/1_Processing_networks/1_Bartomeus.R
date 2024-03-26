#DATASET NUMBER 1; Bartomeus

#Load libraries
library(rmangal)
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")


#Prepare interaction data ----
#Install Mangal
#remotes::install_github("ropensci/rmangal", force = TRUE)
#search_references function not working, I'm having an issue with the certificate
#This workaround fix the issue
library(httr)
library(curl)
set_config(config(ssl_verifypeer = FALSE))
options(RCurlOptions = list(ssl_verifypeer = FALSE))
options(rsconnect.check.certificate = FALSE)

#Make the search now by DOI
bart <- search_references(doi = "10.1007/s00442-007-0946-1")

#Download data
bart <- get_collection(bart)

#Save data in a list 
InteractionData <- list() 

for (i in 1:12){

#Prepare data to create network
nodes <- bart[[i]]$nodes
interactions <- bart[[i]]$interactions 
network <- bart[[i]]$network 

#To add frequency of interactions later
Freq <- as_tibble(interactions$value) %>% rename(Interaction = value)
#To add coordinates later
Lat <- as_tibble(network) %>% rename(Latitude = geom_lat)
Long <- as_tibble(network) %>% rename(Longitude = geom_lon)
#To add date
Date <- interactions %>% select(date) %>% mutate(date = as.Date(interactions$date, format =  "%Y-%m-%d")) 
#Split date into three columns
Date <- Date %>% separate(date, sep="-", into = c("Year", "Month", "Day")) %>%
  select(Day, Month, Year)


#Create col with pollinator names
n_from <- as_tibble(nodes) %>% select(node_id, taxonomy.name) %>% rename(node_from = node_id)
i_from <- as_tibble(interactions) %>% select(node_from) 
polls <- inner_join(i_from,n_from) %>% rename(Pollinator_species = taxonomy.name)

#Create col with plant names
n_to <- as_tibble(nodes) %>% select(node_id, taxonomy.name) %>% rename(node_to = node_id)
i_to <- as_tibble(interactions) %>% select(node_to) 
plants <- inner_join(i_to,n_to) %>% rename(Plant_species = taxonomy.name)

#Create dataframe with the cols of interest ordered
data <- bind_cols(polls,plants, Freq) %>% 
  select(Plant_species, Pollinator_species, Interaction) %>%
  mutate(Sampling_method = "Focal_observation") %>%
  mutate(Site_id = network$name) %>%
  mutate(Habitat = "Coastal shrubland") %>%
  mutate(Country = "Spain") %>%
  mutate(Locality = "Cap de Creus Natural Park") %>%
  mutate(Latitude = as.numeric(unique(network$geom_lat))) %>%  
  mutate(Longitude =  as.numeric(unique(network$geom_lon))) %>%
  mutate(Coordinate_precision = "<100m") %>%
  mutate(Elevation = NA) %>% 
  bind_cols(Date) %>% 
  mutate(Comments = NA) %>%
  mutate(Temperature = NA) %>%
  mutate(Humidity = NA) %>% 
  mutate(Site_id = str_replace(Site_id, "bartomeus_2005_", "")) %>% 
  mutate(Flower_data = "Unprocessed") %>% 
  mutate(Flower_data_merger = Site_id)

#Set standard data structure
data = change_str(data)

InteractionData[[i]] <- data

names(InteractionData)[i] <- str_replace(levels(factor(data$Site_id)), "bartomeus_2005_", "")

}

##Prepare flower count data ----
#flower_count <- read_csv("Data/1_Raw_data/1_Bartomeus/Flower_count.csv")
#
##Flower counts
#flower_count = flower_count%>% 
#mutate(Comments = paste0("Round: ", Round,
#      ";Flower_number_in_transect: ", Flower_numer_in_transect))
#
##Prepare data
#date_flower_count <- flower_count %>% 
#  separate(Date, sep="/", into = c("Day", "Month", "Year")) %>%
#  select(Day, Month, Year)
##Add leading 0's
#date_flower_count$Month <- ifelse(as.numeric(date_flower_count$Month) < 10, paste0("0", date_flower_count$Month), date_flower_count$Month)
#date_flower_count$Year <- 2005
##Create FlowerCount dataset
#FlowerCount <- date_flower_count %>% 
#  bind_cols(flower_count) %>% 
#  rename(Site_id = Site) %>% 
#  rename(Flower_count = Flower_numer_in_transect) %>%
#  mutate(Units = "Flower_units") %>% 
#  select(Day, Month, Year, Site_id, Plant_species, Flower_count, Units, Comments)
#
#FlowerCount = change_str2(FlowerCount)
#str(FlowerCount)
#
##Split flower count by Site_id
#FlowerCount = split(FlowerCount, FlowerCount$Site_id)


#Flower counts are not reliable at the moment
#Check levels of Site_id
site_id_levels = levels(factor(bind_rows(InteractionData)$Site_id))

FlowerCount = tibble(Day = NA_character_, Month = NA_character_, Year = NA, Site_id = site_id_levels, Plant_species = NA_character_,
                      Flower_count = NA, Units = NA_character_, Comments = NA_character_,
                     Flower_data_merger = site_id_levels)

#Set common structure
FlowerCount = change_str2(FlowerCount)

#Split by Site_id
FlowerCount = split(FlowerCount, FlowerCount$Site_id)


#Prepare metadata data ----
#Sum species per site to calculate sampling effort
for (i in InteractionData) {
#Generate sum of distinct plants per site
plant_sum <- bind_rows(lapply(InteractionData, function(x) x %>% 
                      select(Plant_species, Site_id) %>% 
                      group_by(Site_id) %>% 
                      summarise(Sum = n_distinct(Plant_species))))

}

#Sum the number of distintc plants per site
plant_sum = sum(plant_sum$Sum)

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Create metadata ordered
Metadata <- tibble(
Doi = "https://doi.org/10.1007/s00442-007-0946-1",
Dataset_description = "Study site at Cap de Creus, Catalonia, Spain.
Sampling method of survey is 6 minuts per plant species. All plant species where sampled
for equal time period. All floral visitors were collected.
We determined the total number of flowers or inflorescences (fower units)
per plant species in 1-m2 areas located at 1-m intervals along the two 50 m transects.
Flower unit description is defined as the distance that a small bee (c. 1 cm length)
would fly, rather than walk (Saville 1993). For example, in the Asteraceae, a flower
unit is the entire inflorescence while in the Rosaceae, a flower unit is a single flower.
Networks where only sampled April to June for Carpobrotus and June/July for Opuntia,
Sites are invaded or non invaded, and 50*50m; situated at least 300 m apart.",
Taxa_recorded = "All floral visitors",
Sampling_year = "2015",
Country = "Spain",
Habitat = "Mediterranean shrubland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "6",
Sampling_method = "Focal observations",
Sampling_area_details = "6 observation areas of 0.3 * 0.3 m / species and site",
Sampling_area_species_m2 = as.character(0.3 * 0.3 * 6 * 12),
Sampling_area_total_m2 = as.character(0.3 * 0.3 * 6 * 12 * plant_sum) ,
Sampling_time_details = "1 min / species, site, round and observation area; 36 min per species and site",
Sampling_time_species_round_min = 6,
Sampling_time_total_min = as.character(36 * plant_sum * 12),
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Unprocessed")

#Transpose metadata, so is in long instead of wide
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- tibble(
  Coauthor_name = "Ignasi Bartomeus",
  Orcid = "0000-0001-7893-4389",
  E_mail = "nacho.bartomeus@gmail.com")

#Save data ----
#Create metadata list
Bartomeus <- list(InteractionData, FlowerCount, Metadata, Authorship)
names(Bartomeus) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Bartomeus, file="Data/2_Processed_data/1_Bartomeus.rds")

