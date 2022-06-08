
#DATASET NUMBER 1----

#Install Mangal
#remotes::install_github("ropensci/rmangal", force = TRUE)

#Load libraries
library(rmangal)
library(tidyverse)

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

#Create dataframe with the cols of interest
data <- bind_cols(polls,plants, Freq) %>% 
  select(Plant_species, Pollinator_species, Interaction) %>%
  mutate(Sampling_method = "Focal_observations") %>%
  mutate(Sampling_effort_minutes = (6*nlevels(factor(data$Plant_species)))) %>% 
  mutate(Sampling_area_square_meters = 0.09) %>%
  mutate(Site_id = network$name) %>%
  mutate(Habitat = "Coastal shrubland") %>%
  mutate(Country = "Spain") %>%
  mutate(Locality = "Cap de Creus Natural Park") %>%
  mutate(Latitude = network$geom_lat) %>%  
  mutate(Longitude = network$geom_lon) %>%
  mutate(Coordinate_precision = "<100m") %>%
  mutate(Elevation = NA) %>% 
  bind_cols(Date) %>% 
  mutate(Comments = NA) %>%
  mutate(Temperature = NA) %>%
  mutate(Humidity = NA) 

InteractionData[[i]] <- data


}


#Read flower counts
flower_count <- read_csv("Data/Raw_data/Bartomeus/1_Flower_count.csv")

#Prepare data
date_flower_count <- flower_count %>% 
  separate(Date, sep="/", into = c("Day", "Month", "Year")) %>%
  select(Day, Month, Year)

#Add leading 0's
date_flower_count$Month <- ifelse(as.numeric(date_flower_count$Month) < 10, paste0("0", date_flower_count$Month), date_flower_count$Month)
date_flower_count$Year <- 2005

#Create FlowerCount dataset
flower_count <- date_flower_count %>% 
  bind_cols(flower_count) %>% 
  rename(Site_id = Site) %>% 
  rename(Flower_count = Flower_numer_in_transect) %>%
  select(Day, Month, Year, Site_id, Plant_species, Flower_count) %>%
  mutate(Units = "Flower_units") %>%
  mutate(Comment = NA)

#Create Metadata dataset
meta <- data.frame(
  Doi = "https://doi.org/10.1007/s00442-007-0946-1",
  Dataset_description = "Study site at Cap de Creus, Catalonia, Spain. 
Sampling method of survey is 6 minuts per plant species. 
All plant species where sampled for equal time period. 
All floral visitors were collected. 
We determined the total number of flowers or inflorescences (fower units) per plant species in 1-m2 areas located at 1-m intervals along the two 50 m transects. 
Flower unit description is defined as the distance that a small bee (c. 1 cm length) would fly, rather than walk (Saville 1993). 
For example, in the Asteraceae, a flower unit is the entire inflorescence while in the Rosaceae, a flower unit is a single flower.
Publications with the dataset: Bartomeus et al. 2008 Oecologia. 
Networks where only sampled April to June for Carpobrotus and June/July for Opuntia, Sites are invaded or non invaded, and 50*50m; situated at least 300 m apart.",
  Taxa_recorded = "All floral visitors")

#Create Authorship dataset
authors <- data.frame(
  Coauthor_name = "Ignasi Bartomeus",
  Orcid = "0000-0001-7893-4389",
  E_mail = "nacho.bartomeus@gmail.com")

#Create metadata list
Metadata <- list(meta) 
Authorship <- list(authors) 
FlowerCount <- list(flower_count) 

Bartomeus <- list(InteractionData, FlowerCount, Metadata, Authorship)

names(Bartomeus) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Bartomeus, file="Data/Clean_data/1_Bartomeus.RData")

