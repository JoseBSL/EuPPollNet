
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
  mutate(Sampling_method = "Transect") %>%
  mutate(Sampling_effort_minutes = 6) %>%
  mutate(Sampling_area_square_meters = 100) %>%
  mutate(Site_id = network$name) %>%
  mutate(Habitat = "Coastal shrubland") %>%
  mutate(Country = "Spain") %>%
  mutate(Locality = "Cap de Creus Natural Park") %>%
  mutate(Latitude = network$geom_lat) %>%  
  mutate(Longitude = network$geom_lon) %>%
  mutate(Coordinate_precision = NA) %>%
  mutate(Elevation = NA) %>% 
  bind_cols(Date) %>% 
  mutate(Comments = NA) %>%
  mutate(Temperature = NA) %>%
  mutate(Humidity = NA) %>%
  uncount(Interaction) %>% 
  add_column(Interaction = 1, .after = "Pollinator_species")

InteractionData[[i]] <- data


}

meta <- NULL
meta <- as_tibble(meta) %>% 
  mutate(Doi = NA) %>% 
  mutate(Dataset_description = NA) %>%
  mutate(Taxa_recorded = NA)


authors <- NULL
authors <- as_tibble(authors) %>% 
  mutate(Coauthor_name = NA) %>% 
  mutate(Orcid = NA) %>%
  mutate(E_mail = NA)

  
#Create metadata list
Metadata <- list(meta) 
Authorship <- list(authors) 

Bartomeus_2005 <- list(InteractionData, Metadata, Authorship)

names(Bartomeus_2005) <- c("InteractionData","Metadata", "Authorship")



