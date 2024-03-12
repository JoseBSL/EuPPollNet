#Create functions to fix str of data

#Fix str of data of individual networks
change_str <- function(data) { 
data = data %>% 
mutate(Plant_species = as.character(Plant_species)) %>% 
mutate(Pollinator_species = as.character(Pollinator_species)) %>% 
mutate(Interaction = as.double(Interaction)) %>% 
mutate(Sampling_method = as.character(Sampling_method)) %>% 
mutate(Site_id = as.character(Site_id)) %>% 
mutate(Habitat = as.character(Habitat)) %>% 
mutate(Country = as.character(Country)) %>% 
mutate(Locality = as.character(Locality)) %>% 
mutate(Latitude = as.double(Latitude)) %>% 
mutate(Longitude = as.double(Longitude)) %>% 
mutate(Coordinate_precision = as.double(Coordinate_precision)) %>% 
mutate(Elevation = as.double(Elevation)) %>% 
mutate(Day = as.double(Day)) %>% 
mutate(Month = as.double(Month)) %>% 
mutate(Year = as.double(Year)) %>% 
mutate(Comments = as.character(Comments)) %>% 
mutate(Temperature = as.double(Temperature)) %>% 
mutate(Humidity = as.double(Humidity)) %>% 
mutate(Flower_data = as.character(Flower_data)) %>% 
mutate(Flower_data_merger = as.character(Flower_data_merger)) 

}

#Fix str of data from GBIF
change_str1 <- function(data) { 
data = data %>% 
select(!c(usageKey, confidence, kingdomKey,
          phylumKey, classKey, orderKey, familyKey,
         genusKey,  speciesKey, acceptedUsageKey,
         verbatim_index)) %>% 
rename(Fixed_name = verbatim_name,
       Scientific_name  = scientificName,
       Canonical_name  = canonicalName,
       Accepted_name = species) %>% 
select(Fixed_name, rank, status, matchType, 
       Scientific_name, Canonical_name,
       Accepted_name, phylum, order, family,
       genus) %>% 
  rename_all(~str_to_title(.))
}

#Fix str of data of individual networks
change_str2 <- function(data) { 
#Set common structure
data = data %>% 
mutate(Day = as.double(Day)) %>% 
mutate(Month = as.double(Month)) %>% 
mutate(Year = as.double(Year)) %>% 
mutate(Site_id = as.character(Site_id)) %>% 
mutate(Plant_species = as.character(Plant_species)) %>% 
mutate(Flower_count = as.numeric(Flower_count)) %>% 
mutate(Units = as.character(Units)) %>% 
mutate(Comments = as.character(Comments)) %>% 
mutate(Flower_data_merger = as.character(Flower_data_merger)) 
}
