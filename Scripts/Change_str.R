#Create function to unify colnames

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
mutate(Humidity = as.double(Humidity)) 

}


