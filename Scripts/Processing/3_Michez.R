library(tidyverse)

#Read comma delimited
data <- read_csv("Data/Raw_data/Michez/Michez.csv")

#Check colnames and first 5 rows
colnames(data)
data %>% slice_head(n = 5)

#Things to check
#Add sampling area??
#Rename species Bombus cf. terrestris

InteractionData = data %>% 
mutate(Coordinate_precision = NA) %>%
mutate(Elevation = NA) %>% 
mutate(Comments = NA) %>% 
mutate(Temperature = NA) %>% 
mutate(Humidiy = NA) %>% 
select(Plant_species, Pollinator_species, Interaction, 
       Sampling_method, Sampling_effort_minutes, Sampling_area_square_meters,
       Site_id, Habitat, Country, Locality, Latitude, Longitude,
       Coordinate_precision, Elevation, Day, Month, Year, Comments,
       Temperature, Humidiy) %>%

  replace(.=="MISSING", NA)




