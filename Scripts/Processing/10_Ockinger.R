#DATASET NUMBER 12; Ockinger

#Load libraries
library(tidyverse)


#Prepare interaction data ----
#Load interaction data
data = read_csv2("Data/Raw_data/10_Ockinger/interaction_data_ockinger.csv")

#Prepare interaction data with the same number of columns
data = data %>% 
remove_rownames() %>%
column_to_rownames(var = 'X1') %>% 
rename(Plant_species = flw_visited_by_pollin) %>% 
rename(Pollinator_species = pollin_species) %>% 
drop_na(Plant_species) %>% 
rename(Interaction = Visit) %>% 
mutate(Sampling_method = "Transect") %>% 
mutate(Year = "2021") %>% 
mutate(Month = NA) %>% 
mutate(Day = NA) %>% 
mutate(Sampling_effort_minutes = NA) %>% 
mutate(Sampling_area_square_meters = 200) %>% 
mutate(Country = "Sweeden") %>% 
mutate(Locality = NA) %>% 
mutate(Coordinate_precision = NA) %>% 
mutate(Elevation = NA) %>% 
mutate(Comments = NA) %>% 
mutate(Temperature = NA) %>% 
mutate(Humidity = NA) 

#Coordinates are in another dataset, load it
coord = read_csv("Data/Raw_data/10_Ockinger/coordinate_data_ockinger.csv") %>% 
rename(Latitude = Lat_WGS84) %>% 
rename(Longitude = Long_WGS84) %>% 
select(Name, site_ID, Latitude, Longitude)
#glimpse(coord)

data = left_join(data, coord, by = c("PlatsID" = "site_ID")) %>% 
rename(Site_id = Site_name) 

#Habitat type can be added from this other dataset
habitat = read_csv2("Data/Raw_data/10_Ockinger/site_landscape_data.csv") %>% 
select(Category, PlatsID)

#Bind datasets  
data = left_join(data, habitat, by = c("PlatsID" = "PlatsID")) %>% 
rename(Habitat = Category)  %>% 
select(Plant_species, Pollinator_species, Interaction, Sampling_method,
         Sampling_effort_minutes, Sampling_area_square_meters,
         Site_id, Habitat, Country, Locality, Latitude, Longitude,
         Coordinate_precision, Elevation, Day, Month, Year, Comments,
         Temperature, Humidity)

#Relocate col in its position
data = data %>%
group_by(across(-Interaction)) %>% 
summarise(Interaction = sum(Interaction, na.rm = TRUE)) %>%    
relocate(Interaction, .after = Pollinator_species) %>% 
mutate(Plant_species = str_replace(Plant_species, "_", " ")) %>% 
mutate(Pollinator_species = str_replace(Pollinator_species, "_", " "))

#Split by site, just for createing the listed name in this case
InteractionData <- split(data, data$Site_id)


#Prepare flower count data ----
#I have aggregated the int data by site and I'm going to do the same here
flower_count = read_csv("Data/Raw_data/10_Ockinger/flower_counts_ockinger.csv")

flower_count = flower_count %>% 
rename(Plant_species = plantsp_abund) %>% 
mutate(Plant_species = str_replace(Plant_species, "_", " ")) %>% 
group_by(Site_name, Plant_species) %>% 
summarise(Flw_abund = sum(Flw_abund)) %>% 
ungroup() %>% 
mutate(Day = NA) %>% 
mutate(Month = NA) %>% 
mutate(Year = NA) %>% 
rename(Site_id = Site_name) %>% 
rename(Flower_count = Flw_abund) %>% 
mutate(Units = "Flowers per square meter, 3 per transect. 12 in total per site") %>% 
mutate(Comment = NA) %>% 
select(Day, Month, Year, Site_id, Plant_species, Flower_count, Units, Comment)

#Split by site, just for createing the listed name in this case
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
Metadata <- tibble(
Doi = "https://doi.org/10.1038/s41598-019-51912-4",
Dataset_description = "38 semi-natural pastures in south central Sweden. There were
10 abandoned, 18 restored and 10 continuously grazed semi-natural pastures. The pastures
were selected along a connectivity gradient to continuously grazed semi-natural
grasslands in the surrounding landscapes, and restored pastures ranged in time since
restoration from 2–16 years.",
Taxa_recorded = "Bees and hoverflies")

#Prepare authorship data ----
Authorship <- data.frame(
Coauthor_name = c("Erik Öckinger ", "Marie Winsa"),
Orcid = c("0000-0001-7517-4505", "0000-0001-5200-0027"),
E_mail = c("erik.ockinger@slu.se", "marie.winsa@gmail.com"))

#Save data ----
#Create list with all dataframes of interest
Heleno <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Heleno) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 12
saveRDS(Heleno, file="Data/Clean_data/12_Ockinger.RData") 

