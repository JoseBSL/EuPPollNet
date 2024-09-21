#DATASET NUMBER 10; Vanbergen

#Load libraries
library(tidyverse)
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read.csv("Data/1_Raw_data/10_Vanbergen/Interaction_data.csv")

#Fix dates
data = data %>%
separate(Date, into = c("Day", "Month", "Year"), sep = "/")

#Check what is missing and what we have
#compare_variables(check_interaction_data, data)

#Create cols of Temperature and humidity
data = data %>% 
mutate(Temperature = NA) %>% 
mutate(Humidity = NA) %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Flower_data_merger = NA) %>% 
rename(Plant_species = Best.plant.ID) %>% 
rename(Pollinator_species = Best.ID) %>% 
rename(Site_id = Site) %>% 
mutate(Interaction = 1) %>% 
mutate(Country = "France") %>% 
mutate(Locality = "Côte d'or") %>% 
mutate(Sampling_method = "Transect") %>% 
mutate(Comments = paste("sex", Sex)) %>% 
select(!Habitat) %>% 
rename(Habitat = Habitat.EUNIS)
 
#We can add coordinates by loading this other file
#It is a hit chaotic because we restarted the process in order to merge with floral counts
#They were at landscape level but without species
to_add_coordinates = read_csv("Data/1_Raw_data/10_Vanbergen/To_add_coordinates.csv")
#Select main cols to bind
to_add_coordinates = to_add_coordinates %>% 
select(c(Site_id, Latitude, Longitude, Coordinate_precision, Elevation)) %>% 
distinct()

#Join this information to interaction data so we include coordinates
data = left_join(data, to_add_coordinates) 

#Create the Floral merger col
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1),word(Plant_species,2), 
                                   Site_id, Habitat)) 

#Add missing vars
data = add_missing_variables(check_interaction_data, data) 
#Reorder variables
data = drop_variables(check_interaction_data, data) 

#Finally drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#One last thing!
#Drop rows with n/a in poll or plant species
data = data %>%
filter(!Pollinator_species == "n/a")%>%
filter(!Plant_species == "n/a")

#Unify structure of data
data = change_str(data)

#Split by Site_id
InteractionData = split(data, data$Site_id)
#Looks good now :)

##Prepare flower count data ----
FlowerCount = read_csv("Data/1_Raw_data/10_Vanbergen/Flower_count.csv")

#Note!
#There are rare species not included in the quadrat
#Maybe they could be given a low symbolic value

#Compare vars
compare_variables(check_flower_count_data, FlowerCount)

#Rename vars
FlowerCount = FlowerCount %>% 
mutate(Day = NA) %>% 
mutate(Month = NA) %>% 
mutate(Year = NA) %>% 
rename(Plant_species = Flower.species) %>% 
mutate(Flower_data_merger = NA) %>% 
mutate(Comments = "Spp without counts are rare and could assigned a low representative value -check script of floral counts in raw data folder for further info-") %>% 
rename(Site_id = Site) %>% 
mutate(Units = "Mean flowers per quadrat and transect corrected by sampling effort") %>% 
rename(Flower_count = Weighted.floral.ab) %>% 
rename(Habitat = Habitat.EUNIS) %>% 
mutate(Plant_species = str_replace(Plant_species, "[.]", " "))

#Create column to merge floral counts 
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1),word(Plant_species,2), 
                                   Site_id, Habitat)) 


#Order data as template and drop variables
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#Set common structure
FlowerCount = change_str2(FlowerCount)

#keep 1 value of flower count
##Calculate average flowers per site 
FlowerCount = FlowerCount %>%
group_by_at(vars(-c(Flower_count, Day, Month, Year))) %>%
summarise(Flower_count = mean(Flower_count))

#Split by Site_id
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----
#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata <- tibble(
Doi = NA,
Dataset_description = "This dataset is part of the VOODOO-project.
It consists of plant-pollinator networks in 12 different landscapes
(4 agricultural, 4 rural, 4 urban), sampled over 3 periods in 2021
(mid april, end may and mid july). Coordinates are the centroids of each landscape.
Transects were stratified in relation with the area of each type of pollinator habitat
within a 500 m radius around the centroid. Habitat is given for each individual catch,
not for the landscape as a whole. Transect length and duration are therefore pooled in
this file, rather than giving the sampling time in each habitat
(in total: 120 min per site visit, so 360 min in total, if the networks are pooled
over the 3 sampling periods). If requested, we can also provide sampling time from
each subtransect in different habitat types.",
Taxa_recorded = "All flying insects, except really small stuff (< 1 mm)",
Sampling_year = 2021,
Country = "France",
Habitat = "4 agricultural, 4 rural, 4 urban",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = 3,
Sampling_method = "Transect",
Sampling_area_details = NA,
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA,
Sampling_time_details = NA,
Sampling_time_species_round_min = 120,
Sampling_time_total_min = 120 * 3 * 12, #120 mins * 3 rounds * 12 sites
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Unprocessed")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Adam J. Vanbergen", "Willem Proesmans", "Emeline Felten", "Emilien Laurent", "Nathan Cyrille"),
  Orcid = c("0000-0001-8320-5535", "0000-0003-0358-6732", "0009-0005-8159-9426", "", "0009-0003-1102-928X"),
  E_mail = c("adam.vanbergen@inrae.fr", "willem.proesmans@gmail.com", "emeline.felten@inrae.fr",
             "emilien.laurent@inrae.fr", "nathan.cyrille.pro@gmail.com"))

#Save data ----
#Create metadata list
Vanbergen <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Vanbergen) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 10
saveRDS(Vanbergen, file="Data/2_Processed_data/10_Vanbergen.rds") 
