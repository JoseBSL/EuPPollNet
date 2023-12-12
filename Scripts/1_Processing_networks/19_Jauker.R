#DATASET NUMBER 19; Birgit Jauker
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/19_Jauker/Interaction_data.csv")

#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Compare vars
compare_variables(check_interaction_data, data)

#Prepare data
data = data %>% 
select(!c(Pollinator_genus, Plant_genus, Habitat_area)) %>% #delete extra cols 
group_by(Site_id, Plant_species, Pollinator_species, Longitude, Latitude) %>%  #aggregate to sum interactions
summarise(Interaction = n()) %>%  #Generate sum of interactions col by site
ungroup() %>% 
mutate(Sampling_method = "Transects") %>% 
mutate(Sampling_effort_minutes = "REVIEW") %>% 
mutate(Sampling_area_square_meters = "REVIEW") %>% 
mutate(Country = "Germany") %>% 
mutate(Habitat = "Calcareous grasslands") %>% 
mutate(Locality = "Goettingen") %>% 
mutate(Coordinate_precision = NA) %>% 
mutate(Elevation = NA) %>% 
mutate(Day = NA) %>% 
mutate(Month = NA) %>% 
mutate(Year = 2004) %>% 
mutate(Comments = NA) %>% 
mutate(Temperature = NA) %>% 
mutate(Humidity = NA) %>% 
filter(!is.na(Latitude) & !is.na(Longitude))

#Safety check 
sum(is.na(data$Latitude)) # There is one site without coordinate that won't be added
str(data)

#Reorder variables
data <- drop_variables(check_interaction_data, data) 

#Drop this last two columns that are going to be at the metadata
data = data %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Unify level
data = data %>% 
mutate(Sampling_method = "Transect")

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data,data$Site_id)

#Prepare flower count data ----
FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = NA, Plant_species = NA,
                      Flower_count = NA, Units = NA, Comment = NA)


#Prepare metadata data ----

#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Create ordered metadata
Metadata <- tibble(
Doi = "https://doi.org/10.1002/ecy.2569",
Dataset_description = "Pollinator-plant interaction records from 32
calcareous grassland sites near Goettingen, Germany. Records for each
site are aggregated over 6 sampling events conducted between April and
September 2004.",
Taxa_recorded = "Wild bees and hoverflies",
Sampling_year = 2004,
Country = "Germany",
Habitat = "Calcareous grassland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = 6,
Sampling_method = "Transect",
Sampling_area_details = "Transect length varied (mean = 15.7 +/- 6.2 sd) and width of 4m",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA,
Sampling_time_details = "5 mins per transect but transect number varied per site size",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = (20 + 40 + 60) * 6, #time spent in the different sites 20, 40 and 60 x 6 rounds
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "No")


#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()


#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = "Birgit Jauker",
  Orcid = "0000-0001-5027-9351",
  E_mail = "birgit.jauker@allzool.bio.uni-giessen.de")

#Save data ----
#Create metadata list
Jauker <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Jauker) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Jauker, file="Data/Clean_data/19_Jauker.rds")

