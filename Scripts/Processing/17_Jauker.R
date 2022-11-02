#DATASET NUMBER 19; Birgit Jauker (script 17)
source("Scripts/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/17_Jauker/Interaction_data.csv")

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

#Split interaction data into dataframes within a list
InteractionData <- split(data,data$Site_id)

#Prepare flower count data ----
FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = NA, Plant_species = NA,
                      Flower_count = NA, Units = NA, Comment = NA)


#Prepare metadata data ----
Metadata <- tibble(
  Doi = "https://doi.org/10.1002/ecy.2569",
  Dataset_description = "Pollinator-plant interaction records from 32
  calcareous grassland sites near Goettingen, Germany. Records for each
  site are aggregated over 6 sampling events conducted between April and
  September 2004.",
  Taxa_recorded = "Wild bees and hoverflies")

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

