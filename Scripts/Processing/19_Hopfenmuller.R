#DATASET NUMBER 21; Bernhard Hopfenmuller (script 19)
source("Scripts/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)
library(lubridate)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/19_Hopfenmuller/Interaction_data.csv")

#Compare vars
compare_variables(check_interaction_data, data)

#Data
data = data %>% 
mutate(Date = as.Date(Date, format="%m-%d-%y")) %>% 
mutate(Year = year(ymd(Date))) %>% 
mutate(Month = month(ymd(Date))) %>% 
mutate(Day = day(ymd(Date))) 

#Add interaction col
data = data %>% 
mutate(Interaction = 1) %>% 
mutate(Sampling_method = "Variable transect walks") %>% 
mutate(Sampling_effort_minutes = NA) %>% 
mutate(Sampling_area_square_meters = NA) %>% 
mutate(Habitat = "Calcareous grassland") %>% 
mutate(Locality = Site_id) 

#Add missing vars
data = add_missing_variables(check_interaction_data, data) 

#Reorder variables
data = drop_variables(check_interaction_data, data) 

#Drop this last two columns that are going to be at the metadata
data = data %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Split interaction data into dataframes within a list
InteractionData <- split(data,data$Site_id)


#Prepare flower count data ----
flower_count <- read_csv("Data/Raw_data/19_Hopfenmuller/Flower_count.csv")

#Compare vars
compare_variables(check_flower_count_data, flower_count)

#Fix names and variable
flower_count = flower_count %>% 
rename(Flower_count = Flower_units) %>% 
mutate(Units = "Flower number")

#Add variables
flower_count = add_missing_variables(check_flower_count_data, flower_count) 

#Order data as template
flower_count = drop_variables(check_flower_count_data, flower_count) 

#Split interaction data into dataframes within a list
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)


#Create ordered metadata
Metadata <- tibble(
Doi = "",
Dataset_description = "",
Taxa_recorded = "",
Sampling_year = NA,
Country = "",
Habitat = "",
Sampling_sites = NA,
Sampling_rounds = "",
Sampling_method = "",
Sampling_area_details = "",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA, 
Sampling_time_details = NA,
Sampling_time_species_round_min = NA,
Sampling_time_total_min = NA,
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

