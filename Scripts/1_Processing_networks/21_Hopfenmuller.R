#DATASET NUMBER 21; Bernhard Hopfenmuller 
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)
library(lubridate)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read_csv("Data/1_Raw_data/21_Hopfenmuller/Interaction_data.csv")

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
mutate(Locality = Site_id) %>% 
mutate(Flower_data = "No") %>% 
mutate(Flower_data_merger = Site_id)

#Add missing vars
data = add_missing_variables(check_interaction_data, data) 

#Reorder variables
data = drop_variables(check_interaction_data, data) 

#Drop this last two columns that are going to be at the metadata
data = data %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Unify level
data = data %>% 
mutate(Sampling_method = "Variable_transect")

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data,data$Site_id)


#Prepare flower count data ----
#flower_count <- read_csv("Data/1_Raw_data/21_Hopfenmuller/Flower_count.csv")
#
##Add lat long as comment col in case they are relevant to merge floral counts
#flower_count = flower_count %>% 
#mutate(Flower_data_merger = paste0(word(Plant_species, 1), 
#word(Plant_species, 2),Site_id, Latitude, Longitude))
#
##Compare vars
#compare_variables(check_flower_count_data, flower_count)
#
##Fix names and variable
#flower_count = flower_count %>% 
#rename(Flower_count = Flower_units) %>% 
#mutate(Units = "Flower number")
#
##Add variables
#flower_count = add_missing_variables(check_flower_count_data, flower_count) 
#
##Order data as template
#flower_count = drop_variables(check_flower_count_data, flower_count) 
##Set common structure
#flower_count = change_str2(flower_count)
##Split interaction data into dataframes within a list
#FlowerCount <- split(flower_count, flower_count$Site_id)

site_id_levels = levels(factor(bind_rows(InteractionData)$Site_id))

FlowerCount = tibble(Day = NA_character_, Month = NA_character_, Year = NA, Site_id = site_id_levels, Plant_species = NA_character_,
                      Flower_count = NA, Units = NA_character_, Comments = NA_character_,
                     Flower_data_merger = site_id_levels)

#Set common structure
FlowerCount = change_str2(FlowerCount)

#Split by Site_id
FlowerCount = split(FlowerCount, FlowerCount$Site_id)


#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)


#Create ordered metadata
Metadata <- tibble(
Doi = "https://doi.org/10.1371/journal.pone.0104439",
Dataset_description = "Bee-flower interactions in calcareous grasslands. 
Authors recorded interactions of Wild bees (honey bees excluded) and flowering plants
(excluding trees and graminoids); note that plants were quantified in
terms of flowering units, not area",
Taxa_recorded = "Hymenoptera",
Sampling_year = 2010,
Country = "Germany",
Habitat = "Calcareous grassland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = 5,
Sampling_method = "Variable transect walks",
Sampling_area_details = "0.1 ha per site (maybe 250 m long * 4 m wide?)",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = 1000 * 23, 
Sampling_time_details = NA,
Sampling_time_species_round_min = NA,
Sampling_time_total_min = 45 *5 * 23, #45 mins per site * 5 rounds * 23 sites
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- tibble(
Coauthor_name = c("Sebastian Hopfenmüller", "Ingolf Steffan-Dewenter"),
Orcid = c("0009-0004-5138-6342", "0000-0003-1359-3944"),
E_mail = c("sebastian_hopfenmueller@web.de", "ingolf.steffan@uni-wuerzburg.de"))

#Save data ----
#Create metadata list
Hopfenmuller <- list(InteractionData, FlowerCount, Metadata, Authorship)
names(Hopfenmuller) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Hopfenmuller, file="Data/2_Processed_data/21_Hopfenmuller.rds")
