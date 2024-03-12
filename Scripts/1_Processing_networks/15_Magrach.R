#DATASET NUMBER 15; Magrach

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/1_Raw_data/15_Magrach/Interaction_data.csv", col_names = T) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Seems to be a mistake with coordinates of site 2
#Check number of levels of each
#Latitude site 2 (most repeated value)
site_2_lat = data %>% 
filter(Site_id == 2) %>%
count(Latitude) %>% 
filter(n == max(n)) %>% 
pull(Latitude)
#Longitude site 2 (most repeated value)
site_2_lon = data %>% 
filter(Site_id == 2) %>%
count(Longitude) %>% 
filter(n == max(n)) %>% 
pull(Longitude)
#Replace values
data = data %>% 
mutate(Latitude = case_when(Site_id == 2 ~ site_2_lat,
                            TRUE ~ Latitude)) %>% 
mutate(Longitude = case_when(Site_id == 2 ~ site_2_lon,
                            TRUE ~ Longitude))

#Add flower coll id
data = data %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Flower_data_merger = NA) 

#Create id to merge with flower count data
data = data %>%  
mutate(Flower_data_merger = paste0(word(Plant_species,1),word(Plant_species,2), word(Plant_species,3),
                                   Site_id, Day, "-", Month, "-", Year)) 

#Unify structure of data
data = change_str(data)

#Split data into different dataframes based on survey name
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount = read_csv("Data/1_Raw_data/15_Magrach/Flower_count.csv", col_names = T)

#Set common colnames
FlowerCount = FlowerCount %>% 
rename(Comments = Comment) %>% 
mutate(Flower_data_merger = NA) 

#Set common structure
FlowerCount = change_str2(FlowerCount)

#Split data into different dataframes based on survey name
FlowerCount <- split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----

#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata <- tibble(
Doi = "Unpublished",
Dataset_description = NA,
Taxa_recorded = "All floral visitors",
Sampling_year = "2020",
Country = "Spain",
Habitat = "Grassland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = NA,
Sampling_method = "Transect",
Sampling_area_details = NA,
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

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Ainhoa Magrach", "Maddi Artamendi", "Paula Dominguez Lapido"),
  Orcid = c("0000-0003-2155-7556", "0009-0003-8534-9235", ""),
  E_mail = c("ainhoa.magrach@bc3research.org", "maddiart19@gmail.com", 
             "pauladl@enebada.eu"))

#Save data ----
#Create list with all dataframes of interest
Magrach <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Magrach) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 15
saveRDS(Magrach, file="Data/2_Processed_data/15_Magrach.rds") 


