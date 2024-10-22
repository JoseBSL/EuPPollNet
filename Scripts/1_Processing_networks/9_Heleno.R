#DATASET NUMBER 9; Heleno

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read.csv("Data/1_Raw_data/9_Heleno/Interaction_data.csv")

data = data %>% 
mutate(Coordinate_precision = str_replace(Coordinate_precision, " ", "")) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) 

#Unify level
data = data %>% 
mutate(Sampling_method = "Focal_observation") %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Flower_data_merger = NA) 

#Create column to merge floral counts
data = data %>%  
mutate(Flower_data_merger = paste0(word(Plant_species,1),word(Plant_species,2), 
                                   Site_id))
#Unify structure of data
data = change_str(data)

#Split by site, just for createing the listed name in this case
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount = read.csv("Data/1_Raw_data/9_Heleno/Flower_count.csv")

#Set common colname
FlowerCount = FlowerCount %>% 
rename(Comments = Comment) %>% 
mutate(Flower_data_merger = NA) 

#Common excel mistake on id, fix
FlowerCount$Site_id = "Coimbra_2017"

#Create column to merge floral counts
FlowerCount = FlowerCount %>%  
mutate(Flower_data_merger = paste0(word(Plant_species,1),word(Plant_species,2), 
                                   Site_id)) 

#Order data as template and drop variables
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#Unify data structure
FlowerCount = change_str2(FlowerCount)
#Split by site, just for creating the listed name in this case
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata = tibble(
Doi = "https://doi.org/10.1007/s00442-018-4281-5",
Dataset_description = "This dataset documents flower visits in winter and
spring in the Botanical Garden of the University of Coimbra.
Interactions have been recorded weekly by performing plant-centered
direct observations between 28 October 2016 and 19 May 2017
(thus including the main flowering season). Observation time was proportional
to flower abundance and to the duration of its flowering period, with a minimum
of two 15 minutes of observations per species per week, and a total observation
time of 255 h. The area includes many exotic and cultivated plants from around the world.
Each week.",
Taxa_recorded = "All flower visitors",
Sampling_year = "2016-2017",
Country = "Portugal",
Habitat = "Botanical garden",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "Twice a week",
Sampling_method = "Focal observations",
Sampling_area_details = NA,
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA,
Sampling_time_details = "15 mins twice per week during the whole flowering season",
Sampling_time_species_round_min = 15,
Sampling_time_total_min = 255 * 60, #255 hours *60 mins
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")


#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship = data.frame(
  Coauthor_name = c("Ruben Heleno", "Francisco López-Núñez", "Catherine J. O’Connor"),
  Orcid = c("0000-0003-2297-006X", "0000-0003-0773-9134", NA),
  E_mail = c("rheleno@uc.pt", "lnfran85@gmail.com", NA))

#Save data ----
#Create list with all dataframes of interest
Heleno = list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Heleno) = c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 9
saveRDS(Heleno, file="Data/2_Processed_data/9_Heleno.rds") 
