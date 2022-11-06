#DATASET NUMBER 7; Scheper, Badenhausser & Kleijn

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data = read.csv("Data/Raw_data/7_Scheper/interaction_data_scheper.csv")

#Standardaze poll species
data = data %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Halictus compressus/langobardicus/simplex", "Halictus sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Osmia", "Osmia sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Osmia leaiana/melanogaster", "Osmia sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Nomada (distinguenda)", "Nomada sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Xylocopa", "Xylocopa sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Syrphidae", "Syrphidae sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Megachile", "Megachile sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Halictus", "Halictus sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Episyrphus", "Episyrphus sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Eristalis", "Eristalis sp."))  %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Bombus", "Bombus sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Bombus terr-complex", "Bombus sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Sphaerophoria", "Sphaerophoria sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Halictus/Lasioglossum", "	Halictidae sp.")) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))


#Split data into different dataframes based on survey name
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
flower_count = read.csv("Data/Raw_data/5_Scheper/flower_count_scheper.csv")

FlowerCount <- split(flower_count, flower_count$Site_id)


#Prepare metadata data ----

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Create metadata in order
Metadata <- tibble(
Doi = "https://doi.org/10.1111/1365-2664.13658",
Dataset_description = "Dataset on bees and hoverflies sampled in 25 grassland
- field boundary pairs in the area around Chize, France, 2015.
Flowers and plant-pollinator interactions were surveyed two to six times a year
(for most sites 4 times), in two 150m2 transects (15 min each)
per habitat on each occasion. Grasslands varied in land use intensity.",
Taxa_recorded = "Bees and hoverflies",
Sampling_year = 2015,
Country = "France",
Habitat = "Grassland",
Sampling_sites = 25,
Sampling_rounds = 4,
Sampling_method = "Transects",
Sampling_area_details = "2 transects per round (transect size 1 or 2 m × 75 m; we use 1.5 m to calculate the total area)",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = 1.5 * 75 * 2 * 25, #Average transect size (1.5) * 75 m long * 2 transects * 25 sites
Sampling_time_details = "15 mins per transect and round",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = 15 * 2 * 4 * 25, #15 mins * 2 transects per site * 4 rounds * 25 sites 
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")
 
#Transpose metadata so it is in long instead of wide format
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()


#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Jeroen Scheper", "Isabelle Badenhausser", "David Kleijn"),
  Orcid = c("0000-0002-4314-996X", "0000-0002-6919-8647", "0000-0003-2500-7164"),
  E_mail = c("jeroen.scheper@wur.nl", "Isabelle.Badenhausser@inrae.fr", "david.kleijn@wur.nl"))

#Save data ----
#Create metadata list
Scheper <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Scheper) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 7
saveRDS(Scheper, file="Data/Clean_data/7_Scheper.rds") 

