#DATASET NUMBER 26; Castro 

#Read templates to compare with
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)
library(stringr)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/1_Raw_data/26_27_Castro/Interaction_data1.csv")

#Check col names with template
compare_variables(check_interaction_data, data)
data = add_missing_variables(check_interaction_data, data)
#Reorder variables (just in case)
data = drop_variables(check_interaction_data, data) 

#Unify level
data = data %>% 
mutate(Sampling_method = "Focal_observation")

#Select main cols
data = data %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Column of Site_id does not match flower_count at the moment
#Do it in a two step process
data = data %>% 
mutate(Site_id = str_replace(Site_id, "_CENSO[0-9]+", "")) %>% 
mutate(Site_id = str_replace(Site_id, "_DAY[0-9]+", "")) %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Flower_data_merger = NA)

#Create a merger column
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1),"_", word(Plant_species, 2),"_", Site_id,"_", ifelse(Day < 19, "10-19", "20-22"))) %>% 
mutate(Flower_data_merger = str_replace(Flower_data_merger, "-", "_"))

#Unify structure of data
data = change_str(data)

#Convert into list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount = read_csv("Data/1_Raw_data/26_27_Castro/Flower_count1.csv") %>% 
mutate(Comments = NA) %>% 
mutate(Flower_data_merger = NA)

FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1),"_", word(Plant_species, 2),"_", Site_id,"_", Day )) %>% 
mutate(Flower_data_merger = str_replace(Flower_data_merger, "-", "_"))

#Set common structure
FlowerCount = change_str2(FlowerCount)

#Compare vars
compare_variables(check_flower_count_data, FlowerCount)
FlowerCount = add_missing_variables(check_flower_count_data, FlowerCount)
#Order data (just in case)
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 
#Split flower count data
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases1 = data %>% distinct(Plant_species)
pollinator_single_cases1 = data %>%distinct(Pollinator_species)

#Create ordered metadata
Metadata1 <- tibble(
Doi = "https://doi.org/10.1007/s10530-013-0457-4", #The original doi in the raw dataset seems wrong, I think this is the correct one
Dataset_description = "This dataset documents 1 site in the surroundings of
Coimbra sampled with Oxalis (days10,16,18,19) and without Oxalis pes-capare (days20-22)
the later accomplished after invaders mowing. The focal habitat are agricultural areas in
rural region invaded by Bermuda buttercup. Observations were made in 2011 in a period < 2 weeks,
and we measured plant-pollinator interactions in predefined areas  (6 patches of 2 square meters each).
We recorded all floral visitors observed.",
Taxa_recorded = "All flower visitors, but Coleoptera,
Lepidoptera and Syrphideae have no taxonomic resolution.",
Sampling_year = "2011",
Country = "Portugal",
Habitat = "Agricultural area",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "Flowering season",
Sampling_method = "Focal observation",
Sampling_area_details = "6 square plots of 6m2",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = 12*6,
Sampling_time_details = "277 censuses of 15 mins",
Sampling_time_species_round_min = 15,
Sampling_time_total_min = 15*277,
Total_plant_species = nrow(plant_single_cases1),
Total_pollinator_species = nrow(pollinator_single_cases1),
Floral_counts =  "Yes")

#Transpose metadata
Metadata1 = as.data.frame(t(Metadata1)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship1 <- tibble(
Coauthor_name = c("Sílvia Castro", "Victoria Ferrero", "Joana Costa", 
                  "João Loureiro", "Paola Acuña"),
Orcid = c("0000-0002-7358-6685", "0000-0002-2091-8957", "0000-0002-9784-8272", 
          "0000-0002-9068-3954", "0000-0002-9399-4483"),
E_mail = c("scastro@bot.uc.pt", "vferrv@unileon.es", "jfmc.biologia@gmail.com", 
           "jloureiro@uc.pt", "paola.acuna.perez@gmail.com"))

#Save data ----
#Create metadata list
Castro1 <- list(InteractionData, FlowerCount, Metadata1, Authorship1)
names(Castro1) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Castro1, file="Data/2_Processed_data/26_Castro.rds")


