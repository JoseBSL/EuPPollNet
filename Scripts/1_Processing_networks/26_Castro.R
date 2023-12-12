#DATASET NUMBER 26; Castro 

#Read templates to compare with
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)
library(stringr)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/26_27_Castro/Interaction_data1.csv")

#Check col names with template
compare_variables(check_interaction_data, data)

#Reorder variables (just in case)
data = drop_variables(check_interaction_data, data) 

#Unify level
data = data %>% 
mutate(Sampling_method = "Focal_observation")

#Unify structure of data
data = change_str(data)

#Select main cols
InteractionData1 = data %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Prepare flower count data ----
flower_count1 <- read_csv("Data/Raw_data/26_27_Castro/Flower_count1.csv")

#Compare vars
compare_variables(check_flower_count_data, flower_count1)

#Order data (just in case)
FlowerCount1 = drop_variables(check_flower_count_data, flower_count1) 

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases1 = InteractionData1 %>% distinct(Plant_species)
pollinator_single_cases1 = InteractionData1 %>%distinct(Pollinator_species)

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
Castro1 <- list(list(InteractionData1), FlowerCount1, Metadata1, Authorship1)
names(Castro1) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Castro1, file="Data/Clean_data/26_Castro.rds")


