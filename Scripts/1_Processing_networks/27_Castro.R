#DATASET NUMBER 27; Castro 

#Read templates to compare with
source("Scripts/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)
library(stringr)
#Load function to unify structure of data
source("Scripts/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/26_27_Castro/Interaction_data2.csv")

#Check col names with template
compare_variables(check_interaction_data, data)

#Select main cols
data = data %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list (separate now by site)
InteractionData2 <- split(data, data$Habitat)

#Prepare flower count data ----
FlowerCount2 = tibble(Day = NA, Month = NA, Year = NA, Site_id = NA, Plant_species = NA,
                      Flower_count = NA, Units = NA, Comment = NA)


#Select unique cases of plants and poll
plant_single_cases2 = data %>% distinct(Plant_species)
pollinator_single_cases2 = data %>%distinct(Pollinator_species)

#Create ordered metadata
Metadata2 <- tibble(
Doi = "Unpublished",
Dataset_description = "This dataset documents 4 different sites in the surroundings of
a sweet cherry orchards from Castelo Branco district. Plant-pollinator interactions were
measured through census, with sampling during one day per habitat (2 hours in the morning and 2
hours in the afternoon) in March, April and May.",
Taxa_recorded = "All flower visitors",
Sampling_year = 2021,
Country = "Portugal",
Habitat = "Pasture",
Sampling_sites = "4",
Sampling_rounds = NA,
Sampling_method = "Random walks",
Sampling_area_details = NA,
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA,
Sampling_time_details = "2 hours in the morning and 2 in the afternoon per round and site",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = NA,
Total_plant_species = nrow(plant_single_cases2),
Total_pollinator_species = nrow(pollinator_single_cases2),
Floral_counts =  "No")

#Transpose metadata
Metadata2 = as.data.frame(t(Metadata2)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship2 <- tibble(
Coauthor_name = c("Catarina Siopa", "Sara Lopes", "Sílvia Castro", "Hugo Gaspar", "João Loureiro"),
Orcid = c("0000-0002-5289-9287", "0000-0002-5024-640X", "0000-0002-7358-6685", "0000-0001-5448-8396", "0000-0002-9068-3954"),
E_mail = c("catarinasiopa@gmail.com", "saralopes2295@gmail.com ", "scastro@bot.uc.pt", "hgaspar@uc.pt", "jloureiro@bot.uc.pt"))

#Save data ----
#Create metadata list
Castro2 <- list(InteractionData2, list(FlowerCount2), Metadata2, Authorship2)
names(Castro2) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Castro2, file="Data/Clean_data/27_Castro.rds")


