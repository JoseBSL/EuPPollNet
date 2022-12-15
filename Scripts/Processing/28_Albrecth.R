#DATASET NUMBER 28; Albrecht 

#Read templates to compare with
source("Scripts/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/28_Albrecht/Interaction_data.csv")

#Check col names with template
compare_variables(check_interaction_data, data)

#Rename variables accordingly
data = data %>% 
rename(Plant_species = Plant_species_corrected) %>% 
rename(Habitat = Focal_habitat) %>% 
mutate(Habitat =  as.factor(Habitat))  %>% 
mutate(Habitat = recode_factor(Habitat, "HA" = "Herbaceous areal 1",
                              "HA2" = "Herbaceous areal 2",
                              "HA3" = "Herbaceous areal 3",
                              "HL" = "Herbaceous linear",
                              "HL2" = "Herbaceous linear 2",
                              "WA" = "Woody areal",
                              "WL" = "Woody linear",
                              "WL2" = "Woody linear 2"))

#Add missing variables
data = add_missing_variables(check_interaction_data, data)

#Plant species are just codes at the moment
#Load flower count data which has the correct species names 
plant_names <- read_csv("Data/Raw_data/28_Albrecht/Flower_count.csv") %>% 
select(Plant_species, Species_abbreviation) %>% 
rename(Plant_names_new = Plant_species, Plant_species = Species_abbreviation)

#This dataset still has some NA's and we are missing species 
#Check... :(

#Select unique species
unique_plant = plant_names %>% 
select(Plant_species) %>%  
distinct() %>% 
pull(Plant_species)

#Select unique plants to check missing ones
data %>% 
select(Plant_species) %>%
distinct() %>%
filter(!Plant_species %in% unique_plant) %>%
pull()


