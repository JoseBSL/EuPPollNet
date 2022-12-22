#DATASET NUMBER 28; Albrecht 
#Some species can be recovered from this dataset
#There are some unmatching plant names
#I have done the best I can but there are somoe missing ones
#As far as I can read most are non-relevant poaceae for pollination
#For instance, dactylis glomerolata, poa, lolium, etc...

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

#Recode some factors
plant_names = plant_names %>% 
mutate(Plant_species = case_when(
Plant_names_new == "Taraxacum.campylodes" ~ "tar.off", 
T ~ Plant_species))
#Recode Silene
plant_names = plant_names %>% 
mutate(Plant_species = case_when(
Plant_names_new == "Silene.flos.cuculi" ~ "sil.flo", 
T ~ Plant_species)) %>% 
mutate(Plant_names_new = case_when(
Plant_names_new == "Silene.flos.cuculi" ~ "Silene.flos-cuculi", 
T ~ Plant_names_new))
#Ranunculus acris
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"ran.car" = "ran.acr"))
#Vicia sepium
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"vic.tet" = "vic.sep"))
#Echium vulgare
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"ech.cru" = "ech.vul"))
#It seems that this was rubus fruticosus 
#but is not on the plant names
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"rub.fru" = "rub.ide"))
#This seems a typo
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"rub.ida" = "rub.ide"))
#This seems that is also ranunculus acris
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"ran.fri" = "ran.acr"))
#Silene latifolia subs alba
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"sil.alb" = "sil.lat"))
plant_names = plant_names %>% 
mutate(Plant_species = case_when(
Plant_names_new == "Silene.latifolia" ~ "sil.lat", 
T ~ Plant_species)) 
#Recode rosa spp
plant_names = plant_names %>% 
mutate(Plant_species = case_when(
Plant_names_new == "Rosa.spp" ~ "ros.can", 
T ~ Plant_species)) %>% 
mutate(Plant_names_new = case_when(
Plant_names_new == "Rosa.spp" ~ "Rosa.canina", 
T ~ Plant_names_new))
#Recode spp
plant_names = plant_names %>% 
mutate(Plant_species = case_when(
Plant_names_new == "Reseda.lutea" ~ "res.lut", 
T ~ Plant_species))
#Onobrychis.viciifolia
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"ono.vici" = "ono.vic"))
#Cornus.sanguinea
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"cor.sag" = "cor.san"))
#Synonyms T.flavum and T.dubium
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"tri.fla" = "tri.dub"))
#Capsella.bursa.pastoris
plant_names = plant_names %>% 
mutate(Plant_species = case_when(
Plant_names_new == "Capsella.bursa.pastoris" ~ "cap.bur", 
T ~ Plant_species))
#Leontodon hir to Leontodon hispidus
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"leo.hir" = "leo.his"))
#eup.amy
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"eup.amy" = "eup.cyp"))
#synonym
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"cam.tra" = "cam.rap"))
#synonym
data = data %>% 
mutate(Plant_species = recode_factor(Plant_species,
"san.maj" = "san.off"))

#Select unique species
unique_plant = plant_names %>% 
select(Plant_species) %>%  
distinct() %>% 
pull(Plant_species)

#Select unique plant species
data = data %>% 
filter(Plant_species %in% unique_plant)
#Transect field, eliminate
data = data %>% 
filter(!Plant_species == "transect")

#Select unique species
unique_plant = plant_names %>% 
select(Plant_species) %>%  
distinct() %>% 
pull(Plant_species)
#Not included species
#hol.lan/lol.per/phl.pra/poa.tri/sil.pra/dac.glo
#ace.pse/arr.ela/evo.eur/car.imp/lol.mul/agr.cap/
#fag.syl/ely.rep/ant.odo/cen.cya/fes.pra/sen.jac/
#pol.per

#Select unique plants to check missing ones
data %>% 
select(Plant_species) %>%
distinct() %>%
filter(!Plant_species %in% unique_plant) %>%
pull()

#Create col to join to original dataframe
unique_plant = plant_names %>% 
distinct() 
#Join data
data = left_join(data, unique_plant) %>% 
select(!Plant_species) %>% 
rename(Plant_species = Plant_names_new) %>% 
mutate(Sampling_method = "Transect")

#Compare vars
#compare_variables(check_interaction_data, data)
#Add missing vars
data = add_missing_variables(check_interaction_data, data) 
#Reorder variables
data = drop_variables(check_interaction_data, data) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

