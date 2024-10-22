#DATASET NUMBER 28; Sutter 
#There are some unmatching plant names


#Read templates to compare with
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/1_Raw_data/28_Sutter/Interaction_data.csv")

#Filter interactions greater than 0
#And filter out insects caught flying
data = data %>% 
filter(Interaction > 0) %>% 
filter(!Plant_species_corrected == "transect")

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
                              "WL2" = "Woody linear 2")) %>% 
mutate(Country = "Switzerland")

#Add missing variables
data = add_missing_variables(check_interaction_data, data)

#Plant species are just codes at the moment
#Load flower count data which has the correct species names 
plant_names <- read_csv("Data/1_Raw_data/28_Sutter/Flower_count.csv") %>% 
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

#Check missing species
data %>% 
select(Plant_species) %>%
distinct() %>%
filter(!Plant_species %in% unique_plant) %>%
pull()

#Select plant species with full species names on the flower dataset
data = data %>% 
filter(Plant_species %in% unique_plant)

#Create col to join to original dataframe
unique_plant = plant_names %>% 
distinct() 
#Join data
data = left_join(data, unique_plant) %>% 
select(!Plant_species) %>% 
rename(Plant_species = Plant_names_new) %>% 
mutate(Sampling_method = "Transect")
#Reorder variables
data = drop_variables(check_interaction_data, data) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Fix year typo
levels(factor(data$Year))

#Fix mistakes in dates
data = data %>% 
mutate(Year = case_when(
Year == 213 ~ 2013, 
T ~ Year))

#Fix mistakes in dates
data = data %>% 
mutate(Year = case_when(
is.na(Year) ~ 2013, 
T ~ Year))

#Add other needed cols
data = data %>% 
mutate(Flower_data_merger = NA) %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Comments = NA)

#Unify structure of data
data = change_str(data)

#Delete dot from plant species
data = data %>% 
mutate(Plant_species = str_replace(Plant_species, "[.]", " "))%>% 
mutate(Pollinator_species = str_replace(Pollinator_species, "[.]", " "))

#Create column to merge with floral data
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1),word(Plant_species,2), 
                                   Site_id, Habitat, Month, Year)) 

#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount = read_csv("Data/1_Raw_data/28_Sutter/Flower_count.csv")
#Recode some factors
FlowerCount = FlowerCount %>% 
mutate(Species_abbreviation = case_when(
Plant_species == "Taraxacum.campylodes" ~ "tar.off", 
T ~ Species_abbreviation))
#Recode Silene
FlowerCount = FlowerCount %>% 
mutate(Species_abbreviation = case_when(
Plant_species == "Silene.flos.cuculi" ~ "sil.flo", 
T ~ Species_abbreviation)) %>% 
mutate(Plant_species = case_when(
Plant_species == "Silene.flos.cuculi" ~ "Silene.flos-cuculi", 
T ~ Plant_species))
FlowerCount = FlowerCount %>% 
mutate(Species_abbreviation = case_when(
Plant_species == "Silene.latifolia" ~ "sil.lat", 
T ~ Species_abbreviation)) 
#Recode rosa spp
FlowerCount = FlowerCount %>% 
mutate(Species_abbreviation = case_when(
Plant_species == "Rosa.spp" ~ "ros.can", 
T ~ Species_abbreviation)) %>% 
mutate(Plant_species = case_when(
Plant_species == "Rosa.spp" ~ "Rosa.canina", 
T ~ Plant_species))
#Recode spp
FlowerCount = FlowerCount %>% 
mutate(Species_abbreviation = case_when(
Plant_species == "Reseda.lutea" ~ "res.lut", 
T ~ Species_abbreviation))
#Capsella.bursa.pastoris
FlowerCount = FlowerCount %>% 
mutate(Species_abbreviation = case_when(
Plant_species == "Capsella.bursa.pastoris" ~ "cap.bur", 
T ~ Species_abbreviation))

#Rename variables accordingly
FlowerCount = FlowerCount %>% 
rename(Habitat = Focal_habitat) %>% 
mutate(Habitat =  as.factor(Habitat))  %>% 
mutate(Habitat = recode_factor(Habitat, "HA" = "Herbaceous areal 1",
      "HA2" = "Herbaceous areal 2",
      "HA3" = "Herbaceous areal 3",
      "HL" = "Herbaceous linear",
      "HL2" = "Herbaceous linear 2",
      "WA" = "Woody areal",
      "WL" = "Woody linear",
      "WL2" = "Woody linear 2")) %>% 
rename(Comments = Inflorescence_unit_used) %>% 
mutate(Plant_species = str_replace(Plant_species, "[.]", " "))

#Create column to merge with floral data
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1),word(Plant_species,2), 
                                   Site_id, Habitat, Month, Year)) 


#Compare vars
#compare_variables(check_flower_count_data, flower_count)

#Set common structure
FlowerCount = FlowerCount %>% 
mutate(Day = as.character(Day)) %>% 
mutate(Month = as.character(Month)) %>% 
mutate(Year = as.numeric(Year)) %>% 
mutate(Site_id = as.character(Site_id)) %>% 
mutate(Plant_species = as.character(Plant_species)) %>% 
mutate(Flower_count = as.numeric(Flower_count)) %>% 
mutate(Units = as.character(Units)) %>% 
mutate(Comments = as.character(Comments)) %>% 
mutate(Units = "Flowers/m2")

#Unify structure of data
FlowerCount = change_str2(FlowerCount)
#Order data (just in case)
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#There are duplicated rorws
#Fix that, so this does not give trouble when left_join with interactions
FlowerCount = FlowerCount %>%
group_by_at(vars(-c(Flower_count,Day))) %>%
summarise(Flower_count = mean(Flower_count)) 

#Split interaction data into dataframes within a list
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
  Doi = "https://doi.org/10.1111/1365-2664.12907",
  Dataset_description = "This dataset contains bee-flower interactions
  in 17 agricultural landscapes (= site_id) of 1km radius along a
  landscape composition and configuration gradient in the northern
  Swiss Lowlands. In this survey, wild bees were sampled in one habitat
  patch per habitat type within each landscape",
  Taxa_recorded = "Wild bees and syrphids",
  Sampling_year = "2013-2014",
  Country = "Switzerland",
  Habitat = "Gradient of non-crop vegetation and seminatural habitats",
  Sampling_sites = nlevels(factor(data$Site_id)),
  Sampling_rounds = "4",
  Sampling_method = "Transects",
  Sampling_area_details = "50m length * 1.5m wide",
  Sampling_area_species_m2 = NA,
  Sampling_area_total_m2 = 75 * 17 * 4, #transect_area * sites *rounds
  Sampling_time_details = "10 mins per transect", 
  Sampling_time_species_round_min = NA, 
  Sampling_time_total_min = 64*60, #64h *60mins, info from the paper
  Total_plant_species = nrow(plant_single_cases),
  Total_pollinator_species = nrow(pollinator_single_cases),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Louis Sutter", "Matthias Albrecht"),
  Orcid = c("0000-0002-2626-216X", "0000-0001-5518-3455"),
  E_mail = c("louis.sutter@agroscope.admin.ch", "matthias.albrecht@agroscope.admin.ch"))

#Save data ----
#Create metadata list
Sutter <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Sutter) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Sutter, file="Data/2_Processed_data/28_Sutter.rds")





