#DATASET NUMBER 44; Knight-Romania
#Dataset sent by Tiffany

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
data <- read_csv("Data/Raw_data/44_Knight/Interaction_data.csv", locale = locale(encoding = "latin1"))

#Compare vars
#compare_variables(check_interaction_data, data)
#Nothing missing!

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ---- 
flower_count <- read_csv("Data/Raw_data/44_Knight/Flower_count.csv")

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#No misisng vars

#Split interaction data into dataframes within a list
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
Doi = NA,
Dataset_description = "We sample plants, pollinators and
their interactions in 30 by 2 meter transects across 18
sites in translyvania. We choose sites that were all
semi-natural grasslands that varied in soil type, moisture
(wet, mesophilic and dry meadows were considered) and
elevation",
Taxa_recorded = "Diptera, Hymenoptera, Lepidoptera",
Sampling_year = "2018",
Country = "Romania",
Habitat = "Grassland",
Sampling_sites = "18",
Sampling_rounds = "",
Sampling_method = "Transect",
Sampling_area_details = "30 by 2 meter transects",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA ,
Sampling_time_details = "",
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
  Coauthor_name = c("Tiffany M. Knight", "Elena Motivans Švara",
                    "Demetra Rakosy"),
  Orcid = c("0000-0003-0318-1567", "0000-0002-2407-9564", "0000-0001-8010-4990"),
  E_mail = c("tiffany.knight@idiv.de", "elena.motivans@ufz.de", "demetra.rakosy@gmail.com"))

#Save data ----
#Create metadata list
Knight <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Knight) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Knight, file="Data/Clean_data/44_Knight.rds")




