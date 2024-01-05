#DATASET NUMBER 43; Knight-Finland
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
data <- read_csv("Data/1_Raw_data/43_Knight/Interaction_data.csv", locale = locale(encoding = "latin1"))
#Note encoding= "latin1"
#to read special finish charaters

#Compare vars
#compare_variables(check_interaction_data, data)
#Nothing missing!

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Unify level
data = data %>% 
mutate(Sampling_method = "Transect")

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
#We need to create a list of lists too
#Check levels of Site_id
site_id_levels = levels(factor(data$Site_id))

FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = site_id_levels, Plant_species = NA,
                     Flower_count = NA, Units = NA, Comment = NA)

FlowerCount = FlowerCount %>% 
mutate(Day = as.character(Day)) %>% 
mutate(Month = as.character(Month)) %>% 
mutate(Year = as.numeric(Year)) %>% 
mutate(Site_id = as.character(Site_id)) %>% 
mutate(Plant_species = as.character(Plant_species)) %>% 
mutate(Flower_count = as.numeric(Flower_count)) %>% 
mutate(Units = as.character(Units)) %>% 
mutate(Comment = as.character(Comment))

#Split by Site_id
FlowerCount <- split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
Doi = "https://doi.org/10.1038/s41559-022-01928-3",
Dataset_description = "We sample plants,
pollinators and their interactions,
in 30 by 2 meter transects across 12 sites,
four in each of three locations at different
latitudes spanning 750 km in Finland in June and July
of 2019. In each of the three locations (Kittilä,
Pudasjärvi and Lammi), we selected four sites that
represented common habitats, were non-densely forested and
contained flowering herbaceous plant species. In Northern
Finland, agriculture is sparse, and open habitats with
herbaceous plants are usually found in transitional woodland/shrub.
In Southern Finland, there is proportionally more arable
land.",
Taxa_recorded = "Diptera, Hymenoptera, Lepidoptera",
Sampling_year = "2019",
Country = "Finland",
Habitat = "Grassland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "",
Sampling_method = "Transects",
Sampling_area_details = "30 by 2 meter transects across 12 sites",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA ,
Sampling_time_details = "",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = NA, 
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "No")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Tiffany M. Knight", "Leana Zoller", "Øystein Opedal",
                    "Stefan Klotz", "Annette Trefflich"),
  Orcid = c("0000-0003-0318-1567", "0000-0002-7161-6529", "0000-0002-7841-6933",
            "0000-0003-4355-6415", ""),
  E_mail = c("tiffany.knight@idiv.de", "Leana.Zoller@colorado.edu", 
             "oystein.opedal@biol.lu.se", "stefan.klotz@ufz.de",
             "antreff@web.de"))

#Save data ----
#Create metadata list
Knight <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Knight) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Knight, file="Data/2_Processed_data/43_Knight.rds")


