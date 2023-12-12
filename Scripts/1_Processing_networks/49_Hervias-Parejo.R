#DATASET NUMBER 49; Hervias-Parejo
#Dataset sent by Sandra H-P
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
data <- read_csv("Data/Raw_data/49_Hervias-Parejo/Interaction_data.csv", locale = locale(encoding = "latin1"))

#Compare vars
#compare_variables(check_interaction_data, data)
#Rename plant spp col
data = data %>% 
rename("Plant_species" = "Plant species")

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ---- 
flower_count <- read_csv("Data/Raw_data/49_Hervias-Parejo/Flower_count.csv")
#Rename plant spp col
flower_count = flower_count %>% 
rename("Plant_species" = "Plant species") %>% 
select(!...9) #extra col from excel

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
Doi = "https://doi.org/10.1101/2023.07.02.547400",
Dataset_description = "This dataset documents 1 islet 
(Na Redona, 39° 10’ 5 “ N, 2° 58’ 35” 443 E) of
approximately 11 ha and 56 m high in the Cabrera
Archipelago National Park (Balearic Islands, Western
Mediterranean Sea). Its primary habitat is Mediterranean
shrubland with a relatively rich plant species diversity
(ca. 108). A team of five people visited the islet for
five consecutive days at the peak of flowering (April/May).
We measured both plant-pollinator interactions and flower
abundance of each censused individual plant. We cover the
full phenology of most plants and pollinators and measure
all floral visitors. Two years of data are available.",
Taxa_recorded = "All floral visitors",
Sampling_year = "2018 and 2019",
Country = "Spain",
Habitat = "Scrubland",
Sampling_sites = "1",
Sampling_rounds = "",
Sampling_method = "Transects",
Sampling_area_details = "6 transects 100 m long 10 m wide",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = "6000" ,
Sampling_time_details = NA,
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
  Coauthor_name = c("Sandra Hervias-Parejo", "Anna Traveset"),
  Orcid = c("0000-0002-5377-3619", "0000-0002-1816-1334"),
  E_mail = c("shervias@imedea.uib-csic.es", "atraveset@imedea.uib-csic.es"))

#Save data ----
#Create metadata list
Hervias_Parejo <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Hervias_Parejo) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Hervias_Parejo, file="Data/Clean_data/49_Hervias-Parejo.rds")




