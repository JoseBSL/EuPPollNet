#DATASET NUMBER 50; Hervias-Parejo
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
data <- read_csv("Data/Raw_data/50_Hervias-Parejo/Interaction_data.csv", locale = locale(encoding = "latin1"))

#Compare vars
#compare_variables(check_interaction_data, data)
#Rename plant spp col
data = data %>% 
rename("Plant_species" = "Plant species")

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Unify level
data = data %>% 
mutate(Sampling_method = "Focal_observation")

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ---- 
flower_count <- read_csv("Data/Raw_data/50_Hervias-Parejo/Flower_count.csv")
#Rename plant spp col
flower_count = flower_count %>% 
rename("Plant_species" = "Plant species")

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
Doi = "https://doi.org/10.1111/oik.09818",
Dataset_description = "This dataset documents 3 different
sites on Menorca island (Balearic archipelago, western
Mediterranean Sea): (NE) one in the north-east
(Es Grau-Favàritx 39°58′22.90″N, 4°14′59.20″E); (N)
one in the north (Torre Fornells-Cala Tirant 40°03′04.40″N,
4°07′01.01″E); and (S) one in the south (Son Bou-Llucalari
39°53′37.05″N, 4°04′56.80″E) of the island. Within each
site, we sampled the three most common habitats (spaced
about 3 km apart) of Menorca: 1) a dune system,
nitrohalophytic vegetation of sandy beaches with mostly
herbaceous species such as Cakile maritima, Glaucium
flavum, Lobularia maritima and Lotus cytisoides, 2) a
coastal scrubland that occupies the foreshore, with
vegetation adapted to extreme conditions of wind and
salinity, generally endemic species of very restricted
distribution such as Limonium minutum, Santolina chamaecyparissus,
Anthyllis histrix, Thymelaea velutina and T. hirsuta and 3)
an agro-forestry system, consisting of agricultural crops
of herbaceous plants combined with patches of shrubs and
trees and other herbaceous species that take advantage of
the spaces left by cultivated plants, such as Galactites
tomentosa, Rubus ulmifolius, Glebionis coronaria and 
Daucus carota. Across the flowering season, 5 visits to each
site were performed, and in each visit, we measured both plant-pollinator
interactions and flower abundance of each censused individual plant.
We cover the full phenology of most plants and pollinators
and measure all floral visitors. One year of data is available.",
Taxa_recorded = "All floral visitors",
Sampling_year = "2021",
Country = "Spain",
Habitat = "Dune system and coastal scrubland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "",
Sampling_method = "Focal_observation",
Sampling_area_details = "6 transects 100 m long 10 m wide",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = "" ,
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
  Coauthor_name = c("Sandra Hervias-Parejo", 
                    "Anna Traveset",
                    "Pau Colom",
                    "Rafel Beltran Mas",
                    "Pau Enric Serra"),
  Orcid = c("0000-0002-5377-3619", 
            "0000-0002-1816-1334",
            "0000-0003-0309-8886",
            "0000-0002-6285-4357",
            "0000-0001-6674-3710"),
  E_mail = c("shervias@imedea.uib-csic.es", 
             "atraveset@imedea.uib-csic.es",
             "pau.colom.montojo@gmail.com",
             "rafel.beltran.mas@hotmail.com",
             "pau.enric.serra@gmail.com"))

#Save data ----
#Create metadata list
Hervias_Parejo <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Hervias_Parejo) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Hervias_Parejo, file="Data/Clean_data/50_Hervias-Parejo.rds")




