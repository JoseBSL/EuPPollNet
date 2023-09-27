#DATASET NUMBER 38; Maurer

#Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/39_UMONS/Network_data.csv")

#There are some sites without coordinates: I haven't tried to work that out

#Filter interactions equal to 0
data = data %>% filter(Interaction > 0)

#Compare vars

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
  Doi = " ",
  Dataset_description = "",
  Taxa_recorded = "Pairwise plant pollinator interaccions from museum collections",
  Sampling_year = "1850-2021",
  Country = "Europe",
  Habitat = "No habitat specified",
  Sampling_sites = "Not applicable",
  Sampling_rounds = "Not applicable",
  Sampling_method = "Sinlge collections",
  Total_plant_species = nrow(plant_single_cases),
  Total_pollinator_species = nrow(pollinator_single_cases),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = "UMONS",
  Orcid = " ",
  E_mail = " ")

#Save data ----
#Create metadata list
UMONS <- list(InteractionData,  Metadata, Authorship)
#Rename list elements
names(UMONS) <- c("InteractionData","Metadata", "Authorship")
#Save data
saveRDS(UMONS, file="Data/Clean_data/39_Umons.rds")

