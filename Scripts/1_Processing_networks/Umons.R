#DATASET NUMBER 39; Umons

#Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)
#Load function to unify structure of data
source("Scripts/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/39_UMONS/Network_data.csv")

#Filter interactions equal to 0
data = data %>% filter(Interaction > 0)

#Filter out any row that contains alphatic letter in latitude
#This can be recovered but exclude it for now
data = data %>%
  filter(!grepl("^[A-Za-z]", Latitude)) %>% 
    filter(!grepl("^[A-Za-z]", Longitude))

#Unify structure of data
data = change_str(data)

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
#I comment it for now as thee data needs more work
#saveRDS(UMONS, file="Data/Clean_data/39_Umons.rds")

