#DATASET NUMBER 42; Knight-Czech Republic
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
data <- read_csv("Data/1_Raw_data/42_Knight/Interaction_data.csv")

#Compare vars
#compare_variables(check_interaction_data, data)
#Nothing missing!

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ---- 
FlowerCount <- read_csv("Data/1_Raw_data/42_Knight/Flower_count.csv")

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#No misisng vars
#Set common structure
FlowerCount = FlowerCount %>% 
mutate(Day = as.character(Day)) %>% 
mutate(Month = as.character(Month)) %>% 
mutate(Year = as.numeric(Year)) %>% 
mutate(Site_id = as.character(Site_id)) %>% 
mutate(Plant_species = as.character(Plant_species)) %>% 
mutate(Flower_count = as.numeric(Flower_count)) %>% 
mutate(Units = as.character(Units)) %>% 
mutate(Comment = as.character(Comment))

#Split interaction data into dataframes within a list
FlowerCount <- split(FlowerCount, FlowerCount$Site_id)
#Just one location again

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
  Doi = NA,
  Dataset_description = "We sampled 8 sites,
  4 were old, semi-natural, dry to mesophilic meadows.
  10 transects were placed in each site. Each transect
  was 30m by 2m.  Interactions were sampled for 15
  minutes per transect (not counting processing time)
  for a total of 150 minutes per site.",
  Taxa_recorded = "Hymenoptera, Diptera and Lepidoptera",
  Sampling_year = "2019",
  Country = "Czech Republic",
  Habitat = "Semi-natural, dry to mesophilic meadows",
  Sampling_sites = nlevels(factor(data$Site_id)),
  Sampling_rounds = "1",
  Sampling_method = "Transect",
  Sampling_area_details = "1 transect 30m by 2m",
  Sampling_area_species_m2 = NA,
  Sampling_area_total_m2 = "600m2",
  Sampling_time_details = "15min/transect,150 min site", 
  Sampling_time_species_round_min = NA, 
  Sampling_time_total_min = "1200 min", #time *sites * rounds
  Total_plant_species = nrow(plant_single_cases),
  Total_pollinator_species = nrow(pollinator_single_cases),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Tiffany M. Knight", "Reinart Feldmann",
                    "Demetra Rakosy", "Robert Tropek", 
                    "Jana Jersáková"),
  
  Orcid = c("0000-0003-0318-1567", "", "0000-0001-8010-4990", 
            "0000-0001-7499-6259", "0000-0003-0169-801X"),
  E_mail = c("tiffany.knight@idiv.de", "reinart.feldmann@ufz.de",
             "demetra.rakosy@gmail.com", "robert.tropek@gmail.com",
             "jersa@centrum.cz"))

#Save data ----
#Create metadata list
Knight <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Knight) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Knight, file="Data/2_Processed_data/42_Knight.rds")







