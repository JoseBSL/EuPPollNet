#DATASET NUMBER 40; Knight-Romania
#Dataset sent by Tiffany

#Read empty templates to compare with
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
data <- read_csv("Data/1_Raw_data/40_Knight/Interaction_data.csv")

#Compare vars
#compare_variables(check_interaction_data, data)
#Nothing missing!

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Unify level
data = data %>% 
mutate(Sampling_method = "Random_census")

#Add flower info cols
data = data %>% 
mutate(Flower_data = "Yes") 

#Set merger col
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_" , word(Plant_species,2)))
  
#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)
#In this case there is a single lcoation 

#Prepare flower count data ---- 
FlowerCount = read_csv("Data/1_Raw_data/40_Knight/Flower_count.csv") %>% 
mutate(Comments = NA,
       Flower_data_merger = NA)

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#No misisng vars

#Set merger col
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_" , word(Plant_species,2)))

#Set common structure
FlowerCount = change_str2(FlowerCount)
#Split interaction data into dataframes within a list
FlowerCount = split(FlowerCount, FlowerCount$Site_id)
#Just one location again

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata = tibble(
  Doi = "https://doi.org/10.1093/aobpla/ply068",
  Dataset_description = "This dataset was carried out in a mountain meadow in
  Ghețari village, Romania.  Our focal meadow is traditionally
  managed. It is mowed once per year and manure is applied during late
  autumn or winter. During autumn, the meadow is occasionally grazed
  (no more than one cow/ha). The meadow was 3.8 ha and bordered on
  two sides by forest and on two sides by a hedgerow that separated our
  focal meadow from another meadow. We sampled the meadow from
  10 to 14 July 2017 using a sampling effort of 60 person hours.
  We focused on 33 of the 86 flowering plants that were in bloom at the time.
  These were relatively more abundant species. We wandered
  the meadow and collected insects that landed on flowers and
  came in contact with floral reproductive structures
  (i.e., were potential pollinators).",
  Taxa_recorded = "Hymenoptera, Diptera and Lepidoptera",
  Sampling_year = "2017",
  Country = "Romania",
  Habitat = "Mountain meadow",
  Sampling_sites = nlevels(factor(data$Site_id)),
  Sampling_rounds = "5 consecutive days",
  Sampling_method = "Random census",
  Sampling_area_details = "3.8 ha",
  Sampling_area_species_m2 = NA,
  Sampling_area_total_m2 = "380000",
  Sampling_time_details = "720min/day", 
  Sampling_time_species_round_min = NA, 
  Sampling_time_total_min = "3600min", #time *sites * rounds
  Total_plant_species = nrow(plant_single_cases),
  Total_pollinator_species = nrow(pollinator_single_cases),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Tiffany M. Knight", "Joanne M. Bennett",
                    "Irina Goia", "Amibeth Thompson",
                    "Reinart Feldmann", "Demetra Rakosy",
                    "Valentin Ştefan"),
  
  Orcid = c("0000-0003-0318-1567", "0000-0002-7883-3577",
            "0000-0001-8270-2214", "0000-0002-9224-412X",
            "0009-0009-6004-0702", "0000-0001-8010-4990", "0000-0002-4757-8008"),
  E_mail = c("tiffany.knight@idiv.de", "joanne.bennett@anu.edu.au", 
             "irina.goia@ubbcluj.ro; igoia@yahoo.com", "amibeth.thompson@gmail.com",
             "reinart.feldmann@ufz.de", "demetra.rakosy@gmail.com",
             "valentin.stefan@idiv.de"))

#Save data ----
#Create metadata list
Knight <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Knight) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Knight, file="Data/2_Processed_data/40_Knight.rds")




