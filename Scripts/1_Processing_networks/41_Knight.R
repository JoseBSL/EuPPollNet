#DATASET NUMBER 41; Knight-Estonia
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
data <- read_csv("Data/1_Raw_data/41_Knight/Interaction_data.csv")

#Compare vars
#compare_variables(check_interaction_data, data)
#Nothing missing!

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Add flower info cols
data = data %>% 
mutate(Flower_data = "Yes") 

#Set merger col
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_" , word(Plant_species,2), "_", Site_id))
#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)

#Prepare flower count data ---- 
FlowerCount = read_csv("Data/1_Raw_data/41_Knight/Flower_count.csv") %>% 
mutate(Comments = NA,
       Flower_data_merger = NA)

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#No misisng vars
#Set merger col
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_" , word(Plant_species,2), "_", Site_id))

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
Metadata <- tibble(
  Doi = "https://doi.org/10.1002/ece3.8325",
  Dataset_description = "This dataset documents 6 different
  sites with two different types of extensive management
  that are known to promote plant diversity: mown wooded
  meadows and grazed alvar pastures. Wooded meadows are
  mown once a year and alvars typically have 0.2– 0.5
  livestock units (LSU)/hectar. At each of site, we
  sampled plants and interactions in 10 replicate 30m
  by 2m transects (600m2 for the site). Each transect
  was sampled for 15 minutes for a total of 150 minutes
  of sampling per site. Each site was sampled one time.",
  Taxa_recorded = "Hymenoptera, Diptera and Lepidoptera",
  Sampling_year = "2018",
  Country = "Estonia",
  Habitat = "Mown wooded meadows and grazed alvar pastures",
  Sampling_sites = nlevels(factor(data$Site_id)),
  Sampling_rounds = "5 consecutive days",
  Sampling_method = "Transect",
  Sampling_area_details = "1 transect 30m by 2m",
  Sampling_area_species_m2 = NA,
  Sampling_area_total_m2 = "600m2",
  Sampling_time_details = "15min/transect,150 min site", 
  Sampling_time_species_round_min = NA, 
  Sampling_time_total_min = "1200min", #time *sites * rounds
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
                    "Valentin Ştefan", "Reinart Feldmann", 
                    "Lena Neuenkamp"),
  
  Orcid = c("0000-0003-0318-1567", "0000-0002-2407-9564",
            "0000-0002-4757-8008", "", "0000-0001-6108-5720"),
  E_mail = c("tiffany.knight@idiv.de", "elena.motivans@ufz.de",
             "valentin.stefan@idiv.de", "reinart.feldmann@ufz.de",
             "lena-neuenkamp@web.de"))

#Save data ----
#Create metadata list
Knight <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Knight) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Knight, file="Data/2_Processed_data/41_Knight.rds")







