#DATASETS NUMBER (23-24)-25; Holzschuh
#To keep things clear here I have separated this in 3 different scripts (dataset 25 here)
#I have separated this dataset by habitat. This one compiles seminatural habitat

#Read source to compare with template later on
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)
library(lubridate)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/1_Raw_data/23_24_25_Holzschuh/Interaction_data.csv")

#Compare vars
#compare_variables(check_interaction_data, data)

#Add interaction col
data = data %>% 
mutate(Interaction = 1) %>% 
mutate(Sampling_method = "Transect") %>% 
mutate(Sampling_effort_minutes = NA) %>% 
mutate(Sampling_area_square_meters = NA) %>% 
mutate(Habitat = recode_factor(Habitat, "FB" = "Field boundaries", 
      "MFC" = "Mass flowering crop", "SNH" = "Seminatural")) %>% 
mutate(Comment = "Location col indicates transects rounds (1 and 2)")%>% 
mutate(Flower_data = "No")%>% 
mutate(Flower_data_merger = Site_id)

#Add missing vars
data = add_missing_variables(check_interaction_data, data) 
#Reorder variables
data = drop_variables(check_interaction_data, data) 
#Drop this last two columns that are going to be at the metadata
data = data %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Unify structure of data
data = change_str(data)

#Now I split data into different dataframes based on habitat
split_intdata <- split(data, data$Habitat)
#Convert to tibbles
data3 <- as_tibble(split_intdata[[3]])

#Split interaction data into dataframes within a list (separate now by site)
InteractionData3 <- split(data3, data3$Site_id)

#Prepare flower count data ----
#FlowerCount <- read_csv("Data/1_Raw_data/23_24_25_Holzschuh/Flower_count.csv")
#
##Compare vars
##compare_variables(check_flower_count_data, flower_count)
#
##Split date col
#FlowerCount = FlowerCount %>% 
#mutate(Date = as.Date(Date, format="%m-%d-%y")) %>% 
#mutate(Year = year(ymd(Date))) %>% 
#mutate(Month = month(ymd(Date))) %>% 
#mutate(Day = day(ymd(Date))) 
#
##Rename cols
#FlowerCount = FlowerCount %>% 
#rename(Flower_count = Flower_cover_percent) %>% 
#mutate(Units = "Flower cover/150m2") %>% 
#mutate(Habitat = recode_factor(Habitat, "FB" = "Field boundaries", 
#"MFC" = "Mass flowering crop", "SNH" = "Seminatural")) %>% 
#mutate(Comment = Habitat) #Add habitat in comment col to split datasets
#
##Set common structure
#FlowerCount = FlowerCount %>% 
#mutate(Day = as.character(Day)) %>% 
#mutate(Month = as.character(Month)) %>% 
#mutate(Year = as.numeric(Year)) %>% 
#mutate(Site_id = as.character(Site_id)) %>% 
#mutate(Plant_species = as.character(Plant_species)) %>% 
#mutate(Flower_count = as.numeric(Flower_count)) %>% 
#mutate(Units = as.character(Units)) %>% 
#mutate(Comment = as.character(Comment))
#
##Add variables
#FlowerCount = add_missing_variables(check_flower_count_data, FlowerCount) 
##Order data as template
#FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 
#
##Split flower count data into dataframes by habitat
#split_flower_count_data <- split(FlowerCount, FlowerCount$Comment) 
##Convert to tibbles
#flower_count3 <- as_tibble(split_flower_count_data[[3]])
##Split interaction data into dataframes within a list
#FlowerCount3 <- split(flower_count3, flower_count3$Site_id)

#Flower counts are not reliable at the moment
#Check levels of Site_id
site_id_levels = levels(factor(bind_rows(InteractionData)$Site_id))

FlowerCount3 = tibble(Day = NA_character_, Month = NA_character_, Year = NA, Site_id = site_id_levels, Plant_species = NA_character_,
                      Flower_count = NA, Units = NA_character_, Comments = NA_character_,
                     Flower_data_merger = site_id_levels)

#Set common structure
FlowerCount3 = change_str2(FlowerCount3)

#Split by Site_id
FlowerCount3 <- split(FlowerCount3, FlowerCount3$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases3 = data3 %>% distinct(Plant_species)
pollinator_single_cases3 = data3 %>%distinct(Pollinator_species)

#Create ordered metadata
Metadata3 <- tibble(
Doi = "https://doi.org/10.1111/ele.12657",
Dataset_description = "Two year dataset of bees and hoverflies on three habitats of Germany",
Taxa_recorded = "Bees and hoverflies",
Sampling_year = "2011 and 2012",
Country = "Germany",
Habitat = "Field boundary",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = 2,
Sampling_method = "Transect",
Sampling_area_details = "Two 150 x 1 m transects per round and site",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = 2 * 150 * 1 * 16, #2 transects x 150 m2 * 16 sites
Sampling_time_details = "15 mins per site and per round",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = 2 * 15 * 16 * 2, #2 transects * 15 mins * 16 sites * 2 rounds
Total_plant_species = nrow(plant_single_cases1),
Total_pollinator_species = nrow(pollinator_single_cases1),
Floral_counts =  "Yes")

#Transpose metadata
Metadata3 = as.data.frame(t(Metadata3)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship3 <- tibble(
Coauthor_name = c("Andrea Holzschuh", "Ingolf Steffan-Dewenter"),
Orcid = c("0000-0002-5235-4746", "0000-0003-1359-3944"),
E_mail = c("andrea.holzschuh@uni-wuerzburg.de", "ingolf.steffan@uni-wuerzburg.de"))

#Save data ----
#Create metadata list
Holzschuh3 <- list(InteractionData3, FlowerCount3, Metadata3, Authorship3)
names(Holzschuh3) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Holzschuh3, file="Data/2_Processed_data/25_Holzschuh.rds")


