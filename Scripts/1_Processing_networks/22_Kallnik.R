#DATASET NUMBER 22; Kallnik
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)
library(lubridate)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/1_Raw_data/22_Kallnik/Interaction_data.csv")

#Compare vars
#compare_variables(check_interaction_data, data)

#Data
data = data %>% 
mutate(Date = as.Date(Date, format="%m-%d-%y")) %>% 
mutate(Year = year(ymd(Date))) %>% 
mutate(Month = month(ymd(Date))) %>% 
mutate(Day = day(ymd(Date))) 

#Add interaction col
data = data %>% 
mutate(Interaction = 1) %>% 
mutate(Sampling_method = "Transect") %>% 
mutate(Sampling_effort_minutes = NA) %>% 
mutate(Sampling_area_square_meters = NA) %>% 
mutate(Habitat = "Pastures")

#Add missing vars
data = add_missing_variables(check_interaction_data, data) 
#Reorder variables
data = drop_variables(check_interaction_data, data) 
#Drop this last two columns that are going to be at the metadata
data = data %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data,data$Site_id)

#Prepare flower count data ----
FlowerCount <- read_csv("Data/1_Raw_data/22_Kallnik/Flower_count.csv")

#Compare vars
compare_variables(check_flower_count_data, FlowerCount)
#Rename cols
FlowerCount = FlowerCount %>% 
rename(Flower_count = Flower_cover_m2) %>% 
mutate(Units = "Flower cover/m2")
#Add variables
FlowerCount = add_missing_variables(check_flower_count_data, FlowerCount) 
#Order data as template
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

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

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Create ordered metadata
Metadata <- tibble(
Doi = "https://doi.org/10.1111/oik.08902 and https://doi.org/10.1002/ecy.3712",
Dataset_description = "~15K bumble bee wildflower interactions recorded along
a ~1600 m elevation gradient in the Berchtesgadener Alps over 3 consecutive years,
with corresponding surveys of floral abundance.",
Taxa_recorded = "Bumble bees",
Sampling_year = "2010 to 2012",
Country = "Germany",
Habitat = "Pastures",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "Weekly sampling",
Sampling_method = "Transect",
Sampling_area_details = "Ten 60 x 6m transect per site",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = 10 * 60 * 6 * 25, #10 transects per site, 60 m long * 6 wide * 25 sites
Sampling_time_details = "5 mins per transect, 50 in total per site",
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
Authorship <- tibble(
Coauthor_name = "Katharina Kallnik",
Orcid = NA,
E_mail = "katharina.kallnik@uni-wuerzburg.de")

#Save data ----
#Create metadata list
Kallnik <- list(InteractionData, FlowerCount, Metadata, Authorship)
names(Kallnik) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Kallnik, file="Data/2_Processed_data/22_Kallnik.rds")
