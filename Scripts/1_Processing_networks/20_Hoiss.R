#DATASET NUMBER 20; Bernhard Hoiss 
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(tidyverse)
library(lubridate)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/1_Raw_data/20_Hoiss/Interaction_data.csv")

#Compare vars
#compare_variables(check_interaction_data, data)

#Covert day into three separate cols
data = data %>% 
mutate(Date = as.Date(Date, format="%m-%d-%y")) %>% 
mutate(Year = year(ymd(Date))) %>% 
mutate(Month = month(ymd(Date))) %>% 
mutate(Day = day(ymd(Date))) 

#Add interaction col
data = data %>% 
mutate(Interaction = 1) %>% 
mutate(Sampling_method = "Transect") %>% 
mutate(Habitat = "Grassland") %>% 
mutate(Locality = NA) %>% 
mutate(Coordinate_precision = NA) %>% 
mutate(Comments = NA) %>% 
mutate(Temperature = NA) %>% 
mutate(Humidity = NA) %>% 
mutate(Sampling_effort_minutes = NA) %>% 
mutate(Sampling_area_square_meters = NA)
  
#Reorder variables
data = drop_variables(check_interaction_data, data) 

#Drop this last two columns that are going to be at the metadata
data = data %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Unify level
data = data %>% 
mutate(Sampling_method = "Plot")

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data,data$Site_id)

#Prepare flower count data ----
FlowerCount <- read_csv("Data/1_Raw_data/20_Hoiss/Flower_count.csv")

#Compare vars
#compare_variables(check_flower_count_data, flower_count)

#Covert day into three separate cols
FlowerCount = FlowerCount %>% 
mutate(Date = as.Date(Date, format="%m-%d-%y")) %>% 
mutate(Year = year(ymd(Date))) %>% 
mutate(Month = month(ymd(Date))) %>% 
mutate(Day = day(ymd(Date))) %>% 
rename(Flower_count = Flower_percent_cover) %>% 
mutate(Units = "Percent cover") %>% 
mutate(Comment = NA)

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
Doi = "https://doi.org/10.1111/gcb.12968",
Dataset_description = "Plant-pollinator interactions at 15 study sites
along ~1600 m elevation gradient in Berchtesgadener Alps; complementary
floral surveying. An experimental procedure simulating drought and snowmelt
was employed, so pay attention to the treatment to which each observation
belongs.",
Taxa_recorded = "All floral visitors",
Sampling_year = 2010,
Country = "Germany",
Habitat = "Grassland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "3 to 8",
Sampling_method = "Plot",
Sampling_area_details = "Four different treatments of 4 * 4 m plots per site",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = 4 * 4 * 4 * 15, #4*4m plot X 4 treatments X 15 sites
Sampling_time_details = "Two observations of 5 mins per treatment and site",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = 30 * 15 * 5, #30 mins per site X 15 sites X approximate 5 rounds
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- tibble(
Coauthor_name = "Bernhard Hoiss",
Orcid = NA,
E_mail = "bernhard.hoiss@anl.bayern.de")

#Save data ----
#Create metadata list
Hoiss <- list(InteractionData, FlowerCount, Metadata, Authorship)
names(Hoiss) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Hoiss, file="Data/2_Processed_data/20_Hoiss.rds")

