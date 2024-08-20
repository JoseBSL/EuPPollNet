#DATASET NUMBER 54; Lazaro
#Sent By A. Lazaro 

source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)
library(lubridate)
library(anytime)

#Read empty templates to compare with
source("Scripts/Processing/Functions/Empty_templates.R") 
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read_csv("Data/1_Raw_Data/54_Lazaro/Interaction_data.csv")

#Check against template
compare_variables(check_interaction_data, data)

#All columns are there!
#Only add if there is flower info available
data = data %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_" , word(Plant_species,2), "_", Site_id, "_",
                                   Day, "_", Month, "_", Year))
#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)

#Prepare flower count data ---- 
FlowerCount = read_csv("Data/1_Raw_data/54_Lazaro/Flower_count.csv")

#Group by and calculate acerage per species, date and site
#as there is no finer way to bind with the interaction dataset
FlowerCount = FlowerCount %>% 
group_by(Day, Month, Year, Site_id, Plant_species, Comments, Units) %>% 
summarise(Flower_count = mean(Flower_count))

#Set merger col
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_" , word(Plant_species,2), "_", Site_id, "_", Month,"_", Year))

#Drop not needed vars
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#Set common structure
FlowerCount = change_str2(FlowerCount)

#Split interaction data into dataframes within a list
FlowerCount = split(FlowerCount, FlowerCount$Site_id)


#Prepare metadata data ----
#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
Doi = "https://doi.org/10.1111/1365-2435.14160",
Dataset_description = "This dataset documents 20 different sites in Mallorca,
along a habitat loss gradient. The focal habitat is natural shrublands of wild Olea europaea.
We estimated plant-pollinator interactions along 3 fixed transects of 100 x2 m 1n 1 ha,
and flower abundance (30 sampling 1 x1 m squares).  Seven sampling censuses of 60 minutes
per study site in 2018. We covered the full phenology of most plants and pollinators.",
Taxa_recorded = "All flower visitors",
Sampling_year = "2018",
Country = "Spain",
Habitat = "Scrubland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "",
Sampling_method = "",
Sampling_area_details = "",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA ,
Sampling_time_details = "",
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
  Coauthor_name = c("Amparo Lázaro", "Carmelo Gómez-Martínez", "Miguel A. González-Estévez"),
  Orcid = c("0000-0001-5626-4134", "0000-0003-1449-0138", ""),
  E_mail = c("amparo.lazaro@imedea.uib-csic.es", "cgomez@imedea.uib-csic.es", "mgonzalez@imedea.uib-csic.es"))

#Save data ----
#Create metadata list
Lazaro <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Lazaro) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Lazaro, file="Data/2_Processed_data/54_Lazaro.rds")



