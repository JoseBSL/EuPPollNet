#DATASET NUMBER 49; Hervias-Parejo
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
data <- read_csv("Data/1_Raw_data/49_Hervias-Parejo/Interaction_data.csv", locale = locale(encoding = "latin1"))

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

#Add flower info cols
data = data %>% 
mutate(Flower_data = "Yes") 

#Set merger col
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_" , word(Plant_species,2), "_", Site_id, "_",
                                   Day, "_", Month, "_", Year))
#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ---- 
FlowerCount = read_csv("Data/1_Raw_data/49_Hervias-Parejo/Flower_count.csv")%>% 
mutate(Comments = NA)
#Rename plant spp col
FlowerCount = FlowerCount %>% 
rename("Plant_species" = "Plant species") %>% 
select(!...9) #extra col from excel

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#No misisng vars

#Set merger col
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_" , word(Plant_species,2), "_", Site_id, "_",
                                   Day,"_", Month,"_", Year))

#Drop not needed vars
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#Conduct average per species as we don't have a col to link by individual
#Seems ok for now, as species had approx homogeneous numbers of flowers
#But maybe ask Sandra
FlowerCount = FlowerCount %>% 
group_by_at(vars(-Flower_count)) %>% 
summarise(Flower_count = mean(Flower_count))

#Change units as now does not represent total number
FlowerCount = FlowerCount %>% 
mutate(Units = "Average flowers per species-site-day") %>% 
mutate(Comments = "Raw flower counts are available")

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
Doi = "https://doi.org/10.1101/2023.07.02.547400",
Dataset_description = "This dataset documents 1 islet 
(Na Redona, 39° 10’ 5 “ N, 2° 58’ 35” 443 E) of
approximately 11 ha and 56 m high in the Cabrera
Archipelago National Park (Balearic Islands, Western
Mediterranean Sea). Its primary habitat is Mediterranean
shrubland with a relatively rich plant species diversity
(ca. 108). A team of five people visited the islet for
five consecutive days at the peak of flowering (April/May).
We measured both plant-pollinator interactions and flower
abundance of each censused individual plant. We cover the
full phenology of most plants and pollinators and measure
all floral visitors. Two years of data are available.",
Taxa_recorded = "All floral visitors",
Sampling_year = "2018 and 2019",
Country = "Spain",
Habitat = "Scrubland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "",
Sampling_method = "Focal_observation",
Sampling_area_details = "6 transects 100 m long 10 m wide",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = "6000" ,
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
  Coauthor_name = c("Sandra Hervias-Parejo", "Anna Traveset"),
  Orcid = c("0000-0002-5377-3619", "0000-0002-1816-1334"),
  E_mail = c("shervias@imedea.uib-csic.es", "atraveset@csic.es"))

#Save data ----
#Create metadata list
Hervias_Parejo = list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Hervias_Parejo) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Hervias_Parejo, file="Data/2_Processed_data/49_Hervias-Parejo.rds")




