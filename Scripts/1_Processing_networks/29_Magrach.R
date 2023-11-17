#DATASET NUMBER 29; STEP PROJECT: SPAIN dataset
#This metadataset was sent by A. Magrach
#The dataset of Germany was already sent by other colleagues
#So I won't add this country

#Read templates to compare with
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/29_30_31_STEP/Interaction_data.csv")

#Check col names with template
compare_variables(check_interaction_data, data)

#Check levels per country
levels(factor(data$Country))

#Select 1st level that we don´t have: SPAIN
data = data %>% filter(Country == "Spain")

#Select main cols
data = data %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata
#Check number of sites
levels(factor(data$Site_id))

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list (separate now by site)
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
flower_count <- read_csv("Data/Raw_data/29_30_31_STEP/Flower_count.csv")

#Compare vars
compare_variables(check_flower_count_data, flower_count)
#Select just Spain
flower_count = flower_count %>% filter(country == "Spain")
#Order data as template and drop variables
flower_count = drop_variables(check_flower_count_data, flower_count) 

#Split interaction data into dataframes within a list
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Create metadata ordered
Metadata <- tibble(
Doi = "https://doi.org/10.1038/s41559-017-0249-9",
Dataset_description = "This is the Spanish dataset from the project
Status and Trends of European Pollinators (STEP)",
Taxa_recorded = "Bees and syrphids",
Sampling_year = "2011 and 2012",
Country = "Spain",
Habitat = "Scrubland",
Sampling_sites = "17",
Sampling_rounds = "4 per year",
Sampling_method = "Transect",
Sampling_area_details = "Two transects per site of 150 * 1m",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = 300 * 17, #300 m per 17 sites
Sampling_time_details = "15 mins per transect",
Sampling_time_species_round_min = "",
Sampling_time_total_min = 15 * 2 * 17 * 4 * 2, #Logic: 15 mins per transect * 2 transects *17 sites * 4 times per year * 2 years
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()


#Prepare authorship data ----
Authorship <- tibble(
Coauthor_name = c("Ainhoa Magrach", 
"Ignasi Bartomeus", "Montserrat Vilà", "Juan Pedro González-Varo"),
Orcid = c("0000-0003-2155-7556", "0000-0001-7893-4389",
          "000-0003-3171-8261", "0000-0003-1439-6475"),
E_mail = c("ainhoa.magrach@bc3research.org", "nacho.bartomeus@gmail.com",
           "montse.vila@ebd.csic.es", "juanpe@ebd.csic.es"))

#Save data ----
#Create metadata list
Magrach <- list(InteractionData, FlowerCount, Metadata, Authorship)
names(Magrach) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Magrach, file="Data/Clean_data/29_Magrach.rds")



