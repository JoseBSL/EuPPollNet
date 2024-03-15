#DATASET NUMBER 31; STEP PROJECT: UK dataset
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
data <- read_csv("Data/1_Raw_data/29_30_31_STEP/Interaction_data.csv")

#Add flower info cols
data = data %>% 
mutate(Flower_data_merger = NA) %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Comments = NA)

#Check col names with template
compare_variables(check_interaction_data, data)

#Check levels per country
levels(factor(data$Country))

#Select 1st level that we don´t have: SPAIN
data = data %>% filter(Country == "UK") %>% 
mutate(Country = "England")

#Filter out missing field
data = data %>% 
filter(!Plant_species == "[Unknown] [Unknown]") %>% 
filter(!Plant_species == "[Nothing] [Nothing]") %>% 
filter(!Pollinator_species == "[Unknown] [Unknown]")

#Select main cols
data = data %>% 
  select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata
#Check number of sites
levels(factor(data$Site_id))

#Set floral merger col as we have floral counts
#Fix NA's in second word
data = data %>% 
mutate(second_word = na_if(word(Plant_species, 2, sep = " "), "NA")) %>% 
mutate(second_word = ifelse(is.na(second_word), "", second_word))

#Set floral merger col as we have floral counts
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1), second_word, "_",
                                   Site_id, "_", Day,"_", Month,"_", Year)) %>% 
select(!second_word)

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list (separate now by site)
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount <- read_csv("Data/1_Raw_data/29_30_31_STEP/Flower_count.csv")

#Add flower data merger column
FlowerCount = FlowerCount %>%
mutate(Flower_data_merger =NA) %>% 
mutate(Comments = NA)

#Filter out missing field
FlowerCount = FlowerCount %>% 
filter(!Plant_species == "[Unknown] [Unknown]") 
#Compare vars
compare_variables(check_flower_count_data, FlowerCount)
#Select just Spain
FlowerCount = FlowerCount %>% filter(country == "UK")
#Order data as template and drop variables
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#Check for double cases and drop those
FlowerCount = FlowerCount %>%
group_by_at(vars(-Flower_count)) %>%
mutate(row_number = row_number()) %>%
distinct() %>%
filter(row_number == 1) %>%
select(-row_number)

#Set floral merger col as we have floral counts
#Fix NA's in second word
FlowerCount = FlowerCount %>% 
mutate(second_word = na_if(word(Plant_species, 2, sep = " "), "NA")) %>% 
mutate(second_word = ifelse(is.na(second_word), "", second_word))

#Set floral merger col as we have floral counts
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1), second_word, "_",
                                   Site_id, "_", Day,"_", Month,"_", Year)) %>% 
select(!second_word)

#Unify structure of data
FlowerCount = change_str2(FlowerCount)

#Split interaction data into dataframes within a list
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----
#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Create metadata ordered
Metadata <- tibble(
  Doi = "https://doi.org/10.1111/ele.12657",
  Dataset_description = "This is the English dataset from the project
Status and Trends of European Pollinators (STEP)",
  Taxa_recorded = "Bees and syrphids",
  Sampling_year = "2012 and 2013",
  Country = "England",
  Habitat = "Grassland",
  Sampling_sites = nlevels(factor(data$Site_id)),
  Sampling_rounds = "4 per year",
  Sampling_method = "Transect",
  Sampling_area_details = "Two transects per site of 150 * 1m",
  Sampling_area_species_m2 = NA,
  Sampling_area_total_m2 = 300 * 15, #300 m per 15 sites
  Sampling_time_details = "15 mins per transect",
  Sampling_time_species_round_min = "",
  Sampling_time_total_min = 15 * 2 * 15 * 4 * 2, #Logic: 15 mins per transect * 2 transects *15 sites * 4 times per year * 2 years
  Total_plant_species = nrow(plant_single_cases),
  Total_pollinator_species = nrow(pollinator_single_cases),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- tibble(
Coauthor_name = c("Stuart P. M. Roberts", "Jennifer B. Wickens",
     "Victoria B. Wickens", "Simon G. Potts"),
Orcid = c("", "0000-0003-0475-6780", "0000-0002-2295-0635",
    "0000-0002-2045-980X"),
E_mail = c("aas03spr@reading.ac.uk","jenniferwickens1987@hotmail.com",
           "victoriawickens1987@hotmail.com", "s.g.potts@reading.ac.uk"))

#Save data ----
#Create metadata list
Roberts <- list(InteractionData, FlowerCount, Metadata, Authorship)
names(Roberts) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Roberts, file="Data/2_Processed_data/31_Roberts.rds")



