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
data <- read_csv("Data/1_Raw_data/29_30_31_STEP/Interaction_data.csv")

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

#There is one network (site_id) that has 2 coordinates
#Unify for simplicity as they are practically identical
#latitude
latitude_cartaya = data %>% 
select(Site_id, Latitude)  %>% 
filter(Site_id == "Cartaya") %>% 
pull(Latitude)
#longitude
longitude_cartaya = data %>% 
select(Site_id, Longitude)  %>% 
filter(Site_id == "Cartaya") %>% 
pull(Longitude)
#Add coordinates
#1st latitude
data = data %>% 
mutate(Latitude = 
  case_when(Site_id == "Cartaya" ~ latitude_cartaya[1],
  TRUE ~ Latitude))
#2nd longitude
data = data %>% 
mutate(Longitude = 
  case_when(Site_id == "Cartaya" ~ longitude_cartaya[1],
  TRUE ~ Longitude))

#Add flower info cols
data = data %>% 
mutate(Flower_data_merger = NA) %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Comments = NA)

#Delete sp from species names as flower count datadoes not contain it
data = data %>% 
mutate(Plant_species = str_replace_all(Plant_species, " sp", ""))

#Set floral merger col as we have floral counts
#Fix NA's in second word
data = data %>% 
mutate(second_word = na_if(word(Plant_species, 2, sep = " "), "NA")) %>% 
mutate(second_word = ifelse(is.na(second_word), "", second_word))

#Set floral merger col as we have floral counts
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1),second_word, "_",
                                   Site_id,  "_",Day, "_", Month, "_", Year)) %>% 
select(!second_word)

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list (separate now by site)
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount <- read_csv("Data/1_Raw_data/29_30_31_STEP/Flower_count.csv")

#Compare vars
compare_variables(check_flower_count_data, FlowerCount)
#Select just Spain
FlowerCount = FlowerCount %>% filter(country == "Spain")
#Add flower data merger column
FlowerCount = FlowerCount %>%
mutate(Flower_data_merger =NA) %>% 
mutate(Comments = NA)

#Check for double cases
FlowerCount = FlowerCount %>%
group_by_at(vars(-Flower_count)) %>%
mutate(row_number = row_number()) %>%
distinct() %>%
filter(row_number == 1) %>%
select(-row_number)

#Order data as template and drop variables
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
mutate(Comments = as.character(Comments))

#Set floral merger col as we have floral counts
#Fix NA's in second word
FlowerCount = FlowerCount %>% 
mutate(second_word = na_if(word(Plant_species, 2, sep = " "), "NA")) %>% 
mutate(second_word = ifelse(is.na(second_word), "", second_word))

#Set floral merger col as we have floral counts
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1),second_word, "_",
                                   Site_id,  "_",Day, "_", Month, "_", Year)) %>% 
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
Doi = "https://doi.org/10.1038/s41559-017-0249-9",
Dataset_description = "This is the Spanish dataset from the project
Status and Trends of European Pollinators (STEP)",
Taxa_recorded = "Bees and syrphids",
Sampling_year = "2011 and 2012",
Country = "Spain",
Habitat = "Scrubland",
Sampling_sites = nlevels(factor(data$Site_id)),
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
saveRDS(Magrach, file="Data/2_Processed_data/29_Magrach.rds")



