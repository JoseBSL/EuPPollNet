#DATASET NUMBER 2; Petanidou & Nakas

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/1_Raw_data/2_Petanidou/Interaction_data.csv")

#Quick clean of the interaction data
data = data %>% 
mutate(Plant_species = word(Plant_species, 1, 2), 
Pollinator_species = word(Pollinator_species, 1, 2)) %>%
rename(Latitude = latitude)  %>% 
rename(Longitude = longitude)  %>% 
mutate(Coordinate_precision = "10m") %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) %>% 
mutate(Latitude = as.numeric(Latitude)) %>% 
mutate(Longitude = as.numeric(Longitude)) %>% 
filter(!is.na(Plant_species)) %>% 
filter(!is.na(Pollinator_species)) %>% 
mutate(Day = as.double(Day)) %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Flower_data_merger = NA)

#Create a column that contains all the info to merge with flower counts
#This will help to unify across studies
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1), 
                                   word(Plant_species,2), 
                                   Site_id, Day,"-", Month, "-",Year))
#Unify level
data = data %>% 
mutate(Sampling_method = "Random_census")

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data,data$Site_id)

#Prepare flower count data ----
flower_count <- read_csv("Data/1_Raw_data/2_Petanidou/Flower_count.csv")

#Add leading 0's to days and month under 10
flower_count$Month <- ifelse(as.numeric(flower_count$Month) < 10, paste0("0", flower_count$Month), flower_count$Month)
flower_count$Day <- ifelse(as.numeric(flower_count$Day) < 10, paste0("0", flower_count$Day), flower_count$Day)

#Select cols of interest in order
FlowerCount <- flower_count %>%
select(Day, Month, Year, Site_id, Plant_species, Flower_count) %>%
mutate(Units = gsub("#", "Number", flower_count$Units)) %>% 
mutate(Comments = NA_character_) %>% 
mutate(Flower_data_merger = NA)

#Id to merge flower counts
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1), 
                                   word(Plant_species,2), 
                                   Site_id, Day,"-", Month, "-",Year))

#Order data as template and drop variables
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#Unify data structure
FlowerCount = change_str2(FlowerCount)

 
#Split interaction data into dataframes within a list
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)


#Create metadata ordered
Metadata <- tibble(
Doi = NA,
Dataset_description = "This dataset documents 6 different sites in Chios Island
Greece. The purpose of the samplings was to study post-fire succession
of plant-pollinator system so we have included data from two sites that
they were burnt three years before sampling. The remaining four sites
are unburnt scrublands with two of them being in the margins of cultivations
(olive grove and mastic trees). Each year 3 visits to each site
were performed and in each visit we measured both plant-pollinator
interactions and flower abundance. All samplings were performed during
the period that most pollinators are active and most plants are flowering
(April – End of June). We have included data of one year of sampling
but two more years are available.",
Taxa_recorded = "All flower visitors belonging to the orders:
Hymenoptera (with the exception of Formicidae), Diptera
(we mainly focused in Syrphidae and Bombylidae), Coleoptera and Lepidoptera.",
Sampling_year = 2015,
Country = "Greece",
Habitat = "Scrubland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = 3,
Sampling_method = "Random walks",
Sampling_area_details = NA,
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA,
Sampling_time_details = "120 mins per site and round",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = 6 * 3 * 120,
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()


#Prepare authorship data ----
Authorship <- tibble(
  Coauthor_name = c("Theodora Petanidou", "Georgios Nakas"),
  Orcid = c("0000-0003-1883-0945", "0000-0003-3023-5831"),
  E_mail = c("tpet@aegean.gr", "nakas.g@geo.aegean.gr"))


#Save data ----
#Create metadata list
Petanidou <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Petanidou) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Petanidou, file="Data/2_Processed_data/2_Petanidou.rds")

