#DATASET NUMBER 51; Petanidou
#Dataset sent by T. Petanidou

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)

#Read empty templates to compare with
source("Scripts/Processing/Functions/Empty_templates.R") 
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/51_Petanidou/Interaction_data.csv", locale = locale(encoding = "latin1"))

#Compare vars
#compare_variables(check_interaction_data, data)
#Nothing missing!

#Drop sampling effort and square area (it would be added on the metadata)
#Replace "m" for empty space in coordinate precision
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) %>% 
mutate(Coordinate_precision = str_replace(Coordinate_precision, "m", ""))


#Some plant names include 2 spp
#After asking authors we need to divide those in 2 rows
data_1spp = data %>% 
filter(!str_detect(Plant_species, "[|]"))
#Now work on those
data_2spp = data %>% 
filter(str_detect(Plant_species, "[|]"))
#Split col into 2 cols
data_2spp = data_2spp %>%
separate(Plant_species, c("A", "B"), "[|]")
#Create dataframe 1
data_2spp_1 = data_2spp %>% 
select(-B) %>% 
rename(Plant_species = A) %>% 
mutate(Plant_species = word(Plant_species, 1,2))
#Create dataframe 2
data_2spp_2 = data_2spp %>% 
select(-A) %>% 
rename(Plant_species = B) %>% 
mutate(Plant_species = word(Plant_species, 1,2))
#Bind dataframes (recovered spp and records without mistakes)
data = bind_rows(data_2spp_1, data_2spp_2, data_1spp)

#Select just 2 words for the rest of records too
data = data %>% 
mutate(Plant_species = word(Plant_species, 1,2))

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ---- 
flower_count <- read_csv("Data/Raw_data/51_Petanidou/Flower_count.csv")

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#No misisng vars

#Split interaction data into dataframes within a list
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
Doi = NA,
Dataset_description = "This dataset documents 9 different sites
within 7 islands in the the Aegean",
Taxa_recorded = "All flower visitors belonging to the orders:
Hymenoptera (except Formicidae), Diptera (we mainly focused
on Syrphidae and Bombyliidae), Coleoptera and Lepidoptera",
Sampling_year = "2012 to 2015",
Country = "Greece",
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
  Coauthor_name = c("Theodora Petanidou", "Georgios Nakas"),
  Orcid = c("0000-0003-1883-0945", "0000-0003-3023-5831"),
  E_mail = c("tpet@aegean.gr", "nakas.g@geo.aegean.gr"))

#Save data ----
#Create metadata list
Petanidou <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Petanidou) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Petanidou, file="Data/Clean_data/51_Petanidou.rds")





