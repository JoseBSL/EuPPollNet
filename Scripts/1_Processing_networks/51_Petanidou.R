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
data <- read_csv("Data/1_Raw_data/51_Petanidou/Interaction_data.csv", locale = locale(encoding = "latin1"))

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

#There is one network (site_id) that has 2 coordinates
#Unify for simplicity as they are practically identical
#latitude
latitude = data %>% 
select(Site_id, Latitude)  %>% 
filter(Site_id == "Poisses") %>% 
pull(Latitude)
#longitude
longitude = data %>% 
select(Site_id, Longitude)  %>% 
filter(Site_id == "Poisses") %>% 
pull(Longitude)
#Add coordinates
#1st latitude
data = data %>% 
mutate(Latitude = 
  case_when(Site_id == "Poisses" ~ latitude[1],
  TRUE ~ Latitude))
#2nd longitude
data = data %>% 
mutate(Longitude = 
  case_when(Site_id == "Poisses" ~ longitude[1],
  TRUE ~ Longitude))

#Add flower info cols
data = data %>% 
mutate(Flower_data = "Yes") 


#Delete leading 0's in month -they give issues when merging floral counts-
data = data %>% 
mutate(Month = as.numeric(Month)) %>% 
mutate(Day = as.numeric(Day))

#Set merger col
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_" , word(Plant_species,2), "_", Site_id, "_", Month, "_", Year))

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)

#Prepare flower count data ---- 
FlowerCount = read_csv("Data/1_Raw_data/51_Petanidou/Flower_count.csv")

FlowerCount = FlowerCount %>% 
rename(Comments = Comment)

#Fix quickly some species names (more could be fixed)
FlowerCount = FlowerCount %>% 
mutate(Plant_species = case_when(
  Plant_species == "Anchusa undulata subsp. hybrida (Ten.) Cout." ~ "Anchusa hybrida",
  Plant_species == "Ballota acetabulosa (L.) Benth." ~ "Pseudodictamnus acetabulosus",
  Plant_species == "Legousia speculum-veneris" ~ "Legousia pentagonia",
  Plant_species == "Lathyrus articulatus L." ~ "Lathyrus clymenum",
  TRUE ~ Plant_species))

#Set merger col
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species, 1), "_" , word(Plant_species,2), "_", Site_id, "_", Month,"_", Year))

#Drop not needed vars
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#Calculate an average of flowers per site and month (we do not have a unique id to merge)
FlowerCount = FlowerCount %>% 
group_by_at(vars(-c(Flower_count, Day))) %>% 
summarise(Flower_count = mean(Flower_count)) %>% 
mutate(Day = NA)

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
saveRDS(Petanidou, file="Data/2_Processed_data/51_Petanidou.rds")





