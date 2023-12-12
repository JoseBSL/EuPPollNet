#DATASET NUMBER 13; Karise
#Just bumblebees

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/Raw_data/13_Karise/Interaction_data.csv", locale = locale(decimal_mark = ","))
#Small hack to add coodinates
data1 = read_csv("Data/Raw_data/13_Karise/Interaction_data.csv")

#lat and long cols are swapped, fix
#Create new dataframe with coordinates 
coord = data1 %>% 
mutate(Latitude1 = Longitude) %>% 
mutate(Longitude1 = Latitude) %>% 
select(Latitude1, Longitude1) %>% 
rename(Latitude = Latitude1) %>% 
rename(Longitude = Longitude1)

#Add fixed coordinates to dataset
data = data %>% 
mutate(Latitude = coord$Latitude) %>% 
mutate(Longitude = coord$Longitude)

#Mutate cols now with correct coordinates
data =  data %>% 
mutate(Latitude = coord$Latitude) %>% 
mutate(Longitude = coord$Longitude) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Drop some rows with na's in plants and polls
data = data %>% 
drop_na(Plant_species) %>% 
drop_na(Pollinator_species)


#Convert missing interaction info as 1 for now
data = data %>% 
mutate(Interaction = case_when(is.na(Interaction) ~ 1,
                              TRUE ~ Interaction))


#Fix variable name in sampling method
data = data %>% 
mutate(Sampling_method = "Transect")

#Unify structure of data
data = change_str(data)

#Split by site, just for createing the listed name in this case
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = NA, Plant_species = NA,
                     Flower_count = NA, Units = NA, Comment = NA)


#Prepare metadata data ----

#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata <- tibble(
Doi = NA,
Dataset_description = "66 different sites, from these 33 in Southern Estonia and 33 in
Northern-Central Estonia. Bumble bees on field edges (500 m x 2 m walking transect.
The entire transect on field edges or 400 m on field edges and 100 m inside entomophilous
crop if this excisted in the area). Monitoring is carried out annually since 2006,
but forage plant species was named only in years indicated. Also current year,
the dataset will be created.",
Taxa_recorded = "Just bumblebees",
Sampling_year = "2014 to 2021",
Country = "Estonia.",
Habitat = "Field edges on agricultutal land",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = NA,
Sampling_method = "Transect",
Sampling_area_details = NA,
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA,
Sampling_time_details = NA,
Sampling_time_species_round_min = NA,
Sampling_time_total_min = NA,
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "No")


#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Reet Karise", "Marika Mänd"),
  Orcid = c("0000-0002-6921-0167 ", "0000-0003-4898-5817"),
  E_mail = c("reet.karise@emu.ee", "marika.mand@emu.ee"))

#Save data ----
#Create list with all dataframes of interest
Karise <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Karise) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 13
saveRDS(Karise, file="Data/Clean_data/13_Karise.rds") 

