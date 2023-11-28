#DATASET NUMBER 3; Michez, Fiordaliso & Reverte

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
#Read comma delimited
data <- read_csv("Data/Raw_data/3_Michez/interaction_data.csv")
#Check colnames and first 5 rows
colnames(data)
data %>% slice_head(n = 5)

#For now keep it as 1 interaction these special cases
data = data %>% 
mutate(Interaction = case_when(Interaction == "#" ~ "1",
                              TRUE ~ Interaction))

#Things to do
#Rename Terrestribombus sp. to Bombus terrestris, 90% of them are terrestris are not locuorum
#Other option is to rename species with Bombus cf. terrestris
data = data %>% 
mutate(Coordinate_precision = NA) %>%
mutate(Elevation = NA) %>% 
mutate(Comments = NA) %>% 
mutate(Temperature = NA) %>% 
mutate(Humidity = NA) %>% 
select(Plant_species, Pollinator_species, Interaction, 
       Sampling_method, Sampling_effort_minutes, Sampling_area_square_meters,
       Site_id, Habitat, Country, Locality, Latitude, Longitude,
       Coordinate_precision, Elevation, Day, Month, Year, Comments,
       Temperature, Humidity) %>%
mutate(Pollinator_species = str_replace(Pollinator_species, "Terrestribombus sp.", "Bombus terrestris")) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) %>% 
mutate(Interaction = as.integer(Interaction))



#Unify structure of data
data = change_str(data)

#Split data into different dataframes based on survey name
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = NA, Plant_species = NA,
                      Flower_count = NA, Units = NA, Comment = NA)

#Prepare metadata data ----

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
Doi = NA,
Dataset_description = "This dataset documents 71 different sites in the province of Hainaut,
Wallonia, Belgium. They have been collected for different master thesis and there are sites
on different habitats. The region is post industrial, so terrils (heat slap),
carrieres (quarries), ruderal sites, parks (urban), and prairies are included.
Samplings were performed on 2020. For COVID reasons, the sites were visited only from
May to September, 3 to 5 visits to each site were performed. We measured only wild bees.
Each sampling on each site consisted on a non-linear transect that lasted 20 minutes.",
Taxa_recorded = "Bees",
Sampling_year = "2020",
Country = "Belgium",
Habitat = "See Dataset_description",
Sampling_sites = "71",
Sampling_rounds = "3 to 5",
Sampling_method = "Transects",
Sampling_area_details = "Non-linear transects",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA ,
Sampling_time_details = "20 mins per site",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = 20 * 71 * 4, #This number (71 * 4) can be refined, at the moment is an approximate time
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "No")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Denis Michez", "William Fiordaliso", "Sara Reverte"),
  Orcid = c("0000-0001-8880-1838", "0000-0002-9667-6800", "0000-0002-2924-3394"),
  E_mail = c("denis.michez@umons.ac.be", "william.fiordaliso@umons.ac.be", 
             "sara.revertes@umons.ac.be"))


#Save data ----
#Create metadata list
Michez <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Michez) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Michez, file="Data/Clean_data/3_Michez.rds")

