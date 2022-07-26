#DATASET NUMBER 3; Michez, Fiordaliso & Reverte

#Load libraries
library(tidyverse)

#Prepare interaction data ----
#Read comma delimited
data <- read_csv("Data/Raw_data/3_Michez/Michez.csv")
#Check colnames and first 5 rows
colnames(data)
data %>% slice_head(n = 5)

#Things to do
#Rename Terrestribombus sp. to Bombus terrestris, 90% of them are terrestris are not locuorum
#Other option is to rename species with Bombus cf. terrestris
InteractionData = data %>% 
mutate(Coordinate_precision = NA) %>%
mutate(Elevation = NA) %>% 
mutate(Comments = NA) %>% 
mutate(Temperature = NA) %>% 
mutate(Humidiy = NA) %>% 
select(Plant_species, Pollinator_species, Interaction, 
       Sampling_method, Sampling_effort_minutes, Sampling_area_square_meters,
       Site_id, Habitat, Country, Locality, Latitude, Longitude,
       Coordinate_precision, Elevation, Day, Month, Year, Comments,
       Temperature, Humidiy) %>%
mutate(Pollinator_species = str_replace(Pollinator_species, "Terrestribombus sp.", "Bombus terrestris"))

#Split data into different dataframes based on survey name
InteractionData <- split(InteractionData, InteractionData$Site_id)

#Prepare flower count data ----
FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = NA, Plant_species = NA,
                      Flower_count = NA, Units = NA, Comment = NA)

#Prepare metadata data ----
Metadata <- tibble(
  Doi = NA,
  Dataset_description = "This dataset documents 71 different sites in the province of Hainaut,
  Wallonia, Belgium. They have been collected for different master thesis and there are sites
  on different habitats. The region is post industrial, so terrils (heat slap),
  carrieres (quarries), ruderal sites, parks (urban), and prairies are included.
  Samplings were performed on 2020. For COVID reasons, the sites were visited only from
  May to September, 3 to 5 visits to each site were performed. We measured only wild bees.
  Each sampling on each site consisted on a non-linear transect that lasted 20 minutes.",
  Taxa_recorded = "Bees")

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
saveRDS(Michez, file="Data/Clean_data/3_Michez.RData")

