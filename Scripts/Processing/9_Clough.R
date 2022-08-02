#DATASET NUMBER 11; Clough

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data = read_csv("Data/Raw_data/9_Clough/interaction_data_clough.csv") %>% 
select(Plant_species, Pollinator_species, Interaction, Sampling_method,
       Sampling_effort_minutes, Sampling_area_square_meters,
       Site_id, Habitat, Country, Locality, Latitude, Longitude,
       Coordinate_precision, Elevation, Day, Month, Year, Comments,
       Temperature, Humidity)

#Convert coordinates to lat/lon
library(rgdal)
coordinates = data %>% select(Longitude, Latitude)
coordinates <- SpatialPoints (coordinates, proj4string = CRS ('+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
cord.WGS84 <- spTransform(coordinates, CRS('+proj=longlat +datum=WGS84 +no_defs'))
#Add new cols to the tibble
data = data %>% 
mutate(Latitude = cord.WGS84$Latitude) %>% 
mutate(Longitude = cord.WGS84$Longitude)
#Split by Site_id
InteractionData <- split(data, data$Site_id)


#Prepare flower count data ----
flower_count = read_csv("Data/Raw_data/9_Clough/flower_count_clough.csv")
#Split data into different dataframes based on survey name
FlowerCount <- split(flower_count, flower_count$Site_id)


#Prepare metadata data ----
Metadata <- tibble(
  Doi = NA,
  Dataset_description = "This dataset documents 18 different sites in southernmost Sweden
  (Scania) along a gradient of landscape scale land-use composition.
  The focal habitat are Fennoscandian lowland species-rich dry to mesic grasslands.
  In each site a pair of 3x3 m study plots were established in temporary (April to August)
  5x9m cattle exclosures (electric fence). One plot was subjected to pollinator reduction
  treatment and is not included here. The control plot is completely open and it is the data
  from those plots that are inlcuded here. Three to four visits to each site were performed,
  and in each visit we measured both plant-pollinator interactions and flower abundance. 
  Each visit contained a period of 15 minutes where flower visitors were caught for
  species-level identification. It is this data that is being contributed here, including the 
  observations of insects that were not caught (unintentionally or because identification in the
  field was possible). All floral visitors are included. Two years of data are available
  (3 visits in 2020 and 4 in 2021). A data paper and several manuscripts are in preparation
  where this data is used. A DOI for the dataset is forthcoming. As co-authors in sheet 3
  are listed the three persones responible for the conception,
  design and part of the data collection, quality control and curation. Other signficant
  contributions are by the Lund Biological Museum staff that identified most of the
  collected material (Rune Bygebjerg, Christoffer Fägerström, Christer Hansson, Karin Johnson,
  Jadranka Rota, Ellen Sandström) and the field assistants that contributed to data recording
  (Adam Bates, Sandra Blasiussion, Karlis Kenklis, Mirela Miric, Alessandro Pinto).
  Uncertainty/ambiguity in species id is marked by use of slash in the case of species groups
  (e.g. for some of the bumblebees) or a vertical bar whenever a subset of individuals
  observed were caught for later identification and more than one species was found
  (commonly for pollen beetles). Elevation data is from GSD-Höjddata grid 50+ from Lantmäteriet.
  Funding statement: The data was recorded under the project DrivenByPollinators, which
  received funding from the European Research Council (ERC) under the European Union’s
  Horizon 2020 research and innovation programme (grant agreement No. 819374).",
  Taxa_recorded = "All flower visitors")

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Veronica Hederström", "Theresia Krausl", "Yann Clough"),
  Orcid = c("0000-0002-9231-3156", "0000-0003-2849-4279", "0000-0002-2901-7602"),
  E_mail = c("veronica.hederstrom@cec.lu.se", "theresia.krausl@cec.lu.se", 
             "yann.clough@cec.lu.se"))


#Save data ----
#Create metadata list
Clough <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Clough) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 11
saveRDS(Clough, file="Data/Clean_data/11_Clough.RData") 
