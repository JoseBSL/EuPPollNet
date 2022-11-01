#DATASET NUMBER 13; Karise
#Just bumblebees

#Load libraries
library(tidyverse)

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/Raw_data/11_Karise/interaction_data_karise.csv")

#Drop some rows with na's in plants and polls
data = data %>% 
drop_na(Plant_species) %>% 
drop_na(Pollinator_species)

#lat and long cols are swapped, fix
#Create new dataframe with coordinates 
coord = data %>% 
mutate(Latitude1 = Longitude) %>% 
mutate(Longitude1 = Latitude) %>% 
select(Latitude1, Longitude1) %>% 
rename(Latitude = Latitude1) %>% 
rename(Longitude = Longitude1)
#Mutate cols now with correct coordinates
data =  data %>% 
mutate(Latitude = coord$Latitude) %>% 
mutate(Longitude = coord$Longitude)


#Split by site, just for createing the listed name in this case
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = NA, Plant_species = NA,
                     Flower_count = NA, Units = NA, Comment = NA)


#Prepare metadata data ----
Metadata <- tibble(
  Doi = NA,
  Dataset_description = "66 different sites, from these 33 in Southern Estonia and 33 in
  Northern-Central Estonia. Bumble bees on field edges (500 m x 2 m walking transect.
  The entire transect on field edges or 400 m on field edges and 100 m inside entomophilous
  crop if this excisted in the area). Monitoring is carried out annually since 2006,
  but forage plant species was named only in years indicated. Also current year,
  the dataset will be created.",
  Taxa_recorded = "Just bumblebees")

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

