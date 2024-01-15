#-------------------------------------------------------#
#Read and incorporate plant and pollinator names in master
#Also incorporate 
#-------------------------------------------------------#

library(dplyr)
library(tidyr)

#----------------------
#Read taxonomic and habitat data
#----------------------
plant_taxo = readRDS("Data/Species_taxonomy/Plant_taxonomy.rds")
poll_taxo = readRDS("Data/Species_taxonomy/Pollinator_taxonomy.rds")
habitat = readRDS("Data/Working_files/habitat.rds")

#Read master file
master = readRDS("Data/Working_files/Building_metaweb.rds")

#----------------------
#Add taxonomic data
#----------------------

#Check if the cols are the same
colnames(plant_taxo) == colnames(poll_taxo)
#check cols
colnames(plant_taxo)
#Check master cols
colnames(master)
#Bind by Old_name but first we need to rename plants and polls
master1 = master %>% 
rename(Plant_old_name = Plant_species) %>% 
rename(Pollinator_old_name = Pollinator_species) 
#Now rename in taxonomic files
#First plants
plant_taxo1 = plant_taxo
colnames(plant_taxo1) = paste0("Plant_", tolower(colnames(plant_taxo)))
colnames(plant_taxo1)
#Second pollinators
poll_taxo1 = poll_taxo
colnames(poll_taxo1) = paste0("Pollinator_", tolower(colnames(poll_taxo)))
colnames(poll_taxo1)

#Merge all for now
plant_taxo1
data = left_join(master1, plant_taxo1)
data1 = left_join(data, poll_taxo1, relationship = "many-to-many")

#Exclude records of plants or pollinators that
#are considered as unknown
data1 = data1 %>% 
filter(!Pollinator_accepted_name == "Unknown") %>% 
filter(!Plant_accepted_name == "Unknown")

#----------------------
#Add habitat data
#----------------------
#Rename col before merging
data1 = data1 %>% 
rename("Authors_habitat" = "Habitat")

#Left_join
data1 = left_join(data1, habitat,by = join_by(Study_id, Network_id, Latitude,
Longitude, Authors_habitat))

#Reorganise structure of data before saving
colnames(data1)

data1 = data1 %>% 
relocate(Corine_land_cover, .after = Authors_habitat) %>% 
relocate(SafeNet_habitat, .after = Corine_land_cover)

#----------------------
#Save data
#----------------------

#Save data
saveRDS(data1, "Data/3_Final_data/Interactions.rds")

#Important! Make all rows one single interaction
#1st uncount interactions
data2 = data1 %>% 
uncount(Interaction) %>% 
mutate(Interaction = 1)

#Keep all cols for now
saveRDS(data2, "Data/3_Final_data/Interactions_uncounted.rds")



