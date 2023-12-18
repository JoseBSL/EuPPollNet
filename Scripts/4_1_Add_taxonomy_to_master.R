#-------------------------------------------------------#
#Read and incorporate plant and pollinator names in master
#-------------------------------------------------------#

library(dplyr)
library(tidyr)

#----------------------
#Read taxonomic data
#----------------------
plant_taxo = readRDS("Data/Species_taxonomy/Plant_taxonomy.rds")
poll_taxo = readRDS("Data/Species_taxonomy/Pollinator_taxonomy.rds")
#Read master file
master = readRDS("Data/Processing/Building_metaweb.rds")
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

#Save data
saveRDS(data1, "Data/Interactions.rds")

#Important! Make all rows one single interaction
#1st uncount interactions
data2 = data1 %>% 
uncount(Interaction) %>% 
mutate(Interaction = 1)

#----------------------
#Save data
#----------------------
#Keep all cols for now
saveRDS(data2, "Data/Interactions_uncounted.rds")



