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
habitat = readRDS("Data/Working_files/Habitat.rds")
bioregion = readRDS("Data/Working_files/Bioregion.rds")

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
#Bind data
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

#----------------------
#Add bioregion data
#----------------------
data1 = left_join(data1, bioregion, by = join_by(Study_id, Latitude,
Longitude))
#Relocate column
data1 = data1 %>% 
rename(Bioregion = Bioregions)


#Edit cols that are included in the SafeNet database
#This decision tries to simplify the number of columns while 
#keep high amount of relevant information
#Don't add some cols
#Tasks
#Create date column
library(lubridate)
data1 <- data1 %>%
  mutate(Month = coalesce(Month, 1),  # Replace NA in Month with 1
         Day = coalesce(Day, 1),      # Replace NA in Day with 1
         Date = make_date(Year, Month, Day))
#Note that dates 1/1 are an artefact

#Select cols and establish final order
data1 = data1 %>% 
select(Study_id, Network_id, Site_id, Sampling_method, Authors_habitat, 
       SafeNet_habitat, Bioregion, Country, Locality,
       Latitude, Longitude, Date, Interaction, Plant_old_name, Plant_accepted_name,
       Plant_rank, Plant_status, Plant_matchtype, Plant_order, Plant_family, Plant_genus,
       Plant_unsure_id, Plant_uncertainty_type, Pollinator_old_name, Pollinator_accepted_name, 
       Pollinator_rank, Pollinator_status, Pollinator_matchtype, Pollinator_order, Pollinator_family, 
       Pollinator_genus, Pollinator_unsure_id, Pollinator_uncertainty_type, Flower_data, Flower_data_merger)

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

#Safety checking
missing = data1 %>% 
filter(is.na(Interaction))

#Select cols and establish final order
data2 = data2 %>% 
select(Study_id, Network_id, Sampling_method, Authors_habitat, 
       SafeNet_habitat, Bioregion, Country, Locality,
       Latitude, Longitude, Date, Interaction, Plant_old_name, Plant_accepted_name,
       Plant_rank, Plant_status, Plant_matchtype, Plant_order, Plant_family, Plant_genus,
       Plant_unsure_id, Plant_uncertainty_type, Pollinator_old_name, Pollinator_accepted_name, 
       Pollinator_rank, Pollinator_status, Pollinator_matchtype, Pollinator_order, Pollinator_family, 
       Pollinator_genus, Pollinator_unsure_id, Pollinator_uncertainty_type, Flower_data, Flower_data_merger)

#Keep all cols for now
saveRDS(data2, "Data/3_Final_data/Interactions_uncounted.rds")

#Final version
data3 = data2 %>% 
select(Study_id, Network_id, Sampling_method, Authors_habitat, 
       SafeNet_habitat, Bioregion, Country, Locality,
       Latitude, Longitude, Date, Interaction, Plant_old_name, Plant_accepted_name,
       Plant_rank, Plant_order, Plant_family, Plant_genus,
       Plant_unsure_id, Plant_uncertainty_type, Pollinator_old_name, Pollinator_accepted_name, 
       Pollinator_rank, Pollinator_order, Pollinator_family, 
       Pollinator_genus, Pollinator_unsure_id, Pollinator_uncertainty_type, Flower_data, Flower_data_merger)
saveRDS(data3, "Data/3_Final_data/Interaction_data.rds")



