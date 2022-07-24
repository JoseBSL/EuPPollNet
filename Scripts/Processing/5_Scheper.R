#DATASET NUMBER 7; Scheper, Badenhausser & Kleijn

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data = read.csv("Data/Raw_data/5_Scheper/interaction_data_scheper.csv")

#Standardaze poll species
data = data %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Halictus compressus/langobardicus/simplex", "Halictus sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Osmia", "Osmia sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Osmia leaiana/melanogaster", "Osmia sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Nomada (distinguenda)", "Nomada sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Xylocopa", "Xylocopa sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Syrphidae", "Syrphidae sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Megachile", "Megachile sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Halictus", "Halictus sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Episyrphus", "Episyrphus sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Eristalis", "Eristalis sp."))  %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Bombus", "Bombus sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Bombus terr-complex", "Bombus sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Sphaerophoria", "Sphaerophoria sp.")) %>% 
mutate(Pollinator_species = replace(Pollinator_species, 
       Pollinator_species == "Halictus/Lasioglossum", "	Halictidae sp."))  

#Split data into different dataframes based on survey name
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
flower_count = read.csv("Data/Raw_data/5_Scheper/flower_count_scheper.csv")

#Prepare metadata data ----
meta <- tibble(
  Doi = "https://doi.org/10.1111/1365-2664.13658",
  Dataset_description = "Dataset on bees and hoverflies sampled in 25 grassland
  - field boundary pairs in the area around Chize, France, 2015.
  Flowers and plant-pollinator interactions were surveyed two to six times a year
  (for most sites 4 times), in two 150m2 transects (15 min each)
  per habitat on each occasion. Grasslands varied in land use intensity.",
  Taxa_recorded = "Bees and hoverflies")

#Prepare authorship data ----
authors <- data.frame(
  Coauthor_name = c("Jeroen Scheper", "Isabelle Badenhausser", "David Kleijn"),
  Orcid = c("0000-0002-4314-996X", "0000-0002-6919-8647", "0000-0003-2500-7164"),
  E_mail = c("jeroen.scheper@wur.nl", "Isabelle.Badenhausser@inrae.fr", "david.kleijn@wur.nl"))

#Save data ----
#Create metadata list
Metadata <- list(meta) 
Authorship <- list(authors) 
FlowerCount <- list(flower_count) 
#Create list with all dataframes of interest
Scheper <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Scheper) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 7
saveRDS(Scheper, file="Data/Clean_data/7_Scheper.RData") 

