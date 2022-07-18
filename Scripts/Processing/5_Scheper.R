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
split_intdata <- split(data, data$Site_id)

#Prepare flower count data ----
flower_count = read.csv("Data/Raw_data/5_Scheper/flower_count_scheper.csv")


