#DATASET NUMBER 10; Vanbergen

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data = read.csv("Data/Raw_data/8_Vanbergen/interaction_data_vanbergen.csv")
#Create cols of Temperature and humidity
data = data %>% 
mutate(Temperature = NA) %>% 
mutate(Humidity = NA)
#Split by Site_id
InteractionData <- split(data, data$Site_id)


#Prepare flower count data ----
flower_count = read.csv("Data/Raw_data/8_Vanbergen/flower_count_vanbergen.csv")

#Filter out one column but saved the data
flower_count = flower_count %>% 
mutate(Units = paste0(Units, "; ", Inflorescence.type)) %>% 
select(!Inflorescence.type)
#Split data into different dataframes based on survey name
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
Metadata <- tibble(
  Doi = "NA",
  Dataset_description = "This dataset is part of the VOODOO-project.
  It consists of plant-pollinator networks in 12 different landscapes
  (4 agricultural, 4 rural, 4 urban), sampled over 3 periods in 2021
  (mid april, end may and mid july). Coordinates are the centroids of each landscape.
  Transects were stratified in relation with the area of each type of pollinator habitat
  within a 500 m radius around the centroid. Habitat is given for each individual catch,
  not for the landscape as a whole. Transect length and duration are therefore pooled in
  this file, rather than giving the sampling time in each habitat
  (in total: 120 min per site visit, so 360 min in total, if the networks are pooled
  over the 3 sampling periods). If requested, we can also provide sampling time from
  each subtransect in different habitat types.",
  Taxa_recorded = "All flying insects, except really small stuff (< 1 mm)")

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Adam J. Vanbergen", "Willem Proesmans"),
  Orcid = c("0000-0001-8320-5535", "0000-0003-0358-6732"),
  E_mail = c("adam.vanbergen@inrae.fr", "willem.proesmans@gmail.com"))

#Save data ----
#Create metadata list
Vanbergen <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Vanbergen) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 10
saveRDS(Vanbergen, file="Data/Clean_data/10_Vanbergen.RData") 
