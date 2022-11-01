#DATASET NUMBER 9; Heleno

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data = read.csv("Data/Raw_data/7_Heleno/interaction_data_heleno.csv")

InteractionData = data %>% 
mutate(Coordinate_precision = str_replace(Coordinate_precision, " ", ""))
#Split by site, just for createing the listed name in this case
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
FlowerCount = read.csv("Data/Raw_data/7_Heleno/flower_count_heleno.csv")
#Common excel mistake on id, fix
FlowerCount$Site_id <- "Coimbra_2017"
#Split by site, just for createing the listed name in this case
FlowerCount <- split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----
Metadata <- tibble(
  Doi = "https://doi.org/10.1007/s00442-018-4281-5",
  Dataset_description = "This dataset documents flower visits in winter and
  spring in the Botanical Garden of the University of Coimbra.
  Interactions have been recorded weekly by performing plant-centered
  direct observations between 28 October 2016 and 19 May 2017
  (thus including the main flowering season). Observation time was proportional
  to flower abundance and to the duration of its flowering period, with a minimum
  of two 15 minutes of observations per species per week, and a total observation
  time of 255 h. The area includes many exotic and cultivated plants from around the world.
  Each week.",
  Taxa_recorded = "All flower visitors")

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Ruben Heleno", "Francisco López-Núñez", "Catherine O’Connor"),
  Orcid = c("0000-0003-2297-006X", "0000-0003-0773-9134", NA),
  E_mail = c("rheleno@uc.pt", "lnfran85@gmail.com", NA))

#Save data ----
#Create list with all dataframes of interest
Heleno <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Heleno) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 9
saveRDS(Heleno, file="Data/Clean_data/9_Heleno.rds") 
