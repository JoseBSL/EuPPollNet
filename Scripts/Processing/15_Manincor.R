#DATASET NUMBER 17; Manincor

#Load libraries
library(tidyverse)

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/Raw_data/14_Manincor/interaction_data2_manincor.csv", col_names = T)

#Convert coordinates in degree mins and seconds to lat long in decimal degrees
data = data %>%
  mutate(Longitude = parzer::parse_lon(Longitude),
         Latitude = parzer::parse_lat(Latitude))

unique(factor(data$Latitude))
unique(factor(data$Longitude))

#Split data into different dataframes based on site id
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ----
flower_count = read_csv("Data/Raw_data/14_Manincor/flower_count_data2_manincor.csv", col_names = T)

#Delete extra column that indicated that this dataset was number one
flower_count = flower_count %>% 
  select(!c(Comment...8, Comment...9))

#Split data into different dataframes based on survey name
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
Metadata <- tibble(
  Doi = "10.1016/j.actao.2020.103551",
  Dataset_description = "These datasets document 3 bee-plant networks observed in 3
  calcareaous grasslands in France during the month of July 2016.
  The current data only present the observed interactions (visit-based network
  in the mentioned paper published in Acta Oecologica). These data are part of the
  ANR ARSENIC project (grant no. 14-CE02-0012).",
  Taxa_recorded = "Only female bee visitors have been used for these datasets")


#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Natasha de Manincor", "Nina Hautekèete", "Clément Mazoyer",
                    "Paul Moreau", "Yves Piquot", "Bertrand Schatz", "Eric Schmitt",
                    "Marie Zélazny", "François Massol"),
  Orcid = c("0000-0001-9696-125X", "0000-0002-6071-5601", "", "", "0000-0001-9977-8936",
            "0000-0003-0135-8154", "", "", "0000-0002-4098-955X"),
  E_mail = c("natasha.demanincor@gmail.com", "nina.hautekeete@univ-lille.fr", 
             "clement.mazoyer@univ-lille.fr", "p35.moreau@gmail.com",
             "yves.piquot@univ-lille.fr", "bertrand.schatz@cefe.cnrs.fr",
             "eric.schmitt@univ-lille.fr", "marie.zelazny@hotmail.fr",
             "francois.massol@univ-lille.fr"))

#Save data ----
#Create list with all dataframes of interest
Manicor <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Manicor) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Manicor, file="Data/Clean_data/17_Manicor.rds")

