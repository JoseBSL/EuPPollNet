#DATASETS NUMBER 4; Cappellari & Marini
#DATASETS NUMBER 5; Cappellari & Marini
#DATASETS NUMBER 6; Cappellari, Gazzea & Marini

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data = read.csv("Data/Raw_data/Marini/interaction_data_marini.csv")

#Delete all underscores and space in one column
data %>% 
mutate(across(everything(), function(x) str_replace_all(x,"_", " "))) %>% 
mutate(Coordinate_precision = str_replace(Coordinate_precision, " ", ""))  

#Split data into different dataframes based on survey name
split_intdata <- split(data, data$Survey)
#Convert to tibbles
InteractionData1 <- as_tibble(split_intdata[[1]])
InteractionData2 <- as_tibble(split_intdata[[2]])
InteractionData3 <- as_tibble(split_intdata[[3]])
#Now create a list of the different networks (unique Site_id) for each survey
InteractionData1 <- split(InteractionData1, InteractionData1$Site_id)
InteractionData2 <- split(InteractionData2, InteractionData2$Site_id)
InteractionData3 <- split(InteractionData3, InteractionData3$Site_id)

#Prepare flower count data ----
flower_count = read.csv("Data/Raw_data/Marini/flower_count_marini.csv")

#Delete all underscores
FlowerCount = flower_count %>% 
mutate(across(everything(), function(x) str_replace_all(x, "_", " "))) %>% 
select(!c(Total_flower_cover, Collected_insects))

#Split data into different dataframes based on survey name
split_flwdata <- split(FlowerCount, FlowerCount$Survey)
#Convert to tibbles
FlowerCount1 <- as_tibble(split_flwdata[[1]])
FlowerCount2 <- as_tibble(split_flwdata[[2]])
FlowerCount3 <- as_tibble(split_flwdata[[3]])

#Prepare metadata data ----
Metadata1 <- tibble(
  Doi = "https://doi.org/10.1111/ddi.13132",
  Dataset_description = "We selected 18 sites (9 pairs) in Northern Italy
  across the whole elevational range distribution of Buddleja davidii (100-1200 m a.s.l.),
  our target exotic plant species. For each pair, we chose one site invaded and one
  non-invaded by B. davidii. Sites were open riparian habitats located along valley bottoms.
  Within each pair, the surrounding landscape and plant community composition were similar.
  Sites were visited five times between June and August 2018. At each visit, each flowering
  species was sampled for 5 minutes, so the time spent at each site depended on the number
  of flowering plant species. The Interaction column in the Interaction_data sheet indicates
  the number of specimens observed on each plant species. The Flower_count column in the
  Flower_availability sheet indicates the percentage cover of the species in the total
  site area.",
  Taxa_recorded = "Apoidea, hoverflies, conopids, tachinid flies, butterflies")

Metadata2 <- tibble(
  Doi = "https://doi.org/10.1007/s00442-022-05151-6",
  Dataset_description = "We selected 51 grasslands in Norther Italy, with elevation
  ranging from 150 to 2100 m a.s.l. Each site was visited only once between May and
  August 2019. At each site, each flowering plant species was sampled for 15 minutes,
  so the time spent at each site depended on the number of flowering plant species.
  Air temperature was measured using a Tinytag Plus 2 TGP-4017 data logger.
  The Interaction column in the Interaction_data sheet indicates the number of specimens
  observed on each plant species. The Flower_count column in the Flower_availability sheet
  indicates the percentage cover of the species out of 100%, while the Total_flower_cover
  column indicates the total percentage cover of all flowering plant species at each site.",
  Taxa_recorded = "Apoidea, hoverflies, tachinid flies, butterflies")

Metadata3 <- tibble(
  Doi = NA,
  Dataset_description = "We selected three intensively managed landscapes dominated by
  crops. All landscapes comprised at least one sunflower field. In order to sample
  pollinators at the landscape scale, we placed a grid of 600 m x 600 m on each landscape.
  Each grid was then divided into 36 cells of 100 m x 100 m each, which constituted our
  sampling sites. At each site, each plant species was sampled for 40 minutes, so the
  time spent at each site depended on the number of flowering plant species.
  We sampled plant–pollinator interactions between June and August 2020,
  repeating the survey four times at each site. The Interaction column in the
  Interaction_data sheet indicates the number of specimens observed on each plant species.
  The Flower_count column in the Flower_availability sheet indicates the percentage
  cover of the species out of 100%, while the Total_flower_cover column indicates
  the total percentage cover of all flowering plant species at each site.",
  Taxa_recorded = "Bees, hoverflies")

#Prepare authorship data ----
Authorship1 <- data.frame(
  Coauthor_name = c("Andree Cappellari", "Lorenzo Marini"),
  Orcid = c("0000-0002-6726-1323", "0000-0001-7429-7685"),
  E_mail = c("andree.cappellari@phd.unipd.it", "lorenzo.marini@unipd.it"))

Authorship2 <- data.frame(
  Coauthor_name = c("Andree Cappellari", "Lorenzo Marini"),
  Orcid = c("0000-0002-6726-1323", "0000-0001-7429-7685"),
  E_mail = c("andree.cappellari@phd.unipd.it", "lorenzo.marini@unipd.it"))

Authorship3 <- data.frame(
  Coauthor_name = c("Andree Cappellari", "Elena Gazzea", "Lorenzo Marini"),
  Orcid = c("0000-0002-6726-1323", "0000-0002-3253-2146", "0000-0001-7429-7685"),
  E_mail = c("andree.cappellari@phd.unipd.it", "elena.gazzea@phd.unipd.it", "lorenzo.marini@unipd.it"))


#Save data ----
#Create flower_count list
FlowerCount1 <- list(FlowerCount1) 
FlowerCount2 <- list(FlowerCount2) 
FlowerCount3 <- list(FlowerCount3)
#Create metadata list
Metadata1 <- list(Metadata1) 
Metadata2 <- list(Metadata2) 
Metadata3<- list(Metadata3) 
#Create author list
Authorship1 <- list(Authorship1) 
Authorship2 <- list(Authorship2) 
Authorship3 <- list(Authorship3) 
#Create list with all dataframes of interest
Marini1 <- list(InteractionData1, FlowerCount1, Metadata1, Authorship1)
Marini2 <- list(InteractionData2, FlowerCount2, Metadata2, Authorship2)
Marini3 <- list(InteractionData3, FlowerCount3, Metadata3, Authorship3)
#Rename list elements
names(Marini1) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
names(Marini2) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
names(Marini3) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Marini1, file="Data/Clean_data/4_Marini.RData")
saveRDS(Marini2, file="Data/Clean_data/5_Marini.RData")
saveRDS(Marini3, file="Data/Clean_data/6_Marini.RData")

