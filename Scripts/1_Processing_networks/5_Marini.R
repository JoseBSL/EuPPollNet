#DATASET NUMBER 5; Cappellari & Marini

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read.csv("Data/Raw_data/4_5_6_Marini/Interaction_data.csv")

#Delete all underscores and space in one column
data = data %>% 
mutate(across(everything(), function(x) str_replace_all(x,"_", " "))) %>% 
mutate(Coordinate_precision = str_replace(Coordinate_precision, " ", "")) %>% 
mutate(Comments = Survey) %>%   
select(!c(Survey, Sampling_effort_minutes, Sampling_area_square_meters)) %>% 
mutate(Latitude = as.numeric(Latitude)) %>% 
mutate(Longitude = as.numeric(Longitude)) 

#Unify level
data = data %>% 
mutate(Sampling_method = "Focal_observation")

#Unify structure of data
data = change_str(data)

#Split data into different dataframes based on survey name
split_intdata <- split(data, data$Comments)
#Convert to tibbles
data2 <- as_tibble(split_intdata[[2]])
#Now create a list of the different networks (unique Site_id) for each survey
InteractionData2 <- split(data2, data2$Site_id)

#Prepare flower count data ----
flower_count = read.csv("Data/Raw_data/4_5_6_Marini/Flower_count.csv")

#Delete all underscores
FlowerCount = flower_count %>% 
mutate(across(everything(), function(x) str_replace_all(x, "_", " "))) %>% 
select(!c(Total_flower_cover, Collected_insects))

#Split data into different dataframes based on survey name
split_flwdata <- split(FlowerCount, FlowerCount$Survey)
#Convert to tibbles
FlowerCount2 <- as_tibble(split_flwdata[[2]]) %>%  select(!Survey)
#Split by Site_id
FlowerCount2 = split(FlowerCount2, FlowerCount2$Site_id)

#Prepare metadata data ----

#Plants sampled in each site
for (i in InteractionData2) {
#Generate sum of distinct plants per site
plant_sum2 <- bind_rows(lapply(InteractionData2, function(x) x %>% 
                      select(Plant_species, Site_id) %>% 
                      group_by(Site_id) %>% 
                      summarise(Sum = n_distinct(Plant_species))))

}
#Plant sum should be the total number of plants sampled
plant_sum2 = sum(plant_sum2$Sum)


#Store unique cases of plants and polls
plant_single_cases2 = data2 %>% distinct(Plant_species)
pollinator_single_cases2 = data2 %>%distinct(Pollinator_species)

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
Taxa_recorded = "Apoidea, hoverflies, tachinid flies, butterflies",
Sampling_year = "2019",
Country = "Italy",
Habitat = "Grassland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = 1,
Sampling_method = "Focal observations",
Sampling_area_details = NA,
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA ,
Sampling_time_details = "15 mins / species and round",
Sampling_time_species_round_min = 15,
Sampling_time_total_min = 15 * 51 * plant_sum2, 
Total_plant_species = nrow(plant_single_cases2),
Total_pollinator_species = nrow(pollinator_single_cases2),
Floral_counts =  "Yes")

#Transpose metadata
Metadata2 = as.data.frame(t(Metadata2)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

Authorship2 <- data.frame(
  Coauthor_name = c("Andree Cappellari", "Lorenzo Marini"),
  Orcid = c("0000-0002-6726-1323", "0000-0001-7429-7685"),
  E_mail = c("andree.cappellari@phd.unipd.it", "lorenzo.marini@unipd.it"))

#Save data ----
#Create list with all dataframes of interest
Marini2 <- list(InteractionData2, FlowerCount2, Metadata2, Authorship2)
#Rename list elements
names(Marini2) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Marini2, file="Data/Clean_data/5_Marini.rds")

