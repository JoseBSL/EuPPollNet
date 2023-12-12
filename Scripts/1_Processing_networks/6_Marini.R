#DATASET NUMBER 6; Cappellari, Gazzea & Marini

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

#Unify structure of data
data = change_str(data)

#Split data into different dataframes based on survey name
split_intdata <- split(data, data$Comments)
#Convert to tibbles
data3 <- as_tibble(split_intdata[[3]])
#Now create a list of the different networks (unique Site_id) for each survey
InteractionData3 <- split(data3, data3$Site_id)

#Prepare flower count data ----
flower_count = read.csv("Data/Raw_data/4_5_6_Marini/Flower_count.csv")

#Delete all underscores
FlowerCount = flower_count %>% 
  mutate(across(everything(), function(x) str_replace_all(x, "_", " "))) %>% 
  select(!c(Total_flower_cover, Collected_insects))

#Split data into different dataframes based on survey name
split_flwdata <- split(FlowerCount, FlowerCount$Survey)
#Convert to tibbles
FlowerCount3 <- as_tibble(split_flwdata[[3]]) %>%  select(!Survey)
#Split by Site_id
FlowerCount3 = split(FlowerCount3, FlowerCount3$Site_id)



#Prepare metadata data ----

#Plants sampled in each site
for (i in InteractionData3) {
  #Generate sum of distinct plants per site
  plant_sum3 <- bind_rows(lapply(InteractionData3, function(x) x %>% 
                                   select(Plant_species, Site_id) %>% 
                                   group_by(Site_id) %>% 
                                   summarise(Sum = n_distinct(Plant_species))))
  
}
#Plant sum should be the total number of plants sampled
plant_sum3 = sum(plant_sum3$Sum)
#Store unique cases of plants and polls
plant_single_cases3 = data3 %>% distinct(Plant_species)
pollinator_single_cases3 = data3 %>% distinct(Pollinator_species)

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
  Taxa_recorded = "Bees, hoverflies",
  Sampling_year = 2020,
  Country = "Italy",
  Habitat = "Agricultural",
  Sampling_sites = nlevels(factor(data$Site_id)),
  Sampling_rounds = 4,
  Sampling_method = "Focal observations",
  Sampling_area_details = NA,
  Sampling_area_species_m2 = NA,
  Sampling_area_total_m2 = NA ,
  Sampling_time_details = NA,
  Sampling_time_species_round_min = NA,
  Sampling_time_total_min = NA, 
  Total_plant_species = nrow(plant_single_cases3),
  Total_pollinator_species = nrow(pollinator_single_cases3),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata3 = as.data.frame(t(Metadata3)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship3 <- data.frame(
  Coauthor_name = c("Andree Cappellari", "Elena Gazzea", "Lorenzo Marini"),
  Orcid = c("0000-0002-6726-1323", "0000-0002-3253-2146", "0000-0001-7429-7685"),
  E_mail = c("andree.cappellari@phd.unipd.it", "elena.gazzea@phd.unipd.it", "lorenzo.marini@unipd.it"))

#Save data ----
#Create list with all dataframes of interest
Marini3 <- list(InteractionData3, FlowerCount3, Metadata3, Authorship3)
#Rename list elements
names(Marini3) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Marini3, file="Data/Clean_data/6_Marini.rds")

