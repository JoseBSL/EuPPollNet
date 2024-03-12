#DATASET NUMBER 4; Cappellari & Marini
#It has 3 datasets that are prepared in 3 different scripts 
#Those are: 4_Marini, 5_Marini, 6_Marini

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read.csv("Data/1_Raw_data/4_5_6_Marini/Interaction_data.csv")

#Delete all underscores and space in one column
data = data %>% 
mutate(across(everything(), function(x) str_replace_all(x,"_", " "))) %>% 
mutate(Coordinate_precision = str_replace(Coordinate_precision, " ", "")) %>% 
mutate(Comments = Survey) %>%   
select(!c(Survey, Sampling_effort_minutes, Sampling_area_square_meters)) %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Flower_data_merger = NA) 

#Create column to merge floral counts
data = data %>%  
mutate(Flower_data_merger = paste0(word(Plant_species,1),word(Plant_species,2), 
                                   Site_id, Day, "-", Month, "-", Year)) 

#Unify level
data = data %>% 
mutate(Sampling_method = "Focal_observation")

#Unify structure of data
data = change_str(data)

#Split data into different dataframes based on survey name
split_intdata <- split(data, data$Comments)
#Convert to tibbles
data1 <- as_tibble(split_intdata[[1]])

#Now create a list of the different networks (unique Site_id) for each survey
InteractionData1 <- split(data1, data1$Site_id)

#Prepare flower count data ----
flower_count = read.csv("Data/1_Raw_data/4_5_6_Marini/Flower_count.csv")

#Set common colname
flower_count = flower_count %>% 
rename(Comments = Comment)

#Delete all underscores
FlowerCount = flower_count %>% 
  mutate(across(everything(), function(x) str_replace_all(x, "_", " "))) %>% 
  select(!c(Total_flower_cover, Collected_insects)) %>% 
  mutate(Flower_data_merger = NA)

#When merging with interaction data there are few many to many relationships
#Just 13, small hack to just keep 1
FlowerCount = FlowerCount %>%
group_by_at(vars(-Flower_count)) %>%
mutate(row_number = row_number()) %>%
distinct() %>%
filter(row_number == 1) %>%
select(-row_number)

#Create column to merge floral counts
FlowerCount = FlowerCount %>%  
mutate(Flower_data_merger = paste0(word(Plant_species,1),word(Plant_species,2), 
                                   Site_id, Day, "-", Month, "-", Year)) 


#Split data into different dataframes based on survey name
split_flwdata <- split(FlowerCount, FlowerCount$Survey)
#Convert to tibbles
FlowerCount1 <- as_tibble(split_flwdata[[1]]) %>%  select(!Survey)

#Unify data structure
FlowerCount1 = change_str2(FlowerCount1)

#Split by Site_id
FlowerCount1 = split(FlowerCount1, FlowerCount1$Site_id)

#Prepare metadata data ----

#Plants sampled in each site
for (i in InteractionData1) {
  #Generate sum of distinct plants per site
  plant_sum1 <- bind_rows(lapply(InteractionData1, function(x) x %>% 
                                   select(Plant_species, Site_id) %>% 
                                   group_by(Site_id) %>% 
                                   summarise(Sum = n_distinct(Plant_species))))
  
}
#Plant sum should be the total number of plants sampled
plant_sum1 = sum(plant_sum1$Sum)

#Store unique cases of plants and polls
plant_single_cases1 = data1 %>% distinct(Plant_species)
pollinator_single_cases1 = data1 %>%distinct(Pollinator_species)

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
  Taxa_recorded = "Apoidea, hoverflies, conopids, tachinid flies, butterflies",
  Sampling_year = 2018,
  Country = "Italy",
  Habitat = "Open riparian habitat",
  Sampling_sites = nlevels(factor(data$Site_id)),
  Sampling_rounds = 5,
  Sampling_method = "Focal observations",
  Sampling_area_details = NA,
  Sampling_area_species_m2 = NA,
  Sampling_area_total_m2 = NA ,
  Sampling_time_details = "5 mins / species and round",
  Sampling_time_species_round_min = 5, 
  Sampling_time_total_min = 5 * 18 * 5 * plant_sum1, #5mins * 18 sites * 5 rounds * Number of species per site
  Total_plant_species = nrow(plant_single_cases1),
  Total_pollinator_species = nrow(pollinator_single_cases1),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata1 = as.data.frame(t(Metadata1)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()


#Prepare authorship data ----
Authorship1 <- data.frame(
  Coauthor_name = c("Andree Cappellari", "Lorenzo Marini"),
  Orcid = c("0000-0002-6726-1323", "0000-0001-7429-7685"),
  E_mail = c("andree.cappellari@phd.unipd.it", "lorenzo.marini@unipd.it"))

#Save data ----
#Create list with all dataframes of interest
Marini1 <- list(InteractionData1, FlowerCount1, Metadata1, Authorship1)

#Rename list elements
names(Marini1) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Marini1, file="Data/2_Processed_data/4_Marini.rds")


