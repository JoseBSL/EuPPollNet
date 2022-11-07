#DATASET NUMBER 4; Cappellari & Marini

#Load libraries
library(tidyverse)

#Prepare interaction data ----
data = read.csv("Data/Raw_data/4_5_6_Marini/interaction_data_marini.csv")

#Delete all underscores and space in one column
data = data %>% 
  mutate(across(everything(), function(x) str_replace_all(x,"_", " "))) %>% 
  mutate(Coordinate_precision = str_replace(Coordinate_precision, " ", "")) %>% 
  select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) %>% 
  mutate(Latitude = as.numeric(Latitude)) %>% 
  mutate(Longitude = as.numeric(Longitude))

#Split data into different dataframes based on survey name
split_intdata <- split(data, data$Survey)
#Convert to tibbles
data1 <- as_tibble(split_intdata[[1]])

#Now create a list of the different networks (unique Site_id) for each survey
InteractionData1 <- split(data1, data1$Site_id)

#Prepare flower count data ----
flower_count = read.csv("Data/Raw_data/4_5_6_Marini/flower_count_marini.csv")

#Delete all underscores
FlowerCount = flower_count %>% 
  mutate(across(everything(), function(x) str_replace_all(x, "_", " "))) %>% 
  select(!c(Total_flower_cover, Collected_insects))

#Split data into different dataframes based on survey name
split_flwdata <- split(FlowerCount, FlowerCount$Survey)
#Convert to tibbles
FlowerCount1 <- as_tibble(split_flwdata[[1]]) %>%  select(!Survey)

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
  Sampling_sites = 18,
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
  Sampling_sites = 51,
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
  Sampling_sites = 108,
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
#Create list with all dataframes of interest
Marini1 <- list(InteractionData1, FlowerCount1, Metadata1, Authorship1)

#Rename list elements
names(Marini1) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Marini1, file="Data/Clean_data/4_Marini.rds")


