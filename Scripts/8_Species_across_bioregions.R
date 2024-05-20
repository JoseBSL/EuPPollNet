
#Check network size across bioregions

#Load libraries
library(dplyr)
library(ggplot2)

#Load data 
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")
data = data %>%
dplyr::mutate(Year = lubridate::year(Date), 
                Month = lubridate::month(Date), 
                Day = lubridate::day(Date))


# Filter out Karise study and create an id column
data = data %>% filter(!Study_id == "13_Karise") %>% 
mutate(Study_network_id = paste0(Study_id,"_", Network_id))


#Calculate poll species by study
poll_species = data %>% 
select(Study_network_id, Pollinator_rank, Pollinator_accepted_name, Bioregion, Year) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
group_by(Study_network_id,Bioregion, Year) %>% 
summarise(Poll_species = n_distinct(Pollinator_accepted_name))
#Calculate plant species by study
plant_species = data %>% 
select(Study_network_id, Plant_rank, Plant_accepted_name, Bioregion, Year) %>% 
filter(Plant_rank == "SPECIES") %>% 
group_by(Study_network_id,Bioregion, Year) %>% 
summarise(Plant_species = n_distinct(Plant_accepted_name))
#Join both
species_by_network = left_join(poll_species, plant_species)
#Calculate network size by study
species_by_network = species_by_network %>% 
mutate(Network_size = Poll_species * Plant_species) %>% 
mutate(Spp_mean = sqrt(Poll_species * Plant_species))
#Evaluate size of networks across bioregions
species_by_network %>% 
ggplot(aes(Bioregion, Network_size)) +
geom_boxplot() +
scale_y_log10()
#Correct by number of sampling dates per network
sampling_dates = data %>% 
select(Study_network_id, Bioregion, Year, Date) %>% 
group_by(Study_network_id,Bioregion, Year) %>% 
summarise(N_dates = n_distinct(Date))
#Add to dataset
species_by_network_date = left_join(species_by_network,sampling_dates)
#Correct metrics by number of sampling dates
species_by_network_date %>% 
mutate(Spp_mean_date = Spp_mean/N_dates) %>% 
mutate(Network_size_date = Network_size/N_dates) %>% 
ggplot(aes(Bioregion, Network_size_date)) +
geom_boxplot() +
scale_y_log10()


