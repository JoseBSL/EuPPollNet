
#Load libraries
library(dplyr)
library(iNEXT)

#Load data
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")

#Pollinators
#Select columns of interest
poll_data = data %>% 
mutate(Network_id = paste0(Study_id, Network_id)) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
select(Network_id, Pollinator_accepted_name) %>% 
distinct() 

#Count occurrences of each species in each study
poll_result = poll_data %>%
count(Network_id, Pollinator_accepted_name) %>% 
group_by(Pollinator_accepted_name) %>% 
summarise(Incidence = sum(n))

#Generate incidence matrix with number of sampling units at the begining
poll = matrix(c(length(unique(data$Network_id)) , 
          poll_result %>% pull(Incidence)),  
          ncol = 1)
row.names(poll) = c("Plot", poll_result %>% pull(Pollinator_accepted_name))
poll = data.frame(poll)
colnames(poll) = "Network_id"

#Calculate sampling coverage
poll_output = iNEXT(poll, datatype = 'incidence_freq')
#Save output
saveRDS(poll_output, "Data/Working_files/pollinator_sampling_coverage.rds")
#Plot
ggiNEXT(poll_output, type=1) 


#Plants
#Select columns of interest
plant_data = data %>% 
mutate(Network_id = paste0(Study_id, Network_id)) %>% 
filter(Plant_rank == "SPECIES") %>% 
select(Network_id, Plant_accepted_name) %>% 
distinct() 

#Count occurrences of each species in each study
plant_result = plant_data %>%
count(Network_id, Plant_accepted_name) %>% 
group_by(Plant_accepted_name) %>% 
summarise(Incidence = sum(n))

#Generate incidence matrix with number of sampling units at the begining
plant = matrix(c(length(unique(data$Network_id)) , 
          plant_result %>% pull(Incidence)),  
          ncol = 1)
row.names(plant) = c("Plot", plant_result %>% pull(Plant_accepted_name))
plant = data.frame(plant)
colnames(plant) = "Network_id"
#Calculate sampling coverage
plant_output = iNEXT(plant, datatype = 'incidence_freq')
#Save output
saveRDS(plant_output, "Data/Working_files/plant_sampling_coverage.rds")
#Plot
ggiNEXT(plant_output, type=1) 


#Interactions
#Select columns of interest
int_data = data %>% 
mutate(Network_id = paste0(Study_id, Network_id)) %>% 
filter(Plant_rank == "SPECIES") %>% 
filter(Pollinator_rank == "SPECIES") %>% 
mutate(Interaction_id = paste0(Plant_accepted_name, "_", Pollinator_accepted_name)) %>% 
select(Network_id, Interaction_id) %>% 
distinct() 

#Count occurrences of each species in each study
int_result = int_data %>%
count(Network_id, Interaction_id) %>% 
group_by(Interaction_id) %>% 
summarise(Incidence = sum(n))

#Generate incidence matrix with number of sampling units at the begining
int = matrix(c(length(unique(int_data$Network_id)) , 
          int_result %>% pull(Incidence)),  
          ncol = 1)
row.names(int) = c("Plot", int_result %>% pull(Interaction_id))
int = data.frame(int)
colnames(int) = "Network_id"
#Calculate sampling coverage
int_output = iNEXT(int, datatype = 'incidence_freq')
#Save output
saveRDS(int_output, "Data/Working_files/interactions_sampling_coverage.rds")
#Plot
ggiNEXT(int_output, type=1) 
