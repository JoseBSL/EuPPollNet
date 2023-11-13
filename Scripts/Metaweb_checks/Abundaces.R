#Metaweb taxa abundances per taxonomic rank
#For both plant and pollinator species

#Workflow:
#1)Read master data with accepted taxonomic data
#2)Check abundances of spp, genera, family and order 
#3)Unify and plot

#Load libraris
library(dplyr)
library(ggplot2)

#1)READ DATA
data = readRDS("Data/Interactions_accepted_names.rds")
#Check colnames
colnames(data)
###############################################
#2)NUMBER OF SPECIES, GENERA, FAMILIES AND ORDERS
###############################################
##################
#Polinator species
##################
pollinator_spp_abundance = data %>% 
select(Pollinator_rank, Pollinator_accepted_name) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
select(Pollinator_accepted_name) %>% 
count(Pollinator_accepted_name) %>% 
arrange(-n) %>% 
slice(1:20)

#Top 20 pollinator abundances
ggplot(pollinator_spp_abundance, 
aes(reorder(Pollinator_accepted_name,-n), n)) +
geom_bar(stat="identity") +
ylab("Observations") +
theme(axis.text.x = element_text(
angle = 55, vjust = 1, hjust=1, size = 3)) +
xlab(NULL)

#Top 20 pollinators with higher interactions
pollinator_spp_interactions = data %>% 
select(Pollinator_rank, Pollinator_accepted_name, Interaction) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
select(Pollinator_accepted_name, Interaction) %>% 
group_by(Pollinator_accepted_name) %>% 
summarise(Interaction = as.integer(sum(Interaction))) %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
ungroup() %>% 
arrange(-Interaction) %>% 
slice(1:20)

#Top 20 pollinators with higher interactions
ggplot(pollinator_spp_interactions, 
aes(reorder(Pollinator_accepted_name,-Interaction), Interaction)) +
geom_bar(stat="identity") +
ylab("Interactions") +
theme(axis.text.x = element_text(
angle = 55, vjust = 1, hjust=1, size = 5)) +
xlab(NULL)


##################
#Plant species
##################
plant_spp_abundance = data %>% 
select(Plant_rank, Plant_accepted_name) %>% 
filter(Plant_rank == "SPECIES") %>% 
select(Plant_accepted_name) %>% 
count(Plant_accepted_name) %>% 
arrange(-n) %>% 
slice(1:20)


#Top 20 plants
ggplot(plant_spp_abundance, 
aes(reorder(Plant_accepted_name,-n), n)) +
geom_bar(stat="identity") +
ylab("Observations") +
theme(axis.text.x = element_text(
angle = 55, vjust = 1, hjust=1, size = 3)) +
xlab(NULL)

#Top 20 plants with higher interactions
plant_spp_interactions = data %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
select(Plant_rank, Plant_accepted_name, Interaction) %>% 
filter(Plant_rank == "SPECIES") %>% 
select(Plant_accepted_name, Interaction) %>% 
group_by(Plant_accepted_name) %>% 
summarise(Interaction = as.integer(sum(Interaction))) %>% 
ungroup() %>% 
arrange(-Interaction) %>% 
slice(1:20)

#Top 20 plants with higher interactions
ggplot(plant_spp_interactions, 
aes(reorder(Plant_accepted_name,-Interaction), Interaction)) +
geom_bar(stat="identity") +
ylab("Interactions") +
theme(axis.text.x = element_text(
angle = 55, vjust = 1, hjust=1, size = 5)) +
xlab(NULL)

