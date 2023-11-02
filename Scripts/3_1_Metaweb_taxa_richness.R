#Metaweb checks of richness per taxonomic rank
#For both plant and pollinator species

#Workflow:
#1)Read master data with accepted taxonomic data
#2)Check number of spp, genera, family and order 
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
#Check unique number of pollinator species
pollinator_spp_number = data %>% 
select(Pollinator_rank, Pollinator_accepted_name) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
select(Pollinator_accepted_name) %>% 
n_distinct()
pollinator_spp_number #check number
#Make sure that we do not miss any species
poll_check = data %>% 
select(Pollinator_rank, Pollinator_accepted_name) %>% 
filter(Pollinator_rank != "SPECIES") %>% 
select(Pollinator_accepted_name)  %>% 
distinct() #seems that we havent miss anything
#Check unique number of pollinator genera
pollinator_genus_number = data %>% 
select(Pollinator_genus) %>% 
n_distinct()
pollinator_genus_number
#Check unique number of pollinator families
pollinator_families_number = data %>% 
select(Pollinator_family) %>% 
n_distinct()
pollinator_families_number
#Check unique number of pollinator orders
pollinator_order_number = data %>% 
select(Pollinator_order) %>% 
n_distinct()
pollinator_order_number
##################
#Plant species
##################
#Check unique number of plant species
plant_spp_number = data %>% 
select(Plant_rank, Plant_status, 
Plant_matchtype, Plant_accepted_name) %>% 
filter(Plant_rank == "SPECIES") %>% 
select(Plant_accepted_name) %>% 
n_distinct()
plant_spp_number #check number
#Make sure that we do not miss any species
poll_check = data %>% 
select(Pollinator_rank, Pollinator_status, 
Pollinator_matchtype,Pollinator_accepted_name) %>% 
filter(Pollinator_rank != "SPECIES") %>% 
select(Pollinator_accepted_name)  %>% 
distinct()
#Check unique number of plant genera
plant_genus_number = data %>% 
select(Plant_genus) %>% 
n_distinct()
plant_genus_number
#Check unique number of plany families
plant_families_number = data %>% 
select(Plant_family) %>% 
n_distinct()
plant_families_number
#Check unique number of plant orders
plant_order_number = data %>% 
select(Plant_order) %>% 
n_distinct()
plant_order_number
##################
#3) UNIFY AND PLOT
##################
numbers = c(pollinator_spp_number, plant_spp_number,
            pollinator_genus_number, plant_genus_number,
            pollinator_families_number, plant_families_number,
            pollinator_order_number, plant_order_number)
labels = c("Pollinators", "Plants",
           "Pollinators", "Plants",
           "Pollinators", "Plants",
           "Pollinators", "Plants")
groups = c("Species", "Species", 
           "Genera", "Genera",
           "Families", "Families",
           "Orders", "Orders")
#Create tibble
d = tibble(numbers, labels, groups)

#Set right order of levels
d$groups <- factor(d$groups, levels = c("Species", "Genera",
            "Families", "Orders"))

#Plot
ggplot(d, aes(labels, numbers, color= groups, label= numbers)) + 
geom_count(size=2) +
theme(axis.text.x = element_text(
angle = 55, vjust = 1, hjust=1)) +
ylab(NULL) +
xlab(NULL) +
labs(color="Taxonomic\nrank") +
theme(legend.title.align = 0.5)+
geom_text(hjust=-0.5, vjust=-0.1)





