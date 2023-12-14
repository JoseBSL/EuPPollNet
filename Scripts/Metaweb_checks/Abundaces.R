#Metaweb taxa abundances per taxonomic rank
#For both plant and pollinator species

#Workflow:
#1)Read master data with accepted taxonomic data
#2)Identify 20 most common species overall and plot
#3)Identify 20 most common species across studies----


#Load libraris
library(dplyr)
library(ggplot2)

#1)Read data----
data = readRDS("Data/Interactions_uncounted.rds")
#Check colnames
colnames(data)

###############################################
#2)Identify 20 most common species overall and plot----
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
angle = 55, vjust = 1, hjust=1, size = 7)) +
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
angle = 55, vjust = 1, hjust=1, size = 7)) +
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
angle = 55, vjust = 1, hjust=1, size = 5)) +
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
angle = 55, vjust = 1, hjust=1, size = 7)) +
xlab(NULL)

###############################################
#3)Identify 20 most common species across studies----
###############################################

#----------------#
#Pollinators
#----------------#
#First select cols of rank, accepted name and study ID
#Then select unique levels by study ID
#Now sum levels (maximum can be as maximum number of studies)
poll_spread = data %>% 
select(Pollinator_rank, Pollinator_accepted_name, Study_id) %>% 
group_by(Study_id) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
distinct(Pollinator_accepted_name) %>% 
ungroup() %>% 
count(Pollinator_accepted_name) %>% 
rename(n_studies_shared = n) %>% 
arrange(-n_studies_shared) %>% 
mutate(Percent_total = n_studies_shared / n_distinct(data$Study_id))
  
#Seelect quantile 50% from the total number of species
spp_number = poll_spread %>%  
select(Pollinator_accepted_name) %>% 
n_distinct()
df = seq(1, spp_number)
quantile_50 = quantile(df, 0.5)
#Plot
ggplot(poll_spread, 
aes(reorder(Pollinator_accepted_name,-Percent_total), Percent_total)) +
geom_bar(stat="identity") +
ylab("Shared pollinators (%)") +
theme(axis.text.x = element_blank(),
axis.ticks=element_blank()) +
xlab(NULL) +
geom_vline(xintercept = quantile_50, linetype="dashed", 
color = "black", size=0.75)

#----------------#
#Plants
#----------------#
plant_spread = data %>% 
select(Plant_rank, Plant_accepted_name, Study_id) %>% 
group_by(Study_id) %>% 
filter(Plant_rank == "SPECIES") %>% 
distinct(Plant_accepted_name) %>% 
ungroup() %>% 
count(Plant_accepted_name) %>% 
rename(n_studies_shared = n) %>% 
arrange(-n_studies_shared) %>% 
mutate(Percent_total = n_studies_shared / n_distinct(data$Study_id))
  
#Seelect quantile 50% from the total number of species
spp_number = plant_spread %>%  
select(Plant_accepted_name) %>% 
n_distinct()
df = seq(1, spp_number)
quantile_50 = quantile(df, 0.5)
#Plot
ggplot(plant_spread, 
aes(reorder(Plant_accepted_name,-Percent_total), Percent_total, fill = Plant_accepted_name)) +
geom_bar(stat = "identity", 
           color = "black",
           lwd = 0.025, fill = "#009E73") +
ylab("Shared plants (%)") +
theme(axis.text.x = element_blank(),
axis.ticks=element_blank()) +
xlab(NULL) +
geom_vline(xintercept = quantile_50, linetype="dashed", 
                color = "black", size=0.25)

