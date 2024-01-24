
library(dplyr)
library(ggplot2)
library(readr)
#Read data
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")


sum(pollinator_spp_interactions$Interaction_percent)

#Create workflow to calculate % of visits by all pollinators
pollinator_spp_interactions = data %>% 
select(Pollinator_rank, Pollinator_accepted_name, Interaction) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
select(Pollinator_accepted_name, Interaction) %>% 
group_by(Pollinator_accepted_name) %>% 
summarise(Interaction_percent = as.integer(sum(Interaction))/nrow(.) * 100) %>% 
filter(Pollinator_accepted_name!= "Apis mellifera") %>% 
ungroup() %>% 
arrange(-Interaction_percent) %>% 
mutate(Cumulative_percentage = cumsum(100*Interaction_percent/sum(Interaction_percent)))

#Save data
write_csv(pollinator_spp_interactions, "Data/Working_files/pollinator_percentages.csv")

#Graph
ggplot(pollinator_spp_interactions, 
aes(reorder(Pollinator_accepted_name,Cumulative_percentage), Cumulative_percentage)) +
geom_col()+
theme(axis.text.x = element_text(
angle = 55, vjust = 1, hjust=1, size = 7)) +
xlab(NULL)


#Create workflow to calculate % of visits by all pollinators
plant_spp_interactions = data %>% 
select(Plant_rank, Plant_accepted_name, Interaction) %>% 
filter(Plant_rank == "SPECIES") %>% 
select(Plant_accepted_name, Interaction) %>% 
group_by(Plant_accepted_name) %>% 
summarise(Interaction_percent = as.integer(sum(Interaction))/nrow(.) * 100) %>% 
filter(Plant_accepted_name!= "Helianthus annuus") %>% 
ungroup() %>% 
arrange(-Interaction_percent) %>% 
mutate(Cumulative_percentage = cumsum(100*Interaction_percent/sum(Interaction_percent)))# %>%

#Save data
write_csv(plant_spp_interactions, "Data/Working_files/plant_percentages.csv")
