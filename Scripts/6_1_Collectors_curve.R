#Script to obtain collectors curves
#Study and network level pollinator, species and interactions accumulation curves

#Inspiration for this code: https://www.youtube.com/watch?v=ywHVb0Q-qsM&t=656s

#Load libraries
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)

#Read data
data = readRDS("Data/3_Final_data/Interactions.rds")%>% 
mutate(Network_id = paste0(Study_id, Network_id))  

#Select columns of interest
data1_poll = data %>% 
group_by(Network_id) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
select(Study_id,Network_id, Pollinator_accepted_name) %>% 
distinct() 

#Select columns of interest
data1_plant = data %>% 
filter(Plant_rank == "SPECIES") %>% 
select(Study_id,Network_id, Plant_accepted_name) %>% 
distinct() 


#Select columns of interest
data1_int = data %>% 
filter(Pollinator_rank == "SPECIES") %>% 
filter(Plant_rank == "SPECIES") %>% 
mutate(Interaction_id = paste0(Pollinator_accepted_name, "_", Plant_accepted_name)) %>% 
select(Study_id,Network_id, Pollinator_accepted_name, Plant_accepted_name, Interaction_id) %>% 
distinct() 

#Generate function to extract random cumulative sum of species per studies-----
#Data is our dataset with the cols of interest
#Level: Study_id or Network_id
#Taxa: Pollinator_accepted_name/Plant_accepted_name/Interaction_id  
collect = function(data1, level_id, taxa) {
#Filter studies randomly and sum species per study
sampled_studies = sample(unique(data %>% pull({{ level_id }})))

data1 %>%filter({{ level_id }} %in% sampled_studies) %>%
arrange(match({{ level_id }}, sampled_studies)) %>%
group_by({{ taxa }}) %>%
mutate(distinct = row_number() == 1) %>%
ungroup() %>%
group_by({{ level_id }}) %>%
mutate(Unique_spp = sum(distinct)) %>%
ungroup() %>%
select({{ level_id }}, Unique_spp) %>%
distinct() %>%
mutate(Unique_spp_cumulative = cumsum(Unique_spp)) %>%
mutate(Study_sampled = row_number())
}

#Prepare it at study level----
#Calculate N collectors curves for pollinators
collect_curves = map_dfr(1:100, ~ collect(data1_poll, Study_id, Pollinator_accepted_name), .id="iteration") 
#Calculate mean across curves
rarefaction_curve = collect_curves %>% 
group_by(Study_sampled) %>% 
summarise(mean_curve = mean(Unique_spp_cumulative))
#Plot curves
s1 = collect_curves %>% 
ggplot(aes(x = Study_sampled, y= Unique_spp_cumulative, group = iteration)) +
geom_line(color = "gray") + 
geom_line(data = rarefaction_curve, 
          aes(x = Study_sampled, y =  mean_curve), inherit.aes = FALSE) +
ylab("Pollinator species") +
xlab("Studies") +
theme_bw() +
coord_cartesian(expand = FALSE)
s1
#Calculate N collectors curves for plants
collect_curves = map_dfr(1:100, ~ collect(data1_plant, Study_id, Plant_accepted_name), .id="iteration") 
#Calculate mean across curves
rarefaction_curve = collect_curves %>% 
group_by(Study_sampled) %>% 
summarise(mean_curve = mean(Unique_spp_cumulative))
#Plot curves
s2 = collect_curves %>% 
ggplot(aes(x = Study_sampled, y= Unique_spp_cumulative, group = iteration)) +
geom_line(color = "gray") + 
geom_line(data = rarefaction_curve, 
          aes(x = Study_sampled, y =  mean_curve), inherit.aes = FALSE) +
ylab("Plant species") +
xlab("Studies") +
theme_bw() +
coord_cartesian(expand = FALSE)
s2


colnames(data1_int)

#Calculate N collectors curves for interactions
collect_curves = map_dfr(1:100, ~ collect(data1_int, Study_id, Interaction_id), .id="iteration") 
#Calculate mean across curves
rarefaction_curve = collect_curves %>% 
group_by(Study_sampled) %>% 
summarise(mean_curve = mean(Unique_spp_cumulative))
#Plot curves
s3 = collect_curves %>% 
ggplot(aes(x = Study_sampled, y= Unique_spp_cumulative, group = iteration)) +
geom_line(color = "gray") + 
geom_line(data = rarefaction_curve, 
          aes(x = Study_sampled, y =  mean_curve), inherit.aes = FALSE) +
ylab("Interactions") +
xlab("Studies") +
theme_bw() +
coord_cartesian(expand = FALSE)
s3

#Plot panel of plots
s1 + s2 + s3


#Prepare it at network level----
#Calculate N collectors curves for pollinators
collect_curves = map_dfr(1:100, ~ collect(data1_poll, Network_id, Pollinator_accepted_name), .id="iteration")
saveRDS(collect_curves, "Data/Working_files/collectors_curves_pollinators.rds")
#Calculate mean across curves
rarefaction_curve = collect_curves %>% 
group_by(Study_sampled) %>% 
summarise(mean_curve = mean(Unique_spp_cumulative))
#Plot curves
n1 = collect_curves %>% 
ggplot(aes(x = Study_sampled, y= Unique_spp_cumulative, group = iteration)) +
geom_line(color = "gray") + 
geom_line(data = rarefaction_curve, 
          aes(x = Study_sampled, y =  mean_curve), inherit.aes = FALSE) +
ylab("Pollinator species") +
xlab("Sampling sites") +
theme_bw() +
coord_cartesian(expand = FALSE)
n1
#Calculate N collectors curves for plants
collect_curves = map_dfr(1:100, ~ collect(data1_plant, Network_id, Plant_accepted_name), .id="iteration") 
saveRDS(collect_curves, "Data/Working_files/collectors_curves_plants.rds")
#Calculate mean across curves
rarefaction_curve = collect_curves %>% 
group_by(Study_sampled) %>% 
summarise(mean_curve = mean(Unique_spp_cumulative))
#Plot curves
n2 = collect_curves %>% 
ggplot(aes(x = Study_sampled, y= Unique_spp_cumulative, group = iteration)) +
geom_line(color = "gray") + 
geom_line(data = rarefaction_curve, 
          aes(x = Study_sampled, y =  mean_curve), inherit.aes = FALSE) +
ylab("Plant species") +
xlab("Sampling sites") +
theme_bw() +
coord_cartesian(expand = FALSE)
n2
#Calculate N collectors curves for interactions
collect_curves = map_dfr(1:100, ~ collect(data1_int, Network_id, Interaction_id), .id="iteration") 
saveRDS(collect_curves, "Data/Working_files/collectors_curves_interactions.rds")
#Calculate mean across curves
rarefaction_curve = collect_curves %>% 
group_by(Study_sampled) %>% 
summarise(mean_curve = mean(Unique_spp_cumulative))
#Plot curves
n3 = collect_curves %>% 
ggplot(aes(x = Study_sampled, y= Unique_spp_cumulative, group = iteration)) +
geom_line(color = "gray") + 
geom_line(data = rarefaction_curve, 
          aes(x = Study_sampled, y =  mean_curve), inherit.aes = FALSE) +
ylab("Interactions") +
xlab("Sampling sites") +
theme_bw() +
coord_cartesian(expand = FALSE)
n3

#Plot panel of plots
n1 + n2 + n3


#Try now to obtain accumulated pollinatora per plant species----
collect1 = function(data1) {
sampled_plant = sample(unique(data1$Plant_accepted_name))
data1 %>% 
filter(Plant_accepted_name %in% sampled_plant) %>%
arrange(match(Plant_accepted_name, sampled_plant)) %>%
group_by(Pollinator_accepted_name) %>%
mutate(distinct = row_number() == 1) %>%
ungroup() %>%
group_by(Plant_accepted_name) %>%
mutate(Unique_spp = sum(distinct)) %>%
ungroup() %>%
select(Plant_accepted_name, Unique_spp) %>%
distinct() %>%
mutate(Unique_spp_cumulative = cumsum(Unique_spp)) %>%
mutate(Plant_sampled = row_number())
}



#Select columns of interest
data1 = data %>% 
filter(Pollinator_rank == "SPECIES" ) %>% 
filter(Plant_rank == "SPECIES" ) %>% 
select(Pollinator_accepted_name, Plant_accepted_name) %>% 
distinct() 

unique(factor(data1$Plant_accepted_name)) #1373
unique(factor(data1$Pollinator_accepted_name)) #2200


#Calculate N collectors curves for interactions
collect_curves1 = map_dfr(1:100, ~ collect1(data1), .id="iteration") 
saveRDS(collect_curves1, "Data/Working_files/collectors_curves_plant_poll.rds")
#Calculate mean across curves
rarefaction_curve1 = collect_curves1 %>% 
group_by(Plant_sampled) %>% 
summarise(mean_curve = mean(Unique_spp_cumulative))
saveRDS(rarefaction_curve1, "Data/Working_files/rarefaction_curve_plant_poll.rds")

#Plot curves
pp1 = collect_curves1 %>% 
ggplot(aes(x = Plant_sampled, y= Unique_spp_cumulative, group = iteration)) +
geom_line(color = "gray") +
geom_smooth(data = rarefaction_curve1, 
              aes(x = Plant_sampled, y = mean_curve), inherit.aes = FALSE,method = "loess", 
              color = "black", size = 0.5, linetype = "solid", se = FALSE, span = 0.5) +  
ylab("Pollinator species") +
xlab("Plant species") +
theme_bw() +
coord_cartesian(expand = FALSE) 
pp1                                   
