#Script to obtain collectors curves
#Study and network level pollinator, species and interactions accumulation curves

#Inspiration for this code: https://www.youtube.com/watch?v=ywHVb0Q-qsM&t=656s

#Load libraries
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)

#Read data
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")

#Select columns of interest
data1 = data %>% 
mutate(Network_id = paste0(Study_id, Network_id)) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
filter(Plant_rank == "SPECIES") %>% 
mutate(Interaction_id = paste0(Pollinator_accepted_name, "_", Plant_accepted_name)) %>% 
select(Study_id,Network_id, Pollinator_accepted_name, Plant_accepted_name, Interaction_id) %>% 
distinct() 

#Generate function to extract random cumulative sum of species per studies-----
#Data is our dataset with the cols of interest
#Level: Study_id or Network_id
#Taxa: Pollinator_accepted_name/Plant_accepted_name/Interaction_id  
collect = function(data, level_id, taxa) {
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
collect_curves = map_dfr(1:100, ~ collect(data1, Study_id, Pollinator_accepted_name), .id="iteration") 
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
collect_curves = map_dfr(1:100, ~ collect(data1, Study_id, Plant_accepted_name), .id="iteration") 
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
#Calculate N collectors curves for interactions
collect_curves = map_dfr(1:100, ~ collect(data1, Study_id, Interaction_id), .id="iteration") 
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
collect_curves = map_dfr(1:100, ~ collect(data1, Network_id, Pollinator_accepted_name), .id="iteration") 
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
collect_curves = map_dfr(1:100, ~ collect(data1, Network_id, Plant_accepted_name), .id="iteration") 
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
collect_curves = map_dfr(1:100, ~ collect(data1, Network_id, Interaction_id), .id="iteration") 
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


#Try now to obtain accumulated plants per pollinator species----



collect1 = function(data) {
sampled_pollinator = sample(unique(data1$Pollinator_accepted_name))
data1 %>% 
filter(Pollinator_accepted_name %in% sampled_pollinator) %>%
arrange(match(Pollinator_accepted_name, sampled_pollinator)) %>%
group_by(Plant_accepted_name) %>%
mutate(distinct = row_number() == 1) %>%
ungroup() %>%
group_by(Pollinator_accepted_name) %>%
mutate(Unique_spp = sum(distinct)) %>%
ungroup() %>%
select(Pollinator_accepted_name, Unique_spp) %>%
distinct() %>%
mutate(Unique_spp_cumulative = cumsum(Unique_spp)) %>%
mutate(Pollinator_sampled = row_number())
}


#Calculate N collectors curves for interactions
collect_curves1 = map_dfr(1:100, ~ collect1(data1), .id="iteration") 
#Calculate mean across curves
rarefaction_curve1 = collect_curves1 %>% 
group_by(Pollinator_sampled) %>% 
summarise(mean_curve = mean(Unique_spp_cumulative))
#Plot curves
pp1 = collect_curves1 %>% 
ggplot(aes(x = Pollinator_sampled, y= Unique_spp_cumulative, group = iteration)) +
geom_line(color = "gray") + 
geom_line(data = rarefaction_curve1, 
          aes(x = Pollinator_sampled, y =  mean_curve), inherit.aes = FALSE) +
ylab("Plant species") +
xlab("Pollinator species") +
theme_bw() +
coord_cartesian(expand = FALSE)
pp1                            
