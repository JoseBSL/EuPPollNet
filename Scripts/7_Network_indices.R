#Compute network metrics indices
# - Requires before the data preparation of the matrices 
#Nestedness as proposed by Song et al., 2017
#maxnodf package from https://doi.org/10.1111/2041-210X.13545

#Load libraries
library(dplyr)
#install.packages("maxnodf")
library(maxnodf) #for nestedness
library(bipartite) #for null networks
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)

#Read data
data = readRDS("Data/3_Final_data/Interaction_data.rds")

#Prepare data----
#-Objective- matrix within tibble with Network_id as identifier

#Prepare code to filter more studies
v = unique(data$Study_id)

long_format = data %>% 
mutate(Network_id = paste0(Study_id, "_", Network_id)) %>% 
mutate(Network_id = str_replace_all(Network_id, " ","_")) %>% 
filter(Study_id %in% v[1:10]) %>% 
select(Network_id, Plant_accepted_name, Pollinator_accepted_name) 

#Get vector with network that need to be included (minimum 4 plant and 4 poll species)
networks_to_include = long_format %>% 
group_by(Network_id) %>% 
summarise(Poll_spp = n_distinct(Plant_accepted_name),
          Plant_spp = n_distinct(Pollinator_accepted_name)) %>% 
ungroup() %>% 
filter(Poll_spp >= 4 & Plant_spp >= 4) %>% 
pull(Network_id)


#Filter by networks that fulfill criteria
long_format = long_format %>% 
filter(Network_id %in% networks_to_include)

#Nest data by study Id
nested_by_network = long_format %>% 
nest(Networks = c(Plant_accepted_name, Pollinator_accepted_name))

#Function: Convert to matrix and sum interactions of same plants and polls
sum_interactions = function(data) {
  data %>% 
  group_by(Plant_accepted_name, Pollinator_accepted_name) %>%
  summarise(Interactions = n()) %>%
  #This is not necesary but make it binary anyway
  mutate(Interactions = if_else(Interactions > 0, 1, 0)) %>% 
  pivot_wider(names_from = Pollinator_accepted_name, 
                      values_from = Interactions,
                      values_fill = 0) %>% 
  tibble::column_to_rownames("Plant_accepted_name") %>% 
  as.matrix()
}

#Apply function to each element of the nested tibble
matrices = nested_by_network %>%
mutate(Matrices = map(Networks, sum_interactions)) %>%
ungroup()
#Data is ready to compute metrics!
#Safety check for network 1
#m = as.matrix(matrices$Matrices[[81]])


#Compute network metrics-----
#Normalised nestedness (NODFc)and classic netedness (from Almeida-Neto )
#Note: Classic_nestedness (nodf_cpp) same as nestednof from vegan package
#Note: Quality 0 in NODFc makes everything faster (note that is the default value)
nestedness_by_network = matrices %>%
mutate(Normalised_nestedness = map(Matrices, ~ NODFc(.))) %>%
mutate(Classic_nestedness = map(Matrices, ~ nodf_cpp(.))) %>%
select(c(Network_id, Normalised_nestedness, Classic_nestedness)) %>% 
unnest(cols = c(Normalised_nestedness, Classic_nestedness))

#Create null networks, same connectance but different marginal totals
null_networks = matrices %>% 
mutate(Null_networks = map(Matrices, ~ vaznull(20, .))) %>% 
select(Network_id, Null_networks) %>% 
unnest(Null_networks)
#Compute nestedness for the nulls 
null_metrics_networks = null_networks %>% 
mutate(Normalised_nestedness = map(Null_networks, ~ NODFc(.))) %>%
mutate(Classic_nestedness = map(Null_networks, ~ nodf_cpp(.))) %>% 
select(!Null_networks) %>% 
unnest(cols = c(Normalised_nestedness, Classic_nestedness)) %>% 
group_by(Network_id) %>% 
summarise(Mean_null_normalised_nestedness = mean(Normalised_nestedness),
          Deviation_null_normalised_nestedness = sd(Normalised_nestedness),
          Mean_null_classic_nestedness= mean(Classic_nestedness),
          Deviation_null_classic_nestedness= sd(Classic_nestedness))

#Create dataset for plotting
a = nestedness_by_network %>% 
select(Network_id, Normalised_nestedness) %>% 
mutate(Category = "Observed")

b = null_metrics_networks %>% 
select(Network_id, Mean_null_normalised_nestedness) %>% 
rename(Normalised_nestedness = Mean_null_normalised_nestedness) %>% 
mutate(Category = "Null")
c = bind_rows(a, b)

c1 = c %>% 
group_by(Study, Category) %>% 
summarise(average_nestedness = mean(Normalised_nestedness))

#To color by study
c$Study = str_extract(c$Network_id, "[^_]+")
ggplot(c, aes(x = Category, y = Normalised_nestedness, col = Study)) +
geom_point() +
geom_line(aes(group = Study), linewidth= 0.1) +
theme_bw() +
theme(legend.position = "none")

ggplot(c1, aes(x = Category, y = average_nestedness, col = Study)) +
geom_point() +
geom_line(aes(group = Study), linewidth= 0.5) +
theme_bw() +
theme(legend.position = "none")





#Create dummy tibble for plotting
mean = c(mean(nestedness_by_network$Normalised_nestedness),
         mean(null_metrics_networks$Mean_null_normalised_nestedness))

sd = c(sd(nestedness_by_network$Normalised_nestedness),
       sd(null_metrics_networks$Mean_null_normalised_nestedness))

d = tibble(mean, sd, var = c("observed", "null"))

d = d %>% 
mutate(Upper_sd = mean + sd) %>% 
mutate(Lower_sd = mean - sd)

library(ggplot2)

ggplot(d, aes(var, mean)) +
geom_point() +
geom_errorbar(aes(ymin = Lower_sd, ymax = Upper_sd), width = 0.1) +
ylab("Mean normalised nestedness") +
xlab("Example: 3 Studies")

#Exclude singletones?
