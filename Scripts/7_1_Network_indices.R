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

#Split date into 3 cols
data = data %>%
dplyr::mutate(Year = lubridate::year(Date), 
                Month = lubridate::month(Date), 
                Day = lubridate::day(Date))

#Exclude honey bees
#data = data %>% 
#filter(Pollinator_accepted_name!= "Apis mellifera")

#Check studies with more than 1 year
c = data %>% 
group_by(Study_id) %>% 
summarize(n_distinct(Year))


#Prepare data----
#-Objective- matrix within tibble with Network_id as identifier

#Filtering criteria
#1 Studies with only bumblebees
#2 Networks larger than 4 plants and 4 poll species
#3 Networks with more links than species

###-------------------------------------------------------------------#
#1Prepare code to filter studies with only bumblebees
v = c("9_Heleno", "13_Karise", "22_Kallnik")
subsetting = unique(data$Study_id)

long_format = data %>% 
mutate(Network_id = paste0(Study_id, "_", Network_id, "_", Year)) %>% 
mutate(Network_id = str_replace_all(Network_id, " ","_")) %>% 
  
filter(!Study_id %in% v) %>% 
select(Network_id, Plant_accepted_name, Pollinator_accepted_name) 
###-------------------------------------------------------------------#
#2 Networks larger than 4 plants and 4 poll species
#Get vector with network that need to be included (minimum 5 plant and 5 poll species)
v2 = long_format %>% 
group_by(Network_id) %>% 
summarise(Poll_spp = n_distinct(Plant_accepted_name),
          Plant_spp = n_distinct(Pollinator_accepted_name)) %>% 
ungroup() %>% 
filter(Poll_spp >= 4 & Plant_spp >= 4) %>% 
pull(Network_id)
#Filter by networks that fulfill criteria
long_format = long_format %>% 
filter(Network_id %in% v2)
###-------------------------------------------------------------------#
#3 Networks with more links than species
#Filter out networks with less links than spp
#Probably undersampling!
links = long_format %>% 
group_by(Network_id, Plant_accepted_name, Pollinator_accepted_name) %>%
distinct() %>% 
group_by(Network_id) %>% 
summarise(links = length(Network_id))

spp = long_format %>% 
group_by(Network_id) %>% 
summarise(n_plants = n_distinct(Plant_accepted_name),
          n_polls = n_distinct(Pollinator_accepted_name)) %>% 
mutate(n_total = n_plants+n_polls)

vector_to_include = left_join(links,spp)
#Select networks with more links than spp
v3 = vector_to_include %>% 
filter(links > n_total) %>% 
pull(Network_id)

long_format = long_format %>% 
filter(Network_id %in% v3)

###-------------------------------------------------------------------#
#Final data organisation before obtaining nestedness metrics 
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


#Define the nestedness function (bascompte)
nestedness_temp = function(interaction_matrix) {
# Calculate nestedness using the nestedtemp function
s = nestedtemp(interaction_matrix)
# Return the statistic
return(s$statistic/100)

classic_nestedness = function(interaction_matrix){
# Calculate nestedness using the nestedtemp function
s = nestednodf(interaction_matrix)
# Return the statistic
return(s$statistic[3]/100)
}
}
#Compute network metrics-----
#Normalised nestedness (NODFc)and classic netedness (from Almeida-Neto )
#Note: Classic_nestedness (nodf_cpp) same as nestednof from vegan package
#Note: Quality 0 in NODFc makes everything faster (note that is the default value)
metrics_by_network = matrices %>%
mutate(Normalised_nestedness = map(Matrices, ~ NODFc(.))) %>%
mutate(Classic_nestedness = map(Matrices, ~ classic_nestedness(.))) %>%
mutate(Nestedness_temp = map(Matrices, ~ nestedness_temp(.))) %>%
mutate(Connectance = map(Matrices, ~ networklevel(., index="connectance"))) %>%
select(c(Network_id, Normalised_nestedness, Classic_nestedness,Nestedness_temp, Connectance)) %>% 
unnest(cols = c(Normalised_nestedness, Classic_nestedness, Nestedness_temp, Connectance))

#Save data
saveRDS(metrics_by_network, "Data/Working_files/metrics_by_network.rds")
saveRDS(matrices, "Data/Working_files/matrices_by_network.rds")
