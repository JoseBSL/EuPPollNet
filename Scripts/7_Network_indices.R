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
v = c("13_Karise", "22_Kallnik")
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

#Compute network metrics-----
#Normalised nestedness (NODFc)and classic netedness (from Almeida-Neto )
#Note: Classic_nestedness (nodf_cpp) same as nestednof from vegan package
#Note: Quality 0 in NODFc makes everything faster (note that is the default value)
metrics_by_network = matrices %>%
mutate(Normalised_nestedness = map(Matrices, ~ NODFc(.))) %>%
mutate(Classic_nestedness = map(Matrices, ~ nodf_cpp(.))) %>%
mutate(Connectance = map(Matrices, ~ networklevel(., index="connectance"))) %>%
select(c(Network_id, Normalised_nestedness, Classic_nestedness, Connectance)) %>% 
unnest(cols = c(Normalised_nestedness, Classic_nestedness, Connectance))

#Save data
saveRDS(metrics_by_network, "Data/Working_files/metrics_by_network.rds")

#Create null networks
null_networks = matrices %>% 
mutate(Null_networks = map(Matrices, ~ vaznull(100, .))) %>% 
select(Network_id, Null_networks) %>% 
unnest(Null_networks)
#Compute nestedness for the nulls 
null_metrics_networks = null_networks %>% 
mutate(Normalised_nestedness = map(Null_networks, ~ NODFc(.))) %>%
mutate(Classic_nestedness = map(Null_networks, ~ nodf_cpp(.))) %>% 
mutate(Connectance = map(Null_networks, ~ networklevel(., index="connectance"))) %>%
select(!Null_networks) %>% 
unnest(cols = c(Normalised_nestedness, Classic_nestedness, Connectance)) %>% 
group_by(Network_id, Connectance) %>% 
summarise(Mean_null_normalised_nestedness = mean(Normalised_nestedness),
          Deviation_null_normalised_nestedness = sd(Normalised_nestedness),
          Mean_null_classic_nestedness= mean(Classic_nestedness),
          Deviation_null_classic_nestedness= sd(Classic_nestedness))

#Save data
saveRDS(null_networks, "Data/Working_files/null_networks.rds")
saveRDS(null_metrics_networks, "Data/Working_files/null_metrics_networks.rds")



#Explore how NODFc changes across the Y-X AXES (latitude-Longitude)
#Extract columns of interest
long_format_coords = data %>% 
mutate(Network_id = paste0(Study_id, "_", Network_id, "_", Year)) %>% 
mutate(Network_id = str_replace_all(Network_id, " ","_")) %>% 
#filter(Study_id %in% v) %>% 
select(Network_id, Latitude, Longitude, Bioregion) %>% 
distinct()

#Merge data
metrics_by_network_coords = left_join(metrics_by_network, long_format_coords)
#Plot
p1 = metrics_by_network_coords %>% 
ggplot(aes(Latitude, Normalised_nestedness)) +
geom_point()

p2 = metrics_by_network_coords %>% 
ggplot(aes(Longitude, Normalised_nestedness)) +
geom_point()

summary = metrics_by_network_coords %>% 
group_by(Bioregion) %>% 
tally()

p3 = metrics_by_network_coords %>% 
ggplot(aes(Bioregion, Normalised_nestedness)) +
geom_boxplot() +
geom_text(data = summary,
            aes(Bioregion, Inf, label = n), vjust = 1)

library(patchwork)
p1 + p2 + p3

library('psych') #For geometric mean
#Calculate geometric mean
species_number = long_format %>% 
group_by(Network_id) %>% 
summarise(N_plants = n_distinct(Plant_accepted_name),
          N_pollinators = n_distinct(Pollinator_accepted_name)) %>% 
group_by(Network_id) %>% 
summarise(geometric_mean_spp = geometric.mean(c(N_plants, N_pollinators)))


#Merge data
metrics_by_network_spp_number = left_join(metrics_by_network, species_number)
p4 = metrics_by_network_spp_number %>% 
ggplot(aes(geometric_mean_spp, Normalised_nestedness)) +
geom_point()
p4

p4 = metrics_by_network_spp_number %>% 
ggplot(aes(geometric_mean_spp, Normalised_nestedness)) +
geom_point() +
scale_x_log10() +
xlab("log(geometric mean species)") 
p4
#Calculate geometric mean

#Obtain total interactions per network
interactions = long_format %>% 
group_by(Network_id, Plant_accepted_name, Pollinator_accepted_name) %>%
summarise(Interactions = n()) %>% 
group_by(Network_id) %>% 
summarise(interactions_total = sum(Interactions))

metrics_by_network_interactions = left_join(metrics_by_network, interactions)

p5 = metrics_by_network_interactions %>% 
ggplot(aes(interactions_total, Normalised_nestedness)) +
geom_point() +
scale_x_log10() +
xlab("log(Interactions)") 
p5

#Check
d = left_join(species_number, interactions)
d %>% 
ggplot(aes(interactions_total, geometric_mean_spp)) +
geom_point() +
scale_x_log10() +
scale_y_log10() +
geom_smooth(method = lm, se = FALSE)
#As expected both are highly correlated
#Some weird points, worth checking them

#Get number of sampling days per network and check
sampling_days = data %>% 
mutate(Network_id = paste0(Study_id, "_", Network_id, "_", Year)) %>% 
mutate(Network_id = str_replace_all(Network_id, " ","_")) %>% 
select(Network_id, Date) %>% 
group_by(Network_id) %>% 
summarise(Sampling_days = n_distinct(Date)) 

metrics_by_network_sampling_days = left_join(metrics_by_network, sampling_days)
p6 = metrics_by_network_sampling_days %>% 
ggplot(aes(Sampling_days, Normalised_nestedness)) +
geom_point() +
scale_x_log10() +
xlab("Sampling days") 
p6

#Tasks! Split networks of 2 years in 1 year
(p1 + p2 + p3) /
(p4 + p5 + p6)





d = left_join(metrics_by_network, null_metrics_networks)
colnames(d)

d = d %>% 
mutate(z_score = (Classic_nestedness - Mean_null_classic_nestedness) / Deviation_null_classic_nestedness)


p = 0.05 #cutoff probability 95% confidence
critical_value <- qnorm(p/2) #double tail probability divide by 2

d1 = d %>%
mutate(infra_over_represented = case_when(
    z_score < -abs(critical_value) ~ "Under-represented",
    between(z_score, -abs(critical_value), abs(critical_value)) ~ "No statistical difference",
    z_score > abs(critical_value) ~ "Over-represented"
  ))


levels(factor(d1$infra_over_represented))

#Plot
d1 %>% 
ggplot(aes(x = z_score) ) +
geom_histogram(aes(y = ..density..), bins = 65, alpha = 0.5, color = "black", fill = "azure3") +
stat_function(fun = dnorm,
              args = list(mean = mean(d1$z_score),
                          sd = sd(d1$z_score)),
              n = 1000, inherit.aes = FALSE, color = "gray18") +
stat_function(data=d1,fun = dnorm, color = "black", linetype = "dashed") +
theme_bw() +
coord_cartesian(expand = FALSE)+
geom_vline(xintercept = -abs(critical_value), linetype = "longdash", colour = "black")+
geom_vline(xintercept = abs(critical_value), linetype = "longdash", colour = "black")+ 
ylab("Density") +
xlim(-5,9)



  

ggplot(d1, aes(x=z_score, color=infra_over_represented, fill=infra_over_represented)) + 
geom_histogram(bins = 200, alpha = 0.5, position = "identity",lwd = 0.2, color = "black")+
geom_density(data = d1, aes(x=z_score),alpha = 0.3, color = "black") +  # Add density curve
geom_vline(xintercept = -abs(critical_value), linetype = "longdash")+
geom_vline(xintercept = abs(critical_value), linetype = "longdash")+ ylab("Frequency")+  theme_bw() +
scale_fill_manual(name="" ,values=c("Under-represented"= "coral2", 
                                    "No statistical difference"= "palegreen3", 
                                   "Over-represented"=  "cyan3")) +
#scale_color_manual(name="" ,values=c("Under-represented"= "coral2", 
#                                    "No statistical difference"= "palegreen3", 
#                                   "Over-represented"=  "cyan3")) + xlab("Z-score") +
xlim(-9,9) +
coord_cartesian(expand = FALSE) 





#Calculate Z-scores
nestedness_by_network
null_metrics_networks


colnames(d)

d %>% 
ggplot(aes(x=z_scores))+
geom_density(linetype="dashed") +
geom_vline(xintercept = c(-1.96, 1.96), linetype = "solid", color = "red") 






#Prepare code with lines proportional to geom_hist
library(ggplot2)
library(dplyr)

# Assuming d1 is your data frame

# Pre-calculate density
density_data <- density(d1$z_score, bw = "SJ")
density_data = tibble(x=density_data$x, y=density_data$y)
# Calculate critical value if not defined already
critical_value <- qnorm(0.975) # For a two-tailed test, change the value accordingly

# Determine the bin width
bin_width <- diff(range(density_data$x)) / length(density_data$x)


f = function(x){
  dnorm(x)*1.8}


# Plot
d1 %>% 
  ggplot(aes(x = z_score)) +
  geom_histogram(aes(y = ..density.. , fill = infra_over_represented), bins = 65, alpha = 0.5, color = "black") +
geom_histogram(aes(y = ..density.., fill = infra_over_represented), bins = 65, alpha = 0.5, color = "black") +
    stat_function(data=d1, fun = function(x) dnorm(x, mean = mean(d1$z_score), sd = sd(d1$z_score)) * 2, 
                  n = 1000, inherit.aes = FALSE, color = "gray18")+
  geom_function(fun = f, color = "black", linetype = "dashed") +
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  geom_vline(xintercept = -abs(critical_value), linetype = "longdash", colour = "black") +
  geom_vline(xintercept = abs(critical_value), linetype = "longdash", colour = "black") +
  ylab("Density") +
  xlim(-5, 9)
