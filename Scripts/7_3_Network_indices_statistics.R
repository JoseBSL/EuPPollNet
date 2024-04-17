# Evaluate statistical differences between network indices

#Load libraries
library(ggplot2)
library(emmeans)
library(multcomp)
library(broom)
library(conflicted)
conflict_prefer("select", "dplyr")
# Load data
data = readRDS("Data/3_Final_data/Interaction_data.rds")
metrics_by_network = readRDS("Data/Working_files/metrics_by_network.rds")
null_networks = readRDS("Data/Working_files/null_networks.rds")
null_metrics_networks = readRDS("Data/Working_files/null_metrics_networks.rds")
# Prepare data
data = data %>%
dplyr::mutate(Year = lubridate::year(Date), 
              Month = lubridate::month(Date), 
              Day = lubridate::day(Date))

# Subset data for studies with only bumblebees
v = c("13_Karise", "22_Kallnik")
subsetting = unique(data$Study_id)

long_format = data %>% 
mutate(Network_id = paste0(Study_id, "_", Network_id, "_", Year)) %>% 
mutate(Network_id = str_replace_all(Network_id, " ","_")) %>% 
filter(!Study_id %in% v) %>% 
select(Network_id, Plant_accepted_name, Pollinator_accepted_name)

# Filter networks larger than 4 plants and 4 pollinator species
v2 = long_format %>% 
group_by(Network_id) %>% 
summarise(Poll_spp = n_distinct(Plant_accepted_name),
          Plant_spp = n_distinct(Pollinator_accepted_name)) %>% 
ungroup() %>% 
filter(Poll_spp >= 4 & Plant_spp >= 4) %>% 
pull(Network_id)

long_format = long_format %>% 
filter(Network_id %in% v2)

# Filter networks with more links than species
links = long_format %>% 
group_by(Network_id, Plant_accepted_name, Pollinator_accepted_name) %>%
distinct() %>% 
group_by(Network_id) %>% 
summarise(links = length(Network_id))

spp = long_format %>% 
group_by(Network_id) %>% 
summarise(n_plants = n_distinct(Plant_accepted_name),
          n_polls = n_distinct(Pollinator_accepted_name)) %>% 
mutate(n_total = n_plants + n_polls)

vector_to_include = left_join(links, spp)

v3 = vector_to_include %>% 
filter(links > n_total) %>% 
pull(Network_id)

long_format = long_format %>% 
filter(Network_id %in% v3)

# Explore how NODFc changes across the Y-X AXES (latitude-Longitude)
long_format_coords = data %>% 
mutate(Network_id = paste0(Study_id, "_", Network_id, "_", Year)) %>% 
mutate(Network_id = str_replace_all(Network_id, " ","_")) %>% 
select(Network_id, Latitude, Longitude, Bioregion) %>% 
distinct()

# Merge data
metrics_by_network_coords = left_join(metrics_by_network, long_format_coords)

#Explore distribution of connectance
metrics_by_network_coords %>% 
ggplot(aes(Connectance)) +
geom_histogram()

#Run model of connectance -latitude
connectance_lat = lm(Connectance ~ Latitude, data = metrics_by_network_coords)
#Get a nice summary from broom package
connectance_lat = glance(connectance_lat)
#Save values to load them in r markdown
connectance_lat_r2 = connectance_lat$adj.r.squared
connectance_lat_pval = connectance_lat$p.value[[1]]
#Save values
saveRDS(connectance_lat_r2, "Data/Manuscript_info/connectance_lat_r2.rds")
saveRDS(connectance_lat_pval, "Data/Manuscript_info/connectance_lat_pval.rds")

#Explore distribution of nestedness
metrics_by_network_coords %>% 
ggplot(aes(Normalised_nestedness)) +
geom_histogram()
#Run model of connectance -latitude
normalised_nestedness_lat = lm(Normalised_nestedness ~ Latitude, data = metrics_by_network_coords)
normalised_nestedness_lat = glance(normalised_nestedness_lat)
#Save values to load them in r markdown
normalised_nestedness_lat_r2 = normalised_nestedness_lat$adj.r.squared
normalised_nestedness_lat_pval = normalised_nestedness_lat$p.value[[1]]
normalised_nestedness_lat_pval = ceiling(normalised_nestedness_lat_pval * 100) / 100

#Save values
saveRDS(normalised_nestedness_lat_r2, "Data/Manuscript_info/normalised_nestedness_lat_r2.rds")
saveRDS(normalised_nestedness_lat_pval, "Data/Manuscript_info/normalised_nestedness_lat_pval.rds")

#Expore statististical differences of connectance across bioclimatic regions
connectance_bioregion = lm(Connectance ~ Bioregion, data = metrics_by_network_coords)
marginal = emmeans(connectance_bioregion, "Bioregion")
pairs(marginal)
normalised_nestedness = lm(normalised_nestedness ~ Bioregion, data = metrics_by_network_coords)
marginal = emmeans(normalised_nestedness, "Bioregion")
pairs(marginal)


#Find min, max values of connectance
min_connectance = metrics_by_network_coords %>%  
slice_min(Connectance) %>% 
pull(Connectance)
#
max_connectance = metrics_by_network_coords %>%  
slice_max(Connectance) %>% 
pull(Connectance)
#
mean_connectance = mean(metrics_by_network_coords$Connectance)
#Save values
saveRDS(min_connectance[[1]], "Data/Manuscript_info/min_connectance.rds")
saveRDS(max_connectance[[1]], "Data/Manuscript_info/max_connectance.rds")
saveRDS(mean_connectance[[1]], "Data/Manuscript_info/mean_connectance.rds")

#Find min, max values of normalised nestedness
min_nodfc = metrics_by_network_coords %>%  
slice_min(Normalised_nestedness) %>% 
pull(Normalised_nestedness)
#
max_nodfc = metrics_by_network_coords %>%  
slice_max(Normalised_nestedness) %>% 
pull(Normalised_nestedness)
#
mean_nodfc = mean(metrics_by_network_coords$Normalised_nestedness)
#Save values
saveRDS(min_nodfc[[1]], "Data/Manuscript_info/min_nodfc.rds")
saveRDS(max_nodfc[[1]], "Data/Manuscript_info/max_nodfc.rds")
saveRDS(mean_nodfc[[1]], "Data/Manuscript_info/mean_nodfc.rds")


#Create here quickly a supplementary figure
# 2nd Species (geometric mean)
library(psych)
species_number = long_format %>% 
group_by(Network_id) %>% 
summarise(N_plants = n_distinct(Plant_accepted_name),
          N_pollinators = n_distinct(Pollinator_accepted_name)) %>% 
group_by(Network_id) %>% 
summarise(geometric_mean_spp = geometric.mean(c(N_plants, N_pollinators)))

metrics_by_network_spp_number = left_join(metrics_by_network, species_number)

p3 = metrics_by_network_spp_number %>% 
ggplot(aes(geometric_mean_spp, Normalised_nestedness)) +
geom_point(pch = 21, fill = "azure3", size=2.5, stroke=0.25, alpha=1) +
theme_bw() +
xlab("Species") +
ylab("Normalised nestedness (NODFc)")
p3



#Calculate correlation between metrics and species mean
##Connectance
connectance_geometric_mean_corr = 
   cor.test(metrics_by_network_spp_number$geometric_mean_spp, 
   metrics_by_network_spp_number$Connectance,
   method = "kendall")
#Prepare outputs to be saved
connectance_spp_mean_corr_tau = connectance_geometric_mean_corr$estimate[[1]]
connectance_spp_mean_corr_pval = connectance_geometric_mean_corr$p.value

#Save
saveRDS(connectance_spp_mean_corr_tau, "Data/Manuscript_info/connectance_spp_mean_corr_tau.rds")
saveRDS(connectance_spp_mean_corr_pval, "Data/Manuscript_info/connectance_spp_mean_corr_pval.rds")
##Nestedness
normalised_nestedness_geometric_mean_corr = 
   cor.test(metrics_by_network_spp_number$geometric_mean_spp, 
   metrics_by_network_spp_number$Normalised_nestedness,
   method = "kendall")
#Prepare outputs to be saved
normalised_nestedness_spp_mean_corr_tau = normalised_nestedness_geometric_mean_corr$estimate[[1]]
normalised_nestedness_spp_mean_corr_pval = normalised_nestedness_geometric_mean_corr$p.value
#Save values
#Save values
saveRDS(normalised_nestedness_spp_mean_corr_tau, "Data/Manuscript_info/normalised_nestedness_spp_mean_corr_tau.rds")
saveRDS(normalised_nestedness_spp_mean_corr_pval, "Data/Manuscript_info/normalised_nestedness_spp_mean_corr_pval.rds")

#Finally, find % of networks that differ statistically
# Merge and process data
d = left_join(metrics_by_network, null_metrics_networks)
d = d %>% 
mutate(z_score = (Classic_nestedness - Mean_null_classic_nestedness) / Deviation_null_classic_nestedness)

p = 0.05 #cutoff probability 95% confidence
critical_value = qnorm(p/2) #double tail probability divide by 2

d1 = d %>%
mutate(infra_over_represented = case_when(
z_score < -abs(critical_value) ~ "Under-represented",
between(z_score, -abs(critical_value), abs(critical_value)) ~ "No statistical difference",
    z_score > abs(critical_value) ~ "Over-represented"
  ))


#Calculate percentages
colnames(d1)
z_percent = d1 %>% 
group_by(infra_over_represented) %>% 
summarise(percent = length(infra_over_represented)/nrow(.) * 100)
#Create tibble of the missing category
to_bind = tibble(infra_over_represented = "Under-represented", percent = 0)
#Bind to original tibble
z_percent1 = bind_rows(z_percent, to_bind)
#Save data
saveRDS(z_percent1, "Data/Manuscript_info/z_percent.rds")


no_dif_z = z_percent %>% 
filter(infra_over_represented == "No statistical difference")
