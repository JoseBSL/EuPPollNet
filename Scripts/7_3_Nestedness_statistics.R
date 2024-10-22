#Explore how nestedness is associated with Latitude and how 
#it changes compared to null expextations
#Note! We use 2 different metrics of Nestedness 
#given the different questions
#In this script we prepare Figure 6b and 6d and supplementary s3 

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

#Explore distribution of nestedness
metrics_by_network_coords %>% 
ggplot(aes(Normalised_nestedness)) +
geom_histogram()
#Run model of nestedness -latitude
model_normalised_nestedness_lat= lm(Normalised_nestedness ~ Latitude, data = metrics_by_network_coords)
normalised_nestedness_lat = glance(model_normalised_nestedness_lat)
#Save values to load them in r markdown
normalised_nestedness_lat_r2 = normalised_nestedness_lat$adj.r.squared
normalised_nestedness_lat_pval = normalised_nestedness_lat$p.value[[1]]
normalised_nestedness_lat_pval = ceiling(normalised_nestedness_lat_pval * 100) / 100
#Save values
saveRDS(normalised_nestedness_lat_r2, "Data/Manuscript_info/normalised_nestedness_lat_r2.rds")
saveRDS(normalised_nestedness_lat_pval, "Data/Manuscript_info/normalised_nestedness_lat_pval.rds")
#Predict values with confidence intervals
prediction_intervals <- predict(
  model_normalised_nestedness_lat, 
  newdata = metrics_by_network_coords, 
  interval = "confidence")
#Add to dataset
metrics_by_network_coords = cbind(metrics_by_network_coords, prediction_intervals)

#Save data for plotting
saveRDS(metrics_by_network_coords, "Data/Manuscript_info/nestedness_latitude.rds")

# 2nd Plot
p2 = metrics_by_network_coords %>% 
ggplot(aes(Latitude, Normalised_nestedness)) +
geom_point(aes(fill = Bioregion, shape = Bioregion), stroke = 0.25, size=2) +
theme_bw() +
ylab("NODFc") +
scale_fill_viridis_d() +
scale_colour_viridis_d() +
scale_shape_manual(values = c(
  "Alpine" = 21,  # Circle
  "Atlantic" = 22, # Triangle
  "Boreal" = 23,   # Square
  "Continental" = 24,  # Diamond
  "Mediterranean" = 25  # Cross
  )) +
geom_line(data = metrics_by_network_coords,aes(x=Latitude, y = fit), inherit.aes = FALSE)+
geom_ribbon(data = metrics_by_network_coords, 
    aes(x=Latitude,ymin = lwr, ymax = upr), inherit.aes = FALSE,
    fill = "grey70", alpha= 0.3) +
theme_bw()+
ggtitle("(b)")
p2


#Explore statistical differences of normalised nestedness acros bioregion
normalised_nestedness = lm(Normalised_nestedness ~ Bioregion, data = metrics_by_network_coords)
marginal = emmeans(normalised_nestedness, "Bioregion")
pairs(marginal)

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
#Save this tibble
saveRDS(metrics_by_network_spp_number, "Data/Working_files/metrics_by_network_spp_number.rds")

p3 = metrics_by_network_spp_number %>% 
ggplot(aes(geometric_mean_spp, Normalised_nestedness)) +
geom_point(pch = 21, fill = "azure3", size=2.5, stroke=0.25, alpha=1) +
theme_bw() +
xlab("Species") +
ylab("Normalised nestedness (NODFc)")
p3


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
d = left_join(metrics_by_network, null_metrics_networks, by = "Network_id")
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


saveRDS(d1, "Data/Manuscript_info/nestedness_z_scores.rds")
# Plot
p4 = d1 %>% 
  ggplot(aes(x = z_score)) +
  geom_histogram(aes(y = ..density.., fill = infra_over_represented), 
                 bins = 100, alpha = 0.5, color = "black", position = "stack") +
  stat_function(data = d1, fun = function(x) dnorm(x, mean = mean(d1$z_score), sd = sd(d1$z_score)) * 1.7, 
                n = 1000, inherit.aes = FALSE, color = "gray18", size = 1) +
  theme_bw() +
  coord_cartesian(expand = FALSE) +
  geom_vline(xintercept = -abs(critical_value), linetype = "longdash", colour = "red") +
  geom_vline(xintercept = abs(critical_value), linetype = "longdash", colour = "red") +
  ylab("Density") +
  xlab("Z scores (NODFc)")  +
  xlim(-6, 9) +
  ylim(0, 0.8) +
  scale_fill_manual(name = "Observed against null",
                    limits = c("Under-represented", "No statistical difference", "Over-represented"),
                    labels = c("Less nested", "No difference", "More nested"),
                    values = c("Under-represented" = "coral2", 
                               "No statistical difference" = "palegreen3", 
                               "Over-represented" = "cyan3")) +
ggtitle("(d)")

p4




