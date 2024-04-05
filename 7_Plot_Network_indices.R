# Load libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)

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

# Connectance plots
# 1st Latitude
p1 = metrics_by_network_coords %>% 
  ggplot(aes(Latitude, Connectance)) +
  geom_point(aes(fill = Bioregion, shape = Bioregion), stroke = 0.25, size=2) +
  theme_bw() +
  ylab("Connectance") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  scale_shape_manual(values = c(
    "Alpine" = 21,  # Circle
    "Atlantic" = 22, # Triangle
    "Boreal" = 23,   # Square
    "Continental" = 24,  # Diamond
    "Mediterranean" = 25  # Cross
  ))+
ggtitle("(a)")


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
ggtitle("(b)")

# Merge and process data
d = left_join(metrics_by_network, null_metrics_networks)
colnames(d)
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

# 2nd Species (geometric mean)
species_number = long_format %>% 
  group_by(Network_id) %>% 
  summarise(N_plants = n_distinct(Plant_accepted_name),
            N_pollinators = n_distinct(Pollinator_accepted_name)) %>% 
  group_by(Network_id) %>% 
  summarise(geometric_mean_spp = geometric.mean(c(N_plants, N_pollinators)))

metrics_by_network_spp_number = left_join(metrics_by_network, species_number)

p3 = metrics_by_network_spp_number %>% 
ggplot(aes(geometric_mean_spp, Connectance)) +
geom_point(pch = 21, fill = "azure3") +
theme_bw() +
xlab("Species")+
ggtitle("(c)")


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
# Layout plots
a = p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position = "right")
b = (p3 + p4)
a / b
