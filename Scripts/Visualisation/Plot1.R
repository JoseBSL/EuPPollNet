
#Load libraries
library(ggplot2)
library(giscoR)
library(sf)
library(dplyr)
library(patchwork)

#Load data
data = readRDS("Data/Interactions_uncounted.rds")

#Prepare coordinates
for_plotting = data %>% 
select(Study_id, Latitude, Longitude) %>% 
distinct()
#Get countries and transfrom coordinates
countries <- gisco_get_countries(
resolution = 20) %>%
st_transform(3035) %>% 
sf::st_as_sf(crs = 4326)

#Number of plant and poll species per network
#-----------------------#
#Pollinators------
#-----------------------#
#Check unique number of pollinator species
PollNumber_by_ID = data %>% 
select(Pollinator_rank, Pollinator_accepted_name, Study_id) %>% 
filter(Pollinator_rank == "SPECIES") %>% 
select(Pollinator_accepted_name, Study_id) %>% 
group_by(Study_id) %>% 
summarise(Pollinator_counts = n_distinct(Pollinator_accepted_name))
#Merge now with spp counts by network dataset
poll_data = left_join(PollNumber_by_ID, for_plotting, by = "Study_id")
#Create duplicate cord columns to operate on them
poll_data$coords <- data.frame(
Longitude = poll_data$Longitude, Latitude = poll_data$Latitude)
#Set coordinates
poll_data$coords_sf = st_as_sf(poll_data$coords, 
  coords = c("Longitude", "Latitude"), crs = 4326)
#Plot map of Europe
#Very noisy with many coordinates per point,
#Let's select a unique coordinate by study
poll_data1 =  poll_data %>% 
distinct(Study_id, .keep_all = TRUE)

#Plot!
ggplot(countries) +
geom_sf(fill="gray90",color = "gray40", 
        alpha = 1,size=0.2) +
geom_sf(data = poll_data1$coords_sf, col = "darkorange3",
        alpha=1,shape=21, aes(size=poll_data1$Pollinator_counts)) +
coord_sf(crs = st_crs(3035), default_crs = st_crs(4326),
         xlim = c(-14, 39), ylim = c(35, 70))+
theme(panel.grid.major = element_line(color = gray(0.5),
         linetype = "dashed", size = 0.5), 
         legend.position = c(0, 1), 
         legend.justification = c(0, 1),
         legend.background = element_blank(),
         legend.box.background = element_blank(),
         legend.key = element_blank(), 
         legend.title = element_text(face = "bold"),
         axis.title = element_text(face="bold", size = 14),
         plot.title = element_text(face="bold", size = 18),
         legend.text = element_text(face = "bold",
         size = 12), legend.direction="horizontal") +
theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black",
        fill=NA, size=1))+ 
ylab("Latitude") +
xlab("Longitude") +
guides(size=guide_legend(title="Pollinator\nspecies"))
#-----------------------#
#Plants-----
#-----------------------#
#Check unique number of plant species
PlantNumber_by_ID = data %>% 
select(Plant_rank, Plant_accepted_name, Study_id) %>% 
filter(Plant_rank == "SPECIES") %>% 
select(Plant_accepted_name, Study_id) %>% 
group_by(Study_id) %>% 
summarise(Plant_counts = n_distinct(Plant_accepted_name))
#Merge now with spp counts by network dataset
plant_data = left_join(PlantNumber_by_ID, for_plotting, by = "Study_id")
#Create duplicate cord columns to operate on them
plant_data$coords <- data.frame(
Longitude = plant_data$Longitude, Latitude = plant_data$Latitude)
#Set coordinates
plant_data$coords_sf = st_as_sf(plant_data$coords, 
  coords = c("Longitude", "Latitude"), crs = 4326)
#Plot map of Europe
#Very noisy with many coordinates per point,
#Let's select a unique coordinate by study
plant_data1 =  plant_data %>% 
distinct(Study_id, .keep_all = TRUE)
#Plot!
ggplot(countries) +
geom_sf(fill="gray90",color = "gray40", 
        alpha = 1,size=0.2) +
geom_sf(data = plant_data1$coords_sf, col = "#1F968BFF",
        alpha=1,shape=21, aes(size=plant_data1$Plant_counts)) +
coord_sf(crs = st_crs(3035), default_crs = st_crs(4326),
         xlim = c(-14, 39), ylim = c(35, 70))+
theme(panel.grid.major = element_line(color = gray(0.5),
         linetype = "dashed", size = 0.5), 
         legend.position = c(0, 1), 
         legend.justification = c(0, 1),
         legend.background = element_blank(),
         legend.box.background = element_blank(),
         legend.key = element_blank(), 
         legend.title = element_text(face = "bold"),
         axis.title = element_text(face="bold", size = 14),
         plot.title = element_text(face="bold", size = 18),
         legend.text = element_text(face = "bold",
         size = 12), legend.direction="horizontal") +
theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black",
        fill=NA, size=1))+ 
ylab("Latitude") +
xlab("Longitude") +
guides(size=guide_legend(title="Plant\nspecies"))

#Add vars
plant_data1$Group = "Plant"
poll_data1$Group = "Pollinator"
#Rename cols
plant_data2 = plant_data1 %>% 
rename(Counts = Plant_counts)
poll_data2 = poll_data1 %>% 
rename(Counts = Pollinator_counts)
#Bind rows
all_data = bind_rows(plant_data2, poll_data2)

#Try to plot both together (same plot)
map = ggplot(countries) +
geom_sf(fill="floralwhite",color = "black", 
        alpha = 1,linewidth=0.1) + 
geom_point(data = all_data, alpha=1,shape=21,
           aes(x= Longitude, y=Latitude,
           size=Counts, group=Group, colour = Group))+  
coord_sf(crs = st_crs(3035), default_crs = st_crs(4326),
         xlim = c(-14, 39), ylim = c(37, 70))+
theme(panel.grid.major = element_blank(), 
      legend.position = ("top"),
         legend.background = element_blank(),
         legend.box.background = element_blank(),
         legend.key = element_blank(), 
         legend.title = element_text(face = "bold",  size = 8),
      axis.text = element_text(size=6, face="bold"),
         axis.title = element_text(face="bold", size = 10),
         plot.title = element_text(face="bold", size = 10),
         legend.text = element_text(face = "bold",
         size = 6), legend.direction="horizontal",
         legend.spacing.y = unit(-0, "cm"),
      legend.title.align=0.5) +
theme(panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black",
        fill=NA, size=1))+
  scale_color_manual(name = "Taxonomic\ngroup", values = c("#009E73", "#D55E00")) +
  scale_size(name = "Species\ncounts") +
  ylab("Latitude") +
xlab("Longitude") +
guides(color = guide_legend(override.aes = list(size=3)))+
scale_x_continuous(breaks = seq(0, 20, by = 10)) +
scale_y_continuous(breaks = seq(40, 60, by = 10)) 

 #+
#guides(size=guide_legend(title="Plant\nspecies"))


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
p1 = ggplot(poll_spread, 
aes(reorder(Pollinator_accepted_name,-Percent_total), Percent_total)) +
geom_bar(stat = "identity", 
           color = "white",
           lwd = 0.015, fill = "#D55E00") +
ylab("Shared pollinators (%)") +
theme(axis.text.x = element_blank(),
axis.ticks=element_blank()) +
xlab(NULL) +
ylab(NULL) + 
ggtitle("Pollinators")+
geom_vline(xintercept = quantile_50, linetype="dashed", 
color = "black", size=0.25)

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
p2 = ggplot(plant_spread, 
aes(reorder(Plant_accepted_name,-Percent_total), Percent_total, fill = Plant_accepted_name)) +
geom_bar(stat = "identity", 
           color = "white",
           lwd = 0.015, fill = "#009E73") +
ylab("Shared plants (%)")  +
xlab("Species") +
ylab(NULL) + 
ggtitle("Plants") + 
geom_vline(xintercept = quantile_50, linetype="dashed", 
                color = "black", size=0.25)+theme_minimal() +
  theme(panel.grid = element_line())+theme(axis.text.x = element_blank(),
axis.ticks=element_blank(),
axis.title.x = element_text(face = "bold"),
panel.grid.major.x = element_blank())+
scale_y_continuous(expand = c(0, 0)) 

p2


#Prepare panel with plots
plot2 =  wrap_elements(p1 / p2) +  labs(tag = "Shared across studies (%)") + 
theme(
    plot.tag = element_text(size = rel(1), angle = 90, face = "bold"),
    plot.tag.position = "left"
  )
#Plot everything
map + plot2 
