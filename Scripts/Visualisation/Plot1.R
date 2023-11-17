
#Load data
data = readRDS("Data/Interactions_accepted_names.rds")

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
ggplot(countries) +
geom_sf(fill="gray90",color = "gray40", 
        alpha = 1,size=0.2) + 
geom_point(data = all_data, alpha=1,shape=21,
           aes(x= Longitude, y=Latitude,
           size=Counts, group=Group, colour = Group))+  
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
         size = 10), legend.direction="horizontal",
         legend.spacing.y = unit(-0.5, "cm")) +
theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black",
        fill=NA, size=1))+
  scale_color_manual(values = c("#1F968BFF", "darkorange3")) +
ylab("Latitude") +
xlab("Longitude") #+
#guides(size=guide_legend(title="Plant\nspecies"))
