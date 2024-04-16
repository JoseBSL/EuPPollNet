
#Code to visualize the locations of the networks
#1st) Map with a gradient colour of coordinates
#The more sites sampled together the greater the intensity

#Load libraries
library(ggplot2)
library(giscoR)
library(sf)
library(dplyr)

#Load clean dataset with georeferenced interactions
data = readRDS("Data/Interactions_accepted_names.rds")

#Select for plotting distinct rows of Lat/Long
for_plotting = data %>% 
select(Study_id, Latitude, Longitude) %>% 
distinct()

#Create duplicate cord columns to operate on them
for_plotting$coords <- data.frame(
Longitude = for_plotting$Longitude, Latitude = for_plotting$Latitude)
#Set the default coordinate format
for_plotting$coords_sf = st_as_sf(for_plotting$coords, 
  coords = c("Longitude", "Latitude"), crs = 4326)
#Get countries and transfrom coordinates
countries <- gisco_get_countries(
resolution = 20) %>%
st_transform(3035) %>% 
sf::st_as_sf(crs = 4326)
#Plot Europe map
#Code dapted from other project that I did
#It could be cleaner
ggplot(countries) +
geom_sf(fill="gray80",color = "black", 
        alpha = 1,size=0.2) +
geom_bin2d(data = for_plotting, aes(x = Longitude, 
        y = Latitude, fill=log(..count..)), 
        size = 0.8,bins=50) +
viridis::scale_fill_viridis(name = "log(Count)", 
        breaks = c(0, 2,4), limits=c(0,4.75)) +
theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black",
        fill=NA, size=1)) +
ylab("Latitude") +
xlab("Longitude") +
ggtitle("B") +
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
scale_y_continuous(breaks = seq(40, 60, by = 6)) +
scale_x_continuous(breaks = seq(-8, 18, by = 8)) +
guides(fill = guide_colourbar(title.position = "top", 
          title.hjust = .5)) +
coord_sf(crs = st_crs(3035), default_crs = st_crs(4326),
         xlim = c(-14, 39), ylim = c(35, 70))




