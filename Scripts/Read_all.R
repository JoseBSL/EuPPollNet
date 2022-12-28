library('gtools') #Library that allows to sort files by numerical order
library(tidyverse)
# install.packages("sf")
library(sf)
# install.packages("giscoR")
library(giscoR)

#Read all files with their paths
files <- dir("Data/Clean_data", pattern="*.rds", full.names=T)
files <- mixedsort(files)

#Generate list of lists
all_data = files %>%
map(readRDS) 

#Read all file names
file_names <- dir("Data/Clean_data", pattern="*.rds", full.names=F)
file_names <- mixedsort(file_names)

#Delete extension
file_names = str_replace(file_names, ".rds", "")
#Rename all elements of the list
names(all_data) <- file_names


#Select coordinates from the list of lists----
data <- list()

for (i in 1:length(file_names)) {
  
  int_list <- all_data[[file_names[i]]]$InteractionData
  data[[i]] <- bind_rows(map(int_list, function(x) x %>% #map here is the same as a lapply
                dplyr::select(Longitude, Latitude)),  .id = 'Network_id')
}




#Rename list and 
names(data) <- file_names

#Prepare data to save it
coord_list = data
coord_long_format = bind_rows(data,  .id = 'Study_id')

save(coord_list, coord_long_format, file = "Data/Processing/Coordinates.RData")
save(coord_list, coord_long_format, file = "~/R_projects/SafeNet/Data/Coordinates.RData")

load("Data/Processing/Coordinates.RData")

#Prepare data to plot it
data = bind_rows(data,  .id = 'Study_id')
data_coord <- st_as_sf(data, coords = c(3:4)) %>% 
st_set_crs(4326) %>% 
st_transform(3035)

data_coord$Study_id <- factor(data_coord$Study_id, levels = file_names)
#Save data
st_write(data_coord, "Data/Processing/data_coord.shp")
st_write(data_coord, "~/R_projects/SafeNet/Data/data_coord.shp")

# Year
year_ref <- 2016

# Get all countries and transform to the same CRS
cntries <- gisco_get_countries(year = year_ref,
        resolution = 03) %>%
        st_transform(3035)
#Save data
st_write(cntries, "Data/Processing/cntries.shp")
st_write(cntries, "~/R_projects/SafeNet/Data/cntries.shp")

set.seed(1)

colors <- c("#8AD69E", "#74C5CE",  "#FDE9DE", "#45A631", "#95F7F0", "#CB53E1",
            "#FE90BB", "#B187E1", "#F8A982", "#0075DC", 
            "#993F00",  "#5EF1F2", "#005C31", "#74C5CE", "black", "lightblue",  "darkorange3","#8F7C00",
            "#8F7C00","#8F7C00", "#9DCC00", "#C20088", "gold1","#FFA8BB","#426600","94FFB5","#003380",  
            "#FF0010","#FF0010", "salmon", "seashell3","greenyellow","mediumaquamarine")
#To generate more random colors 
#randomcoloR::distinctColorPalette(k = length(file_names), altCol = F, runTsne = T)

# Plot
ggplot() +
geom_sf(data = cntries, fill = "grey80", color = "black") +
xlim(c(2200000, 7150000)) +
ylim(c(1380000, 5500000)) +
theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
        size = 0.5), panel.background = element_rect(fill = "aliceblue"),
panel.border = element_rect(colour = "black", fill=NA, size=1)) +
geom_sf(data = data_coord, aes(color=Study_id), size = 1.5, stroke = 0, shape = 16) +
theme(legend.position="none") + 
scale_colour_manual(name = "Study \n locations", values = colors) 

