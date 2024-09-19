library(sf)
library(dplyr)


#Read data
data = readRDS("Data/Working_files/Interactions_uncounted.rds")

levels(factor(data$EuPPollNet_habitat))

data = data %>% 
filter(EuPPollNet_habitat =="Moors and heathland")

#Generate a tibble with coordinates
coordinates = data %>% 
select(Longitude, Latitude) %>% 
distinct()

#Read data
natura_shapefile <- st_read("natura_2000/SHP/Natura2000_end2022_epsg3035.shp")
st_crs(natura_shapefile)  # Check the CRS

# Transform to match shapefile CRS
coordinates_sf <- st_as_sf(coordinates, coords = c("Longitude", "Latitude"), crs = 4326)
coordinates_sf_transformed <- st_transform(coordinates_sf, crs = st_crs(natura_shapefile))
# Spatial join to find matching habitats
habitats_at_coordinates <- st_join(coordinates_sf_transformed, natura_shapefile)

