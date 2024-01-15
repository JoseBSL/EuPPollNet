#Extract biogeographical regions based on coordinates
# I have followed these posts (one is mine, the downvoted one :)
#https://stackoverflow.com/questions/74723290/extract-biogeographical-genions-based-on-coodinates
#https://stackoverflow.com/questions/77815238/speed-up-extraction-of-biogeographical-regions-from-shapefile-based-on-coordinat
#Load libraries
library(sf)
library(dplyr)

#Load data 
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")

##Read shapefile:
bioregions <- sf::read_sf('Data/Working_files/Bio_Regions/BiogeoRegions2016.shp')
bioregions <- st_transform(bioregions, crs = 4326) %>%  st_transform(3035)

#Generate a tibble with coordinates
coords = data %>% 
select(Study_id, Longitude, Latitude) %>% 
distinct() 

#Prepare points for extraction
pt <- 
data.frame(x = coords$Longitude, y = coords$Latitude, id = 1:nrow(coords)) |>
## add geometry column:
st_as_sf(coords = c('x', 'y')) |>
## set CRS:
st_set_crs(4326) %>% 
st_transform(3035)

#Spatial join, by default a left join: 
#All point are returned, those with no polygon match will have NA values
result <- st_join(pt, bioregions)
#Bind cols
coords_result = bind_cols(coords, result) 

#Select main cols
#Check for NA's and fix 
#Those are coordinates located on the Ocean
coords_result = coords_result %>% 
select(Study_id, Longitude, Latitude, code)

#Filter NA's
coords_result %>% 
filter(is.na(code))
#Fix manually here, just 5 coordinates
coords_result = coords_result %>% 
mutate(code = case_when(
       Study_id == "34_Stanley" ~ "Atlantic",
       Study_id == "37_White" ~ "Atlantic",
       Study_id == "50_Hervias-Parejo" ~ "Mediterranean",
        TRUE ~ code))  

#Rename columns and save data
coords_result = coords_result %>%
rename(Bioregions = code)

colnames(coords_result)

coords_result %>% 
distinct(Bioregions)

bioregions$code
