

library(elevatr)
library(dplyr)

#Read interaction data
data = readRDS("Data/Working_files/Interactions_uncounted.rds")%>% 
mutate(Network_id = paste0(Study_id, Network_id))  

coords = data %>% 
select(Longitude, Latitude) %>% 
distinct() %>% 
rename(x = Longitude) %>% 
rename(y = Latitude) %>% 
as.data.frame()

elevation = get_elev_point(coords, prj = crs_dd, src = "aws")
saveRDS(elevation, "Data/Working_files/Elevation.rds")
