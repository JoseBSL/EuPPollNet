#####################################################################################--
#Source code to process raw data
#####################################################################################--
#Set up----
library(cleanR)

#First create interaction data TEMPLATE WITH ALL COLNAMES
#Create an empty data file. 
data = data.frame(Plant_species = NA, Pollinator_species = NA, Interaction =  NA, Sampling_method = NA,
                    Sampling_effort_minutes = NA, Sampling_area_square_meters = NA, Site_id = NA,
                    Habitat = NA, Country = NA, Locality = NA, Latitude = NA, Longitude = NA, 
                    Coordinate_precision = NA, Elevation = NA, Day = NA, Month = NA, Year = NA, 
                    Comments = NA, Temperature = NA, Humidity = NA, Flower_data = NA, Flower_data_merger = NA)

#write.csv(data, "Data/Working_files/Interaction_data_empty_template.csv", row.names = FALSE)
#read data.csv for comparisons
data <- read.csv("Data/Working_files/Interaction_data_empty_template.csv", stringsAsFactors=TRUE)
#Set the dataframe to compare with
check_interaction_data <- define_template(data, NA)


#Second create flower count template
FlowerCount = data.frame(Day = NA, Month = NA, Year = NA, Site_id = NA, Plant_species = NA,
                     Flower_count = NA, Units = NA, Comments = NA, Flower_data_merger = NA)

#Save data
#write.csv(FlowerCount, "Data/Working_files/Flower_count_empty_template.csv", row.names = FALSE)
FlowerCount = read.csv("Data/Working_files/Flower_count_empty_template.csv", stringsAsFactors=TRUE)

check_flower_count_data <- define_template(FlowerCount, NA)

