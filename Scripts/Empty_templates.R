#####################################################################################--
#Source code to process raw data
#####################################################################################--
#Set up----
library(cleanR)

#First create TEMPLATE WITH ALL COLNAMES
#Create an empty data file. 
data = data.frame(Plant_species = NA, Pollinator_species = NA, Interaction =  NA, Sampling_method = NA,
                    Sampling_effort_minutes = NA, Sampling_area_square_meters = NA, Site_id = NA,
                    Habitat = NA, Country = NA, Locality = NA, Latitude = NA, Longitude = NA, 
                    Coordinate_precision = NA, Elevation = NA, Day = NA, Month = NA, Year = NA, 
                    Comments = NA, Temperature = NA, Humidity = NA)

#write.csv(data, "Data/Processing/Interaction_data_empty_template.csv", row.names = FALSE)
#read data.csv for comparisons
data <- read.csv("Data/Processing/Interaction_data_empty_template.csv", stringsAsFactors=TRUE)
#Set the dataframe to compare with
check_interaction_data <- define_template(data, NA)
