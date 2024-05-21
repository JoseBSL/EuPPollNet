#Prepare data for Demetra
library(dplyr)
#Read data
data = readRDS("Data/3_Final_data/Interaction_data.rds")
#Filter based on Demetra's criteria
bombus_germany = data %>% 
filter(Country == "Germany" & Pollinator_genus == "Bombus")
#save rdata
saveRDS(bombus_germany, "Data/Working_files/bombus_germany.rds")
