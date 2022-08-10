#DATASET NUMBER 14; Dupont

#Load libraries
library(tidyverse)


#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/Raw_data/12_Dupont/interaction_data_dupont.csv") %>% 
select(!X7)

#Rename two labels
data = data %>% 
mutate(Site = recode_factor(Site,
                            "Isenbjerg" = "Isen_Bjerg",
                            "Isen Bjerg" = "Isen_Bjerg",
                            "Skov Olesen" = "Skov_Olesen"))




#To separate sites by year paste year to Site_id
data = data %>% 
mutate(Site_id = paste0(`Plot ID`, "_", Year))


#Check number of plots on years 2014 and 2015 and multiplied by 15, 
#This woukld be the total number of mins
time_locations_2004 <- data %>% 
group_by(Site, Site_id, Year) %>% 
summarise(Interaction = n()) %>% 
filter(Year == 2004) %>% 
ungroup() %>% 
group_by(Site) %>% 
summarise(Time = n()*15) #15 mins per site

time_locations_2005 <- data %>% 
  group_by(Site, Site_id, Year) %>% 
  summarise(Interaction = n()) %>% 
  filter(Year == 2005) %>% 
  ungroup() %>% 
  group_by(Site) %>% 
  summarise(Time = n()*15) #15 mins per site

#Rename columns and select cols of interest
data = data %>% 
rename(Plant_species = Plant) %>% 
rename(Pollinator_species = Animal) %>% 
rename(Locality = Site) %>% 
select(!c(Year, `Plot ID`)) 

#Drop na's in cols of plants and polls and zeros
data = data %>% 
drop_na(Plant_species, Pollinator_species) %>% 
filter(!Pollinator_species == 0 | !Pollinator_species ==0) 

#Group interactions by plot
data = data %>% 
group_by(across()) %>% 
summarise(Interaction = n())  
  



#Split dates into 3 cols
library(lubridate)
data = data %>% 
mutate(Date = dmy(Date)) %>% 
mutate(Year = lubridate::year(Date), 
                Month = lubridate::month(Date), 
                Day = lubridate::day(Date)) %>% 
mutate(Latitude = case_when(Locality == "Isen_Bjerg" ~ 56.0725, 
                Locality == "Skov_Olesen"  ~ 56.104167,
                Locality == "Horbylunde" ~ 56.141667)) %>% 
mutate(Longitude = case_when(Locality == "Isen_Bjerg" ~ 9.275556, 
                Locality == "Skov_Olesen"  ~ 9.107778,
                Locality == "Horbylunde" ~ 9.39)) %>% 
mutate(Country = "Denmark") %>% 
mutate(Sampling_method = "Nested plots") %>% 
mutate(Sampling_effort_minutes = case_when(Locality == "Isen_Bjerg" & Year == 2004 ~ 3525,
                 Locality == "Skov_Olesen" & Year == 2004 ~ 3720,
                 Locality == "Isen_Bjerg" & Year == 2005 ~ 2985,
                 Locality == "Horbylunde" & Year == 2005 ~ 2625)) %>% 
mutate(Sampling_area_square_meters = 50000) %>% 
mutate(Habitat = "Heathland") %>% 
mutate(Coordinate_precision =  NA) %>% 
mutate(Elevation = NA) %>% 
mutate(Comments =  NA) %>% 
mutate(Temperature = NA) %>% 
mutate(Humidity = NA) %>% 
select(Plant_species, Pollinator_species, Interaction, Sampling_method,
                Sampling_effort_minutes, Sampling_area_square_meters,
                Site_id, Habitat, Country, Locality, Latitude, Longitude,
                Coordinate_precision, Elevation, Day, Month, Year, Comments,
                Temperature, Humidity)

#Prepare flower count data ----
