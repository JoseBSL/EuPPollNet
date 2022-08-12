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
  mutate(Locality = paste0(Site, "_", Year))


#Drop na's in cols of plants and polls and zeros
data = data %>% 
  rename(Plant_species = Plant) %>% 
  rename(Pollinator_species = Animal) %>% 
  drop_na(Plant_species, Pollinator_species) %>% 
  filter(!Pollinator_species == 0 | !Pollinator_species ==0) %>% 
  rename(Site_id = `Plot ID`)

#Split dates into 3 cols
library(lubridate)
data = data %>% 
  mutate(Date = dmy(Date)) %>% 
  mutate(Year = lubridate::year(Date), 
         Month = lubridate::month(Date), 
         Day = lubridate::day(Date)) 


#Group interactions by plot
data = data %>% 
  group_by(Plant_species, Pollinator_species, Year, Site_id, Locality) %>% 
  summarise(Interaction = n())

#Add other columns
data = data %>% 
  mutate(Day = NA) %>% 
  mutate(Month = NA) %>% 
  mutate(Year = as.character(Year)) %>% 
  mutate(Latitude = case_when(Locality == "Isen_Bjerg" ~ 56.0725, 
                              Locality == "Skov_Olesen"  ~ 56.104167,
                              Locality == "Horbylunde" ~ 56.141667)) %>% 
  mutate(Longitude = case_when(Locality == "Isen_Bjerg" ~ 9.275556, 
                               Locality == "Skov_Olesen"  ~ 9.107778,
                               Locality == "Horbylunde" ~ 9.39)) %>% 
  mutate(Country = "Denmark") %>% 
  mutate(Sampling_method = "Nested plots") %>% 
  mutate(Sampling_effort_minutes = 15) %>% 
  mutate(Sampling_area_square_meters = 1) %>% 
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


#write_csv(data, "Data/Raw_data/12_Dupont/int_data.csv")

glimpse(data)

#Split by site, just for createing the listed name in this case
InteractionData <- split(data, data$Locality)

#Prepare flower count data ----
FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = NA, Plant_species = NA,
                     Flower_count = NA, Units = NA, Comment = NA)

#Prepare metadata data ----
Metadata <- tibble(
  Doi = NA,
  Dataset_description = "This dataset includes plant-flower visitor observations throughout
  the flowering season (April-October) at three sites in peninsular Jutland in Denmark.
  Two sites were observed in one year (SO in 2004 and HL in 2005), one site (IB)
  was observed during two years (2004-2005).
  Networks were observed on (nearly) all days of favorable weather
  (no rain and no strong winds). The habitat is dry heathland.",
  Taxa_recorded = "All flower visitors")

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Yoko L. Dupont", "Jens M. Olesen"),
  Orcid = c("0000-0002-8811-2773", "0000-0003-1998-1083"),
  E_mail = c("yoko.dupont@ecos.au.dk", "jens.olesen@bio.au.dk"))

#Save data ----
#Create list with all dataframes of interest
Dupont <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Dupont) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
#The prefix number depends on the total number of datasets
#This is the dataset number 14
saveRDS(Dupont, file="Data/Clean_data/14_Dupont.RData") 



