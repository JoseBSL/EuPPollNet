#DATASET NUMBER 14; Dupont
#Floral info not added yet (review)

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
#Load interaction data
data = read_csv("Data/1_Raw_data/14_Dupont/Interaction_data.csv") %>% 
  select(!...7)

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
  group_by(Plant_species, Pollinator_species, Year,Month, Day, Site_id, Locality) %>% 
  summarise(Interaction = n()) %>%  ungroup()

#Add other columns
data = data %>% 
mutate(Latitude = case_when(Locality == "Isen_Bjerg_2004" ~ 56.0725, 
                              Locality == "Isen_Bjerg_2005" ~ 56.0725, 
                              Locality == "Skov_Olesen_2004"  ~ 56.104167,
                              Locality == "Horbylunde_2005" ~ 56.141667)) %>% 
mutate(Longitude = case_when(Locality == "Isen_Bjerg_2004" ~ 9.275556, 
                               Locality == "Isen_Bjerg_2005" ~ 9.275556, 
                               Locality == "Skov_Olesen_2004"  ~ 9.107778,
                               Locality == "Horbylunde_2005" ~ 9.39)) %>% 
mutate(Country = "Denmark") %>% 
mutate(Sampling_method = "Plot") %>% 
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
         Temperature, Humidity) %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata

#Unify structure of data
data = change_str(data)

#Split by site, just for createing the listed name in this case
InteractionData <- split(data, data$Locality)

#Prepare flower count data ----
#We need to create a list of lists too
#Check levels of Site_id
site_id_levels = levels(factor(data$Site_id))

FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = site_id_levels, Plant_species = NA,
                     Flower_count = NA, Units = NA, Comment = NA)

FlowerCount = FlowerCount %>% 
mutate(Day = as.character(Day)) %>% 
mutate(Month = as.character(Month)) %>% 
mutate(Year = as.numeric(Year)) %>% 
mutate(Site_id = as.character(Site_id)) %>% 
mutate(Plant_species = as.character(Plant_species)) %>% 
mutate(Flower_count = as.numeric(Flower_count)) %>% 
mutate(Units = as.character(Units)) %>% 
mutate(Comment = as.character(Comment))

#Split by Site_id
FlowerCount <- split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----

#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

Metadata <- tibble(
Doi = "https://doi.org/10.1111/j.1365-2656.2008.01501.x",
Dataset_description = "This dataset includes plant-flower visitor observations throughout
the flowering season (April-October) at three sites in peninsular Jutland in Denmark.
Two sites were observed in one year (SO in 2004 and HL in 2005), one site (IB)
was observed during two years (2004-2005).
Networks were observed on (nearly) all days of favorable weather
(no rain and no strong winds). The habitat is dry heathland.",
Taxa_recorded = "All flower visitors",
Sampling_year = "2004 and 2005",
Country = "Denmark",
Habitat = "Heathland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "Full flowering season",
Sampling_method = "Plot",
Sampling_area_details = NA,
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = NA,
Sampling_time_details = NA,
Sampling_time_species_round_min = 15,
Sampling_time_total_min = NA,
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "No")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

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
saveRDS(Dupont, file="Data/2_Processed_data/14_Dupont.rds") 



