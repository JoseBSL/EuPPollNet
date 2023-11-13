#DATASET NUMBER 47; Benadi
#Gita is no longer working in academia
#and gave us permission to use the data from Dryad
#Therefore, requires a bit more of processing

#Note: when merging flowe_counts with interactions
#use the comment col.

source("Scripts/Empty_templates.R") 

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)
library(parzer) #For transforming DMS coordinates
library(lubridate) #to sum time

#Load function to unify structure of data
source("Scripts/Change_str.R")

#Prepare interaction data ----
flower_visits <- read_csv("Data/Raw_data/47_Benadi/Alldata_Benadi_JAE_2013/Flower_visits.csv", locale = locale(encoding = "latin1"))
weather <- read_csv("Data/Raw_data/47_Benadi/Alldata_Benadi_JAE_2013/Weather.csv", locale = locale(encoding = "latin1")) %>% 
rename(Network_no = Network) %>% 
rename(Temperature = Temp_C) %>% 
rename(Humidity = Humidity_Percent) %>% 
select(!c(Time, Network_no, Plot, Observer))
networks <- read_csv("Data/Raw_data/47_Benadi/Alldata_Benadi_JAE_2013/Networks.csv", locale = locale(encoding = "latin1")) %>% 
rename(Network_no = Network) %>% 
rename(Elevation = Altitude_m) %>% 
select(!Observer)

#Prepare dates
flower_visits = flower_visits %>% mutate(Year = lubridate::year(Date), 
        Month = lubridate::month(Date), 
        Day = lubridate::day(Date)) %>% 
select(!c(Date, Time)) %>% 
rename(Pollinator_species = Insect_species) %>% 
mutate(Country = "Germany") %>% 
mutate(Habitat = "Grassland") %>% 
rename(Site_id = Plot)

#Add weather data by sample ID
data = left_join(flower_visits, weather, 
          by = c("SampleID")) 

#Add elevation data
data = left_join(data, networks, by = "Network_no") %>% 
select(!c(SampleID, Date)) %>% 
mutate(Interaction = 1) %>% 
mutate(Sampling_method = "Transect") %>% 
mutate(Locality = "Berchtesgaden National Park") %>% 
mutate(Coordinate_precision = NA) %>% 
mutate(Comments = Network_no) %>% 
mutate(Sampling_effort_minutes = NA) %>% #total time
mutate(Sampling_area_square_meters = 30 * 4 * 5 * 6) %>% #total area
select(!c(Observation_time, Plot, Observer, Network_no))

#Add coordinates
data = data %>% 
mutate(Latitude = case_when(Site_id == 1 ~ parse_lat("47°34.297’ N"),
                           Site_id == 2 ~ parse_lat("47°33.174’ N"),
                           Site_id == 3 ~ parse_lat("47°32.175’ N"),
                           Site_id == 4 ~ parse_lat("47°31.626’ N"),
                           Site_id == 5 ~ parse_lat("47°31.571’ N"),
                           Site_id == 6 ~ parse_lat("47°31.649’ N"))) %>% 
mutate(Longitude = case_when(Site_id == 1 ~ parse_lon("12°53.798’ E"),
                           Site_id == 2 ~ parse_lon("12°53.050’ E"),
                           Site_id == 3 ~ parse_lon("12°53.183’ E"),
                           Site_id == 4 ~ parse_lon("12°54.470’ E"),
                           Site_id == 5 ~ parse_lon("12°54.869’ E"),
                           Site_id == 6 ~ parse_lon("12°55.112’ E")))

#Compare vars
#compare_variables(check_interaction_data, data)
#Nothing missing!

#Reorder variables
data = drop_variables(check_interaction_data, data) 

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data, data$Site_id)

#Prepare flower count data ---- 
flower_count <- read_csv("Data/Raw_data/47_Benadi/Alldata_Benadi_JAE_2013/Flower_areas.csv", locale = locale(encoding = "latin1"))

flower_count = flower_count %>% 
rename(Site_id = Plot) %>% 
rename(Comment = Network) %>% #This col will be useful for merging
rename(Flower_count = Area_per_m2) %>% 
mutate(Units = "Area per m2")

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#Misisng vars
#add them
#Order data as template and drop variables
flower_count = add_missing_variables(check_flower_count_data, flower_count) 
#Order cols
flower_count = drop_variables(check_flower_count_data, flower_count) 
#Split interaction data into dataframes within a list
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
  Doi = "https://doi.org/10.1111/1365-2656.12158",
  Dataset_description = "Plant-pollinator community from
  National Park Berchtesgaden in the German part of the Alps. Data collection
  took place during 2010.",
  Taxa_recorded = "Bees, hoverflies and butterflies",
  Sampling_year = "2010",
  Country = "Germany",
  Habitat = "Grassland",
  Sampling_sites = "10",
  Sampling_rounds = NA_character_,
  Sampling_method = "Transect",
  Sampling_area_details = "5 quadrats of 2m2 per 5 transects per site",
  Sampling_area_species_m2 = 30 * 4 * 5,
  Sampling_area_total_m2 = 30 * 4 * 5 * 6,  
  Sampling_time_details = "5 to 7 hours per sampling day", 
  Sampling_time_species_round_min = NA, 
  Sampling_time_total_min = 46 * 60 * 6, #46 unique dates, times 6 hours in minutes 
  Total_plant_species = nrow(plant_single_cases),
  Total_pollinator_species = nrow(pollinator_single_cases),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
#Not included as G.Benadi did not accept the invitation
Authorship <- data.frame(
  Coauthor_name = c(""),
  Orcid = c(""),
  E_mail = c(""))

#Save data ----
#Create metadata list
Benadi <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Benadi) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Benadi, file="Data/Clean_data/47_Benadi.rds")

