#DATASET NUMBER 37; White
#ALL datasets from RUSSO (There are 6 in total)
#One is an unpublished thesis

#Read empty templates to compare with
source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read_csv("Data/1_Raw_data/32_to_37_Russo/Interaction_data.csv")

#Divide col into cols
levels(factor(data$Site_id))
data = data %>% separate(Site_id, c("Name", "Site", "Site_number"), remove = F)
levels(factor(data$Name))

#Add flower info cols
data = data %>% 
mutate(Flower_data_merger = NA) %>% 
mutate(Flower_data = "Yes") %>% 
mutate(Comments = NA)

#Recode authors to just their surname
data = data %>% 
mutate(Name = recode_factor(Name,  "AoifeO" = "O’Rourke",
                              "EileenPower" = "Power",
                              "DaraStanley" = "Stanley",
                              "SarahMullen" = "Mullen",
                              "MichelleLarkin" = "Larkin",
                              "CianWhite" = "White"))

#Select first the one of Power
data = data %>% filter(Name == "Cian")

#To calculate sampling effort later on
data_time = data %>% select(Sampling_effort_minutes)

#Compare vars
#compare_variables(check_interaction_data, data)

#Prepare data
data = data %>% 
#rename(Plant_species = Plant_Species) %>% 
mutate(Habitat = "Agricultrual and urban sites with different intensity") %>% 
mutate(Locality = "East Leinster (Ireland)") %>% 
rename(Latitude = "WGS84/ETRS89") %>% 
rename(Longitude = "WGS84/ETRS89_2") %>% 
#mutate(Site_id = Site) %>% 
mutate(Sampling_method = "Transects") 

#Delete underscore from plant species
data = data %>% 
mutate(Plant_species = str_replace(Plant_species, "[.]", " ")) %>% 
mutate(Pollinator_species = str_replace_all(Pollinator_species, "[.]", " "))

#Add missing vars
data = add_missing_variables(check_interaction_data, data) 
#Reorder variables
data = drop_variables(check_interaction_data, data) 

#Convert degree minute second to decimal degree coordinates
library(parzer)

#Prepare data before convert coordinates
data = data %>% 
  mutate(Latitude = gsub("\\°.", "° ", Latitude)) %>% 
  mutate(Latitude = gsub("\\'.", "' ", Latitude)) %>% 
  mutate(Longitude = gsub("\\°.", "° ", Longitude)) %>% 
  mutate(Longitude = gsub("\\'.", "' ", Longitude)) %>% 
  mutate(Longitude = gsub("W", "", Longitude, fixed = T)) %>% 
  mutate(Latitude = gsub("N", "", Latitude, fixed = T)) %>% 
  mutate(Latitude = paste0(Latitude, "N")) %>% 
  mutate(Longitude = paste0(Longitude, "W"))

#Convert to decimal lat/long coordinates
coord = parzer::parse_lon_lat(lon = data$Longitude, lat = data$Latitude)

#Add correct coord col to the interaction dataset
data = data %>% 
  mutate(Longitude = coord$lon) %>% 
  mutate(Latitude = coord$lat)

#Finally drop sampling effort and square area (it would be added on the metadata)
data = data %>%
  select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Unify level
data = data %>% 
mutate(Sampling_method = "Transect")

#There are cases with 2 coordinates for unqie Site_id
#Select only a single value
coords = data %>% 
group_by(Site_id) %>%   
summarise(FirstLatitude = first(Latitude),
          FirstLongitude = first(Longitude))
#Conduct left join and add those columns
data = left_join(data, coords) %>% 
mutate(Latitude = FirstLatitude) %>% 
mutate(Longitude = FirstLongitude) %>% 
select(!c(FirstLatitude, FirstLongitude))

#Fix Stie id column
data = data %>% 
mutate(Site_id = str_replace(Site_id,	"Cian.White_", ""))

#Create merger col
#Note that we have calculated the average of flowers per site
#Was the only way to merge both datasets correctly... (ask authors for further info)
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1), "_",
                                   word(Plant_species,2), "_", Site_id))


#Drop interactions with NA
data = data %>% 
filter(!is.na(Interaction))

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)

#Prepare flower count data ---- The data wasn't collected
FlowerCount = read_csv("Data/1_Raw_data/32_to_37_Russo/Flower_count.csv")

#Divide col into cols
levels(factor(FlowerCount$Site_id))
FlowerCount = FlowerCount %>% separate(Site_id, c("Name", "Site", "Site_number"), remove = F)

#Fix plant names
FlowerCount = FlowerCount %>%
mutate(Plant_species =  str_replace(Plant_species, "[.]", " "))

#Recode authors to just their surname
FlowerCount = FlowerCount %>% 
  mutate(Name = recode_factor(Name,  "AoifeO" = "O’Rourke",
                              "EileenPower" = "Power",
                              "DaraStanley" = "Stanley",
                              "SarahMullen" = "Mullen",
                              "MichelleLarkin" = "Larkin",
                              "CianWhite" = "White"))

#Select first the one of Power
FlowerCount = FlowerCount %>% 
filter(Name == "Cian") %>% 
#mutate(Site_id = Site) %>% 
mutate(Comments = "Mean flower number/species-site")%>% 
mutate(Units = "Mean abundance per plant species") %>% 
mutate(Site_id = str_replace(Site_id, "Cian.White_", ""))

#Create unique identifier
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1), "_",
                                   word(Plant_species,2), "_", Site_id))

#Check vars
#compare_variables(check_flower_count_data, flower_count)
#Order data as template
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#Filter out Not collected -couple of cases that create issues for the mean-
FlowerCount = FlowerCount %>% 
filter(!Units == "not collected")

##Calculate average flowers per site 
FlowerCount = FlowerCount %>%
group_by_at(vars(-c(Flower_count, Day, Month, Year))) %>%
summarise(Flower_count = mean(Flower_count)) %>% 
mutate(Day = NA) %>% 
mutate(Month = NA) %>% 
mutate(Year = NA) 

#Set common structure
FlowerCount = change_str2(FlowerCount)

#Split interaction data into dataframes within a list
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
  Doi = "https://doi.org/10.3389/fevo.2022.806615",
  Dataset_description = "Plant-pollinator communties from
  21 sites on agricultural and urbanised landscapes with different
  intensification",
  Taxa_recorded = "Bees, hoverflies and butterflies",
  Sampling_year = "2018",
  Country = "Ireland",
  Habitat = "Urban and agricultural landscapes",
  Sampling_sites = nlevels(factor(data$Site_id)),
  Sampling_rounds = "4",
  Sampling_method = "Transects",
  Sampling_area_details = "4 transects of 100 x 2 m per site",
  Sampling_area_species_m2 = NA,
  Sampling_area_total_m2 = 21 * 200, #21 sites * transect area
  Sampling_time_details = "", 
  Sampling_time_species_round_min = NA, 
  Sampling_time_total_min = NA, #time *sites * transects/site * rounds
  Total_plant_species = nrow(plant_single_cases),
  Total_pollinator_species = nrow(pollinator_single_cases),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Cian White", "Jane C. Stout"),
  Orcid = c("0000-0002-7126-1916", "0000-0002-2027-0863"),
  E_mail = c("ciwhite@tcd.ie", "stoutj@tcd.ie"))

#Save data ----
#Create metadata list
White <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(White) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(White, file="Data/2_Processed_data/37_White.rds")

