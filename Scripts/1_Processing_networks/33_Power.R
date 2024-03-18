#DATASET NUMBER 33; Power
#ALL datasets were gathered by Laura Russo who organised this
#(There are 6 in total)
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
mutate(Comments = NA) %>% 
rename(Plant_species = Plant_Species) 

#Recode authors to just their surname
data = data %>% 
mutate(Name = recode_factor(Name,  "AoifeO" = "O’Rourke",
                            "EileenPower" = "Power",
                            "DaraStanley" = "Stanley",
                            "SarahMullen" = "Mullen",
                            "MichelleLarkin" = "Larkin",
                            "CianWhite" = "White"))

#Select first the one of Power
data = data %>% filter(Name == "Power")

#To calculate sampling effort later on
data_time = data %>% select(Sampling_effort_minutes)

#Compare vars
#compare_variables(check_interaction_data, data)

#Delete underscore from plant species
data = data %>% 
mutate(Plant_species = str_replace(Plant_species, "_", " "))

#Create merger col
#Note that we have calculated the average of flowers per site
#Was the only way to merge both datasets correctly... (ask authors for further info)
data = data %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1), "_",
                                   word(Plant_species,2), "_", Site,"_", Site_number))


#Prepare data
data = data %>% 
mutate(Habitat = "Farming grassland") %>% 
mutate(Locality = "Along south-eastern Ireland") %>% 
rename(Latitude = "WGS84/ETRS89") %>% 
rename(Longitude = "WGS84/ETRS89_2") %>% 
mutate(Site_id = Site) %>% 
mutate(Sampling_method = "Transects") 


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
mutate(Longitude = gsub("\\'.", "' ", Longitude)) 
#Convert to decimal lat/long coordinates
coord = parzer::parse_lon_lat(lon = data$Longitude, lat = data$Latitude)

#Add correct coord col to the interaction dataset
data = data %>% 
mutate(Longitude = coord$lon) %>% 
mutate(Latitude = coord$lat)

#Finally drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Try to fix days
data = data %>% 
mutate(Day = as.Date(Day, origin = "2009-01-01")) %>% 
mutate(Day = format(as.Date(Day,format="%Y-%m-%d"), format = "%d"))
#it seems ok


#Unify level
data = data %>% 
mutate(Sampling_method = "Transect")

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)

#Prepare flower count data ---- The data wasn't collected
FlowerCount = read_csv("Data/1_Raw_data/32_to_37_Russo/Flower_count.csv")

#Divide col into cols
levels(factor(FlowerCount$Site_id))
FlowerCount = FlowerCount %>% separate(Site_id, c("Name", "Site", "Site_number"), remove = F) %>% 
mutate(Day = NA) %>% 
mutate(Month = NA)

#Fix plant names
FlowerCount = FlowerCount %>%
mutate(Plant_species =  str_replace(Plant_species, "_", " "))

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
filter(Name == "Power") %>% 
mutate(Site_id = Site) %>% 
mutate(Comments = "Also available FloralArea in m2") %>% 
mutate(Flower_data_merger = NA) %>% 
mutate(Units = "Mean abundance per plant species")

#Create unique identifier
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(word(Plant_species,1), "_",
                                   word(Plant_species,2), "_",
                                   Site, "_", Site_number))


#Check vars
#compare_variables(check_flower_count_data, flower_count)
#Order data as template
FlowerCount = drop_variables(check_flower_count_data, FlowerCount) 

#Calculate average flowers per site 
FlowerCount = FlowerCount %>%
group_by_at(vars(-c(Flower_count))) %>%
summarise(Flower_count = mean(Flower_count))

#Set common structure
FlowerCount = change_str2(FlowerCount)

#Split interaction data into dataframes within a list
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata = tibble(
Doi = "https://doi.org/10.1111/j.1365-2664.2010.01949.x",
Dataset_description = "Two fields from different farms were surveyed in 2009",
Taxa_recorded = "Bees, hoverflies and butterflies",
Sampling_year = "2009",
Country = "Ireland",
Habitat = "Farming grassland",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "3",
Sampling_method = "Transects",
Sampling_area_details = "Transects 100 x 2m transect",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = "800 * 10", #800 m2 per farm and 10 farms
Sampling_time_details = "10–15 min per transect",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = 12.5 * 10 * 2 * 3, #Average time per transect * 10 farms * 2 sites per farm * 3 sampling rounds
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
rownames_to_column() %>% 
rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Eileen Power", "Jane C. Stout"),
  Orcid = c("0000-0002-5941-4676", "0000-0002-2027-0863"),
  E_mail = c("eipower@tcd.ie", "stoutj@tcd.ie"))


#Save data ----
#Create metadata list
Power <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Power) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Power, file="Data/2_Processed_data/33_Power.rds")

