#DATASET NUMBER 18; Bartomeus 

#Load libraries
library(tidyverse)
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/18_Bartomeus/Interaction_data.csv")

#Quick clean of the interaction data
#We select just a single year (2015), the other data is still being used.
data = data %>% 
filter(Year == "2015" & Out == "transect") %>% 
select(Plant_gen_sp, Pollinator_gen_sp, Frequency, Out,
       Site_ID, Day, Month, Year, Temperature, Humidity) %>% 
rename(Plant_species = Plant_gen_sp, Pollinator_species = Pollinator_gen_sp,
       Interaction = Frequency, Sampling_method = Out, Site_id = Site_ID) %>% 
mutate(Sampling_effort_minutes = "30", .after = Sampling_method) %>% 
mutate(Sampling_area_square_meters = "100", .after = Sampling_effort_minutes) %>% 
mutate(Habitat = "open pine woodlands", .after = Site_id) %>% 
mutate(Country = "Spain", .after = Habitat) %>% 
mutate(Locality = Site_id, .after = Country) %>% 
mutate(Locality = Site_id, .after = Country) %>% 
mutate(Latitude = NA, .after = Locality) %>% 
mutate(Longitude = NA, .after = Latitude) %>% 
mutate(Coordinate_precision = NA, .after = Longitude) %>% 
mutate(Elevation = NA, .after = Coordinate_precision) %>% 
mutate(Comments = NA, .after = Year) %>% 
mutate(Site_id = str_replace_all(Site_id, "_", "")) %>% 
mutate(Sampling_method = "Transects") %>% 
select(!c(Sampling_effort_minutes, Sampling_area_square_meters)) #Including this info in the metadata


#Add coordinates
sites <- data.frame(Site_id = c("Aznalcazar",
     "Villamanriquesur",
     "Villamanriqueeste",	
     "Lasmulas",	
     "Pinaresdehinojos",	
     "Cotitodesantateresa",	
     "Esparragal",	
     "Lacunya",	
     "Larocina",	
     "Pinodelcuervo",	
     "Elpozo",	
     "Bonares",	
     "Niebla",	
     "Conventodelaluz",	
     "Urbanizaciones",	
     "Elpinar"),	 
    Latitude1 = c(37.234966, 	37.217810, 37.239190, 37.214279, 37.289861, 37.247223, 37.282079,
                 37.233157, 37.123856, 37.194957, 37.249254, 37.331447,
                 37.405047, 37.291937, 37.288346, 37.291091),
    Longitude1 = c(-6.168950, -6.304244, -6.280883, -6.371844, -6.433110, -6.428801,
                  -6.555233, -6.555236, -6.506789, -6.761136, -6.660927,
                  -6.667992,  -6.680852, -6.752456, -6.675026, -6.222871))

#Add coordinates
data <- dplyr::inner_join(data, sites, by="Site_id")

#Convert coordinates cols
data = data %>% 
mutate(Latitude = Latitude1) %>% 
mutate(Longitude = Longitude1) %>% 
select(!c(Latitude1, Longitude1))

#Unify level
data = data %>% 
mutate(Sampling_method = "Transect")

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData <- split(data,data$Site_id)

#Prepare flower count data ----
flower_count <- read_csv("Data/Raw_data/18_Bartomeus/Flower_count.csv")

#Order cols
flower_count = flower_count %>% 
mutate(Units = "Flower_units") %>% 
mutate(Comment = "10 counts along the transect") %>% 
select(Day, Month, Year, Site_ID, Plant_gen_sp, Flower_abundance, Units, Comment) %>% 
rename(Site_id = Site_ID, Plant_species = Plant_gen_sp, 
       Flower_count = Flower_abundance) %>% 
mutate(Site_id = str_replace_all(Site_id, "_", "")) %>% 
filter(!Site_id == "Elhongo" & !Site_id =="Lagunadelojillo") #These ids do not match

#Split interaction data into dataframes within a list
FlowerCount <- split(flower_count, flower_count$Site_id)

#Prepare metadata data ----
#Select unique cases of plants and poll
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)


#Create ordered metadata
Metadata <- tibble(
Doi = "https://doi.org/10.24072/pcjournal.1",
Dataset_description = "Data collection is done in 12 to 16 independent
sites in a gradient of land use intensity across the Guadalquivir valley.
All sites are sampled every ~ 15 days (weather permitting) for a total of 6
to 8 rounds per year. All sites are situated in open pine woodlands with an understory dominated by Lavender, Rosemary and Cistaceas. Data collection covers the whole  season from early spring (February) to Summer (June). We only sample on good weather conditions (no rain, extreme cold or high wind speeds).",
Taxa_recorded = "All floral visitors",
Sampling_year = "2015",
Country = "Spain",
Habitat = "Open pine woodlands",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = 8,
Sampling_method = "Transect",
Sampling_area_details = "100 m transect",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = 100 * 16, #100 m long * 1 m wide *16 sites
Sampling_time_details = "30 mins each transect",
Sampling_time_species_round_min = NA,
Sampling_time_total_min = 16 * 30 * 1 * 8, #16 sites * 30 mins * 1 transect * 8 rounds
Total_plant_species = nrow(plant_single_cases),
Total_pollinator_species = nrow(pollinator_single_cases),
Floral_counts =  "Yes")


#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()


#Prepare authorship data ----
Authorship <- tibble(
  Coauthor_name = c("Francisco P. Molina", "Ignasi Bartomeus"),
  Orcid = c("0000-0003-2425-5246",  "0000-0001-7893-4389"),
  E_mail = c("fpaula.molina@gmail.com", "nacho.bartomeus@gmail.com"))

#Save data ----
#Create metadata list
Bartomeus <- list(InteractionData, FlowerCount, Metadata, Authorship)
names(Bartomeus) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")

#Save data
saveRDS(Bartomeus, file="Data/Clean_data/18_Bartomeus.rds")
