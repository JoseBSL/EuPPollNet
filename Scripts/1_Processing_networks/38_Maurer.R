#DATASET NUMBER 38; Maurer

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
data = read_csv("Data/1_Raw_data/38_Maurer/Interaction_data.csv")

#Add flower info cols
data = data %>% 
mutate(Flower_data_merger = NA) %>% 
mutate(Flower_data = "Yes") 

#There are some sites without coordinates
#I asked Corina and she provided those by mail
data = data %>% 
mutate(Latitude = case_when(Locality == "Frick" ~ 47.504475, T ~ Latitude)) %>% 
mutate(Longitude = case_when(Locality == "Frick" ~ 8.019589, T ~ Longitude)) %>% 
mutate(Latitude = case_when(Locality == "Windisch" ~ 47.477017, T ~ Latitude)) %>% 
mutate(Longitude = case_when(Locality == "Windisch" ~ 8.215765, T ~ Longitude)) %>%  
mutate(Latitude = case_when(Locality == "Bullach" ~ 47.514902, T ~ Latitude)) %>% 
mutate(Longitude = case_when(Locality == "Bullach" ~ 8.540593, T ~ Longitude)) %>% 
mutate(Latitude = case_when(Locality == "Winterthur" ~ 47.493949, T ~ Latitude)) %>% 
mutate(Longitude = case_when(Locality == "Winterthur" ~ 8.729096, T ~ Longitude)) 
#Recode one factor that seems to we wrong
data = data %>%
mutate(Locality = recode_factor(Locality, "Bullach" =  "Bulach"))

#Filter interactions equal to 0
data = data %>% filter(Interaction > 0)
#Rename sampling
data = data %>% mutate(Sampling_method = "Transect")
#Rename habitats
data = data %>% rename(Habitat = Focal_habitat) %>% 
mutate(Habitat = recode_factor(Habitat,
      "OSR" = "Oilseed rape crop",
      "forest.edge" = "Forest edge",
      "forest" = "Forest",
      "ext.meadow" = "Extensively managed meadow",
      "art.grass" = "Rotational meadow",
      "fallow" = "Fallow land", 
      "tree.nur" = "Tree nursery",
      "pasture" = "Pasture", 
      "hedge" = "Hedgerow",
      "orchard" = "Orchard",
      "roadside" = "Grassy/herb strip next to road",
      "lawn" = "Frequently mown",
      "perm.mead" = "Permanent meadow",
      "ext.mead" = "Extensively managed meadow",
      "garden" = "Private garden",
      "flow.fal" = "Sown perennial flowering strip",
      "bean" = "Bean crop",
      "potato" = "Potato",
      "sun.flow" = "Sunflower crop",
      "sug.beet" = "Sugar bet crop")) %>% 
mutate(Country = "Switzerland") 

#All levels seem ok now
levels(factor(data$Habitat))
#Add the general habitat to a comment
data = data %>% 
rename(Comments = Landscape_type) %>% 
mutate(Comments = recode_factor(Comments,
    "Intensive.agriculture" = "Intensive agriculture landscape",
    "Rural.habitat.mosaic" = "Rural habitat mosaic landscape",
    "Urban" = "Urban landscape")) 


#Delete . from plant species
data = data %>% 
mutate(Plant_species = str_replace(Plant_species, "[.]", " "))

#Create merger column for flower counts
data = data %>% 
mutate(Flower_data_merger = paste0(Plant_species, Site_id, Transect_part_id))

#Compare vars
#compare_variables(check_interaction_data, data)

#Add missing vars
data = add_missing_variables(check_interaction_data, data) 
#Reorder variables
data = drop_variables(check_interaction_data, data) 
#Finally drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Unify structure of data
data = change_str(data)

#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)

#Prepare flower count data ---- The data wasn't collected
FlowerCount = read_csv("Data/1_Raw_data/38_Maurer/Flower_count.csv")
#Rename habitats
FlowerCount = FlowerCount %>% rename(Habitat = Focal_habitat) %>% 
mutate(Habitat = recode_factor(Habitat,
     "OSR" = "Oilseed rape crop",
     "forest.edge" = "Forest edge",
     "forest" = "Forest",
     "ext.meadow" = "Extensively managed meadow",
     "art.grass" = "Rotational meadow",
     "fallow" = "Fallow land", 
     "tree.nur" = "Tree nursery",
     "pasture" = "Pasture", 
     "hedge" = "Hedgerow",
     "orchard" = "Orchard",
     "roadside" = "Grassy/herb strip next to road",
     "lawn" = "Frequently mown",
     "perm.mead" = "Permanent meadow",
     "ext.mead" = "Extensively managed meadow",
     "garden" = "Private garden",
     "flow.fal" = "Sown perennial flowering strip",
     "bean" = "Bean crop",
     "potato" = "Potato",
     "sun.flow" = "Sunflower crop",
     "sug.beet" = "Sugar bet crop"))
#Add the general habitat to a comment
FlowerCount = FlowerCount %>% 
rename(Comments = Landscape_type) %>% 
mutate(Flower_data_merger = NA) %>% 
mutate(Day = NA) %>% 
mutate(Month = NA) %>% 
mutate(Year = NA) %>% 
mutate(Comments = NA) %>% 
mutate(Units = "Average flower n. per square meter and site")

#Select cols of interest to be able to merge both
FlowerCount = FlowerCount %>% 
select(Site_id, Transect_part_id, Plant_species, Flower_count,
       Day, Month, Year, Units, Comments)

#Delete . from plant species
FlowerCount = FlowerCount %>% 
mutate(Plant_species = str_replace(Plant_species, "[.]", " "))

#Create merger column for flower counts
FlowerCount = FlowerCount %>% 
mutate(Flower_data_merger = paste0(Plant_species, Site_id, Transect_part_id))

#Seems that there are duplicates of flower counts
#Select just 1
FlowerCount = FlowerCount %>%
group_by_at(vars(-Flower_count)) %>%
mutate(row_number = row_number()) %>%
distinct() %>%
filter(row_number == 1) %>%
select(-row_number)

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
Metadata <- tibble(
  Doi = "Unpublished",
  Dataset_description = "This dataset contains plant-pollinator
  interactions sampled at 12 sites (landscapes)
  in the Northern Swiss lowlands (8 sites along a
  landscape simplification gradient, 4 urban sites)
  of 1 km radius (min. 1.5 km apart). In each
  landscape, wild bees and syrphids were sampled along
  transects in flowering habitat types (habitat type
  categories according to EUNIS habitat types, aggregation 2 level).
  A 1 km transect (2 m wide) was divided into sections proportional
  to the amount of these different flowering habitat types
  in the core (500 m radius) of a landscape (= different
  focal habitats within each site; stratified random sampling).
  We placed these tansect sections randomly in different patches
  of the corresponding habitat types in each landscape
  (including flowering crops). Three sampling rounds
  in April, May/June and July were conducted in each landscape,
  adjusting the location and length of transects in each round
  according to the proportion and location of flowering habitats.
  The transect walks were standardized so that 3 min were used for
  recording flower visiting bees and syrphids and their visited
  flower for each 25 m sub-section, pausing the clock for catching
  and processing the samples. ",
  Taxa_recorded = "Flower visiting bees and syrphidae",
  Sampling_year = "2020",
  Country = "Switzerland",
  Habitat = "Gradient from urban to agricultural landscapes",
  Sampling_sites = nlevels(factor(data$Site_id)),
  Sampling_rounds = "3",
  Sampling_method = "Transects",
  Sampling_area_details = "1km length * 2m wide",
  Sampling_area_species_m2 = NA,
  Sampling_area_total_m2 = 2000 * 12, #2000m *12 sites 
  Sampling_time_details = "3 mins for each 25m transect subsection", 
  Sampling_time_species_round_min = "120m per transect", 
  Sampling_time_total_min = 120 * 12 * 3, #time *sites * rounds
  Total_plant_species = nrow(plant_single_cases),
  Total_pollinator_species = nrow(pollinator_single_cases),
  Floral_counts =  "Yes")

#Transpose metadata
Metadata = as.data.frame(t(Metadata)) %>%  
  rownames_to_column() %>% 
  rename(Metadata_fields = rowname, Metadata_info= V1) %>% as_tibble()

#Prepare authorship data ----
Authorship <- data.frame(
  Coauthor_name = c("Corina Maurer", "Matthias Albrecht"),
  Orcid = c("0000-0001-8103-4111", "0000-0001-5518-3455"),
  E_mail = c("corina.m181@gmail.com", "matthias.albrecht@agroscope.admin.ch"))

#Save data ----
#Create metadata list
Maurer <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Maurer) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Maurer, file="Data/2_Processed_data/38_Maurer.rds")


