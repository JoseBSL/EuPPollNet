#DATASET NUMBER 48; Lara-Romero (Spain)
#Dataset sent by Carlos

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
data <- read_delim("Data/1_Raw_data/48_Lara-Romero/Interaction_data.csv",
                 locale = locale(encoding = "latin1", decimal_mark = ","))

#Compare vars
#compare_variables(check_interaction_data, data)
#Nothing missing!

#Drop sampling effort and square area (it would be added on the metadata)
data = data %>%
select(!c(Sampling_effort_minutes, Sampling_area_square_meters))

#Unify level
data = data %>% 
mutate(Sampling_method = "Transect") %>% 
mutate(Flower_data = "No") %>% 
mutate(Flower_data_merger = Site_id) 

#Split interaction data into dataframes within a list
InteractionData = split(data, data$Site_id)


#Prepare flower count data ----
#We need to create a list of lists too
#Check levels of Site_id
site_id_levels = levels(factor(data$Site_id))

FlowerCount = tibble(Day = NA, Month = NA, Year = NA, Site_id = site_id_levels, Plant_species = NA,
                     Flower_count = NA, Units = NA, Comments = NA, Flower_data_merger = site_id_levels)

#Set common structure
FlowerCount = change_str2(FlowerCount)

#Split by Site_id
FlowerCount = split(FlowerCount, FlowerCount$Site_id)

#Prepare metadata data ----

#Store unique cases of plants and polls
plant_single_cases = data %>% distinct(Plant_species)
pollinator_single_cases = data %>%distinct(Pollinator_species)

#Build metadata
Metadata <- tibble(
Doi = "https://doi.org/10.5061/dryad.p869n",
Dataset_description = "We constructed quantitative
plant–flower visitor interaction network assemblages at
replicated plots in two habitat types in dry cryophilic
grasslands of Sierra de Guadarrama (Spain): (i) encroached
pastures  and (ii) pastures dominated by forb species where
shrub species are absent.We collected data throughout the
flowering period (13 June to 28 July 2011) for a total of 10
census days for each sampling site at Nevero and 11 census
days for each sampling site at Peñalara. To collect
visitation data, we established two 60 x 100m sampling
plots at each locality (Pastures and Encroached Pastures)
where we established 10 linear transects across the width
of the plots (60m long and 5m wide). The two sampling sites
at each locality were simultaneously surveyed by two work
teams.",
Taxa_recorded = "All floral visitors",
Sampling_year = "2011",
Country = "Spain",
Habitat = "Dry cryophilic grasslands",
Sampling_sites = nlevels(factor(data$Site_id)),
Sampling_rounds = "3 to 5",
Sampling_method = "Transects",
Sampling_area_details = "60mlong * 5mwide * 10 replicates",
Sampling_area_species_m2 = NA,
Sampling_area_total_m2 = 60*5*10*2,
Sampling_time_details = NA,
Sampling_time_species_round_min = NA,
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
  Coauthor_name = c("Carlos Lara-Romero", "José M. Iriondo", "Javier Morente-López"),
  Orcid = c("0000-0003-0962-0567", "0000-0003-2710-3889", "0000-0001-9141-8581"),
  E_mail = c("carlos.lara@urjc.es", "jose.iriondo@urjc.es", "javimorente@gmail.com"))

#Save data ----
#Create metadata list
Michez <- list(InteractionData, FlowerCount, Metadata, Authorship)
#Rename list elements
names(Michez) <- c("InteractionData", "FlowerCount","Metadata", "Authorship")
#Save data
saveRDS(Michez, file="Data/2_Processed_data/48_Lara-Romero.rds")

