#Calculate potential number of pollinators at European level

#Load libraries
library(dplyr)
#Load data 
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")
data = data %>%
dplyr::mutate(Year = lubridate::year(Date), 
                Month = lubridate::month(Date), 
                Day = lubridate::day(Date))

#Calculate avreage coverage across main groups
bee_coverage = readRDS("Data/Manuscript_info/bee_coverage.RData")
syrphid_coverage = readRDS("Data/Manuscript_info/syrphid_coverage.RData")
lepidoptera_coverage = readRDS("Data/Manuscript_info/lepidoptera_coverage.RData")
coverage = c(bee_coverage, syrphid_coverage, lepidoptera_coverage)
average_coverage = mean(coverage)
average_coverage
#Calculate number of species across main groups
bees_spp = readRDS("Data/Manuscript_info/bee_spp.RData")
syrphid_spp = readRDS("Data/Manuscript_info/syrphid_spp.RData")
lepidoptera_spp =readRDS("Data/Manuscript_info/lepidoptera_spp.RData")
#Species main groups
spp_main_groups = (bees_spp + syrphid_spp + lepidoptera_spp)
#Check number of pollinator species
pollinator_spp_number = data %>% 
select(Pollinator_rank, Pollinator_accepted_name)%>%
filter(Pollinator_rank == "SPECIES") %>% 
select(Pollinator_accepted_name) %>% 
n_distinct()
#substract species of main groups
other_spp = pollinator_spp_number - spp_main_groups
#Let's assume constant coverage and extrapolate potential number of species
extrapolated_other_spp = other_spp + other_spp * (1-average_coverage/100)
#Add number of species of main groups now
bee_potential_spp = readRDS("Data/Manuscript_info/bee_potential_spp.RData")
syrphid_potential_spp = readRDS("Data/Manuscript_info/syrphid_potential_spp.RData")
lepidoptera_potential_spp = readRDS("Data/Manuscript_info/lepidoptera_potential_spp.RData")
#Potential species of main groups
potential_spp_main_groups = bee_potential_spp + syrphid_potential_spp + lepidoptera_potential_spp

#Add other spp
total_expected_spp = potential_spp_main_groups + extrapolated_other_spp
total_expected_spp 
# Approximately 5000 species
