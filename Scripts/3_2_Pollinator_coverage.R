#----------------------------#
#Total pollinator coverage
#----------------------------#
#Workflow:
#1)Load libraries and read data 
#2)Calculate coverage for bees
#3)Calculate coverage for syrphids
#4)Calvulate coverage for butterflies
#5)Save coverage values of all groups
#----------------------------#
#1)Load libraries and read data-----
#----------------------------#
#Load libraries and read data
library(readr)
library(dplyr)
#Load pollinator and syrphid masterlist
master_list = read_csv(file = "Data/Species_taxonomy/Thesaurus/Master_bees_syrphids.csv")
#Check cols
colnames(master_list)
#Select cols of interest
master_list = master_list %>% 
select(Order, Family, Subfamily, Tribe, Genus, 
       Subgenus, Species, GenusAndSpecies, 
       TaxonomicAuthority)
#Load butterfly masterlist
master_list1 <- read.delim(file = "Data/Species_taxonomy/Thesaurus/Butterfly_taxon.txt",
                    header = T)
#Check colnames
colnames(master_list1)
#Select cos of interest
master_list1 = master_list1 %>% 
select(taxonRank, scientificName, order, family, genus) %>% 
rename(Rank = taxonRank) %>% 
rename(Accepted_name = scientificName) %>% 
rename(Order = order) %>% 
rename(Family = family) %>% 
rename(Genus = genus)
#Load observed floral visitor species
data = readRDS("Data/Species_taxonomy/Pollinator_taxonomy.rds")
#Check cols
colnames(data)
#Select cols of interest
species_cols = data %>% 
select(Rank, Status, Accepted_name, Order, Family, Genus)

#----------------------------#
#2) Calculate coverage for bees----
#----------------------------#
#First filter only key bee families
#There are 7 but just 6 in Europe
bee_fam = c("Apidae", "Megachilidae", "Halictidae",
            "Andrenidae", "Colletidae", "Melittidae")
#Create tibble of bee list
potential_bees = master_list %>% 
filter(Family %in% bee_fam) %>% 
select(GenusAndSpecies) 
#Create vetor of bee species
potential_bees1 = potential_bees %>%   
pull()
#Select OBSERVED bee families
observed_bees = species_cols %>% 
filter(Family %in% bee_fam)
#Now select only accepted species
observed_bees = observed_bees %>% 
select(Rank, Accepted_name) %>% 
filter(Rank == "SPECIES") %>% 
select(Accepted_name) %>% 
distinct() 
#Create vector with unique cases
unique_obs_bees =  observed_bees %>% pull()
#Now both lists are ready
#Check if there are unmatching names
matched_bees = observed_bees %>% 
filter(Accepted_name %in% potential_bees1)
unmatched_bees = observed_bees %>% 
filter(!Accepted_name %in% potential_bees1)
#Check if 0 unmatched records
bee_checks = ifelse(nrow(unmatched_bees) == 0, 
       "All bees match masterlist", 
       "There are mistmatches of bees")
bee_checks
#If 0, calculate percentage of coverage for bees
bee_coverage = length(unique_obs_bees) / length(potential_bees1) * 100
bee_coverage
#----------------------------#
#3) Calculate coverage for syrphids----
#----------------------------#
#First filter by Syrphidae family
syrphid_fam = c("Syrphidae")
#Create bee list
potential_syrphids = master_list %>% 
filter(Family %in% syrphid_fam) %>% 
select(GenusAndSpecies) 
#Create vetor of POTENTIAL syrphid species
potential_syrphids1 = potential_syrphids %>%   
pull()
#Select OBSERVED syrphid families
observed_syrphids = species_cols %>% 
filter(Family %in% syrphid_fam)
#Now select only accepted species
observed_syrphids = observed_syrphids %>% 
select(Rank, Accepted_name) %>% 
filter(Rank == "SPECIES") %>% 
select(Accepted_name) %>% 
distinct() 
#Create vector with unique cases
unique_obs_syrphids =  observed_syrphids %>% pull()
#Now both lists are ready
#Check if there are unmatching names
matched_syrphids = observed_syrphids %>% 
filter(Accepted_name %in% potential_syrphids1)
unmatched_syrphids = observed_syrphids %>% 
filter(!Accepted_name %in% potential_syrphids1)
#Check if 0 unmatched records
syrphid_checks = ifelse(nrow(unmatched_syrphids) == 0, 
       "All syrphids match masterlist", 
       "There are mistmatches of syrphids")
syrphid_checks
#If 0, calculate percentage of coverage for bees
syrphid_coverage = length(unique_obs_syrphids) / length(potential_syrphids1) * 100
syrphid_coverage
#----------------------------#
#3) Calculate coverage for butterflies----
#----------------------------#
#No need to filter now asall are butterflies
#Create bee list
potential_lepidoptera = master_list1 %>% 
select(Accepted_name) 
#Create vetor of POTENTIAL butterfly species
potential_lepidoptera1 = potential_lepidoptera %>%   
pull()


but_fam = unique(master_list1$Family)
#Select OBSERVED LEPIDOPTERA species (ORDER LEVEL NOW)
observed_lepidoptera = species_cols %>% 
filter(Order == "Lepidoptera") %>% 
filter(Family %in% but_fam)
#Now select only accepted species
observed_lepidoptera = observed_lepidoptera %>% 
select(Rank, Accepted_name) %>% 
filter(Rank == "SPECIES") %>% 
select(Accepted_name) %>% 
distinct() 
#Create vector with unique cases
unique_obs_lepidoptera =  observed_lepidoptera %>% pull()
#Now both lists are ready
#Check if there are unmatching names
matched_lepidoptera = observed_lepidoptera %>% 
filter(Accepted_name %in% potential_lepidoptera1)
unmatched_lepidoptera = observed_lepidoptera %>% 
filter(!Accepted_name %in% potential_lepidoptera1)
#Check if 0 unmatched records
lepidoptera_checks = ifelse(nrow(unmatched_syrphids) == 0, 
       "All Lepidoptera match masterlist", 
       "There are mistmatches of Lepidoptera")
#If 0, calculate percentage of coverage for bees
lepidoptera_coverage = length(unique_obs_lepidoptera) / 
                    length(potential_lepidoptera1) * 100
lepidoptera_coverage
#----------------------------#
#Save coverage values---- 
#----------------------------#
#Final checks before saving!
c(bee_checks, syrphid_checks, lepidoptera_checks)

#Save
saveRDS(bee_coverage, "Data/Manuscript_info/bee_coverage.RData")
saveRDS(syrphid_coverage, "Data/Manuscript_info/syrphid_coverage.RData")
saveRDS(lepidoptera_coverage, "Data/Manuscript_info/lepidoptera_coverage.RData")


