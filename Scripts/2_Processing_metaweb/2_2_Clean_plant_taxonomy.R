########################################################################
#2) Clean plant taxonomy----
########################################################################

#EXPLAIN WORKFLOW!

#Load libraries
library(dplyr)

#Read raw metaweb
master = readRDS("Data/Processing/Building_metaweb.rds")

#Explore plant data
#Check total number of distinct plant species names
n_distinct(master$Plant_species)
#Check levels
s = tibble(levels(factor(master$Plant_species)))

#Create plant spp dataset
Plant_spp = master %>% 
select(Plant_species) %>% 
distinct() %>%
rename(name = Plant_species) 
#Extract spp names in a vector
name = Plant_spp %>% pull()

#Download taxonomic info from GBIF
gbif_names = name_backbone_checklist(name= name, kingdom='plants')

#Organise structure of data
gbif_names = gbif_names %>% 
select(!c(usageKey, confidence, kingdomKey,
          phylumKey, classKey, orderKey, familyKey,
         genusKey,  speciesKey, acceptedUsageKey,
         verbatim_index, verbatim_kingdom)) %>% 
rename(Fixed_name = verbatim_name,
       Scientific_name  = scientificName,
       Canonical_name  = canonicalName,
       Accepted_name = species) %>% 
select(Fixed_name, rank, status, matchType, 
       Scientific_name, Canonical_name,
       Accepted_name, kingdom, phylum, order, family,
       genus) %>% 
  rename_all(~str_to_title(.))

#Match and unmatched records
matched = gbif_names %>% filter(Matchtype == "EXACT")
unmatched = gbif_names %>% filter(Matchtype != "EXACT")

#######
#Check records with fuzzy matching
#######

#From the unmatched records check first the FUZZY matchtype
unmatched_fuzzy = unmatched %>% filter(Matchtype == "FUZZY")
#Scroll down in dataset and check for mistakes
#Manual fixes in fuzzy dataset
Plant_spp1 = Plant_spp %>% 
rename(Old_name = name) %>% 
mutate(Fixed_name = case_when(
  Old_name == "Heuchera cult" ~ "Heuchera",
  Old_name == "Rosa NA" ~ "Rosa",
  Old_name == "Rubus rubus" ~ "Rubus", #Write subgenus as genus level
  Old_name == "Cruciata verum" ~ "Galium verum", #I think they refer to G. verum
  Old_name == "Ranunculus tribbus" ~ "Ranunculus", #I think they refer to tribus rank
  T ~ Old_name
  ))


name1 = Plant_spp1 %>% select(Fixed_name) %>% pull()
gbif_names1 = name_backbone_checklist(name= name1, kingdom='plants')
#Organise structure of data
gbif_names1 = gbif_names1 %>% 
select(!c(usageKey, confidence, kingdomKey,
          phylumKey, classKey, orderKey, familyKey,
         genusKey,  speciesKey, acceptedUsageKey,
         verbatim_index, verbatim_kingdom)) %>% 
rename(Fixed_name = verbatim_name,
       Scientific_name  = scientificName,
       Canonical_name  = canonicalName,
       Accepted_name = species) %>% 
select(Fixed_name, rank, status, matchType, 
       Scientific_name, Canonical_name,
       Accepted_name, kingdom, phylum, order, family,
       genus) %>% 
  rename_all(~str_to_title(.))
unmatched1 = gbif_names1 %>% filter(Matchtype != "EXACT")
unmatched_fuzzy1 = unmatched1 %>% filter(Matchtype == "FUZZY") %>% 
  select(Fixed_name, Accepted_name)
#Visual check of fuzzy matching, looks good now!

#######
#Check records with higher rank
#######


