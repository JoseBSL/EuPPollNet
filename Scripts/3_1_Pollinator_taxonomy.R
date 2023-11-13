#----------------------------#
#Clean pollinator taxonomy----
#----------------------------#

#Workflow:
#1)Use fuzzy matching to recover species names from Thesaurus master list
#Note: This step could be avoided as the GBIF taxonomy
#provides also very similar and accurate results
#As we started with it, we keep it (at least for now)
#2)Conduct manual fixes from the non-recovered records
#3)Retrieve GBIF data
#4)Save poll 
#5)Safety check by study Id (FOR EACH NEW STUDY)

#Load libraries
library(dplyr) 
library(stringr)
library(purrr)
library(gtools)
library(reshape2) #to use melt function later on

#Read raw metaweb
master = readRDS("Data/Processing/Building_metaweb.rds")
#Read master list of unique cases of pollinator species
species_tesaurus <- readRDS(file = "Data/Species_taxonomy/Thesaurus/taxonomy.rds")
#Function to give str to data (used at the end of the code)
source("Scripts/Processing/Functions/Change_str.R")

#Explore poll data
#Check total number of distinct poll species names
n_distinct(master$Pollinator_species)
#Check levels
levels(factor(master$Pollinator_species))

#----------------------------#
#1)Fuzzy matching----
#----------------------------#
#Create unique list of pollinator species
Gen_sp = master %>% 
select(Pollinator_species) %>% 
distinct() %>% 
pull()
#Generate col with genus and species names from Thesaurus
species_tesaurus = species_tesaurus %>% 
mutate(Gen_sp = paste(trimws(species_tesaurus$Genus),
                trimws(species_tesaurus$Species)))

#Select the matching and the unmatching records
matching <- Gen_sp[which(Gen_sp %in% species_tesaurus$Gen_sp)]
unmatching <- Gen_sp[which(!Gen_sp %in% species_tesaurus$Gen_sp)]
mismatches <- unique(unmatching) #speed up the process (just in case of duplicates)
#As this takes long 
#let's exclude the unidentified records to speed up more the process
#There are few records that get lost that start with sp
#Minor issue but keep in mind
exclude_vector <- c(" sp. "," sp."," sp", " spp."," spec. "," spec"," sp_"," spp"," dp"," ss",
              " Unidentified"," Unidentified ", " P. "," aff. ", " unknown",  "NA", " indet", " IMG") 

#Generate filtered list
mismatches1 <- mismatches[!grepl(paste(exclude_vector, collapse = "|"), mismatches)]

#Now conduct fuzzy matching to recover records 
#(3 mins at the moment with 1500 records)
#Select minimum difference between strings (k=2)
k <- 2
# Calculate the matrix of distances or differences between strings
#And convert to logical based on our k, criteria
dist_matrix <- stringdist::stringdistmatrix(mismatches1, species_tesaurus$Gen_sp) <= k
#Create row sums and add species names
#To know how many times k<2 is accomplished
row_sums <- as_tibble(rowSums(dist_matrix))
row_sums$mismatches = mismatches1
#Extract list of species that are being fixed
spp_fixed = row_sums %>% filter(value == 1) %>% pull(mismatches)
#To extract the right species, we convert the matrix to long format
colnames(dist_matrix) = species_tesaurus$Gen_sp
rownames(dist_matrix) = mismatches1
#Convert matrix to long format
long = melt(dist_matrix, value.name = "Logical", varnames=c('Mismatch', 'Fixed'))
#Select species that are being fixed and has a unique similar case
fixed = long %>% filter(Mismatch %in% spp_fixed & Logical == "TRUE") %>% 
select(Mismatch, Fixed)
#At the moment almost 350 records are being fixed
#Convert vector to tibble and do left join to integrate in the original dataset
mismatches = as_tibble(mismatches) %>% 
rename(Mismatch = value) 
#Create to recover dataset
to_recover = left_join(mismatches, fixed)

#----------------------------#
#2)Manual fixes-----
#----------------------------#
#Keep processing for now general typos or mistakes to homogenise names
#We do it in a 3 step process to avoid mistakes:
#Filter the string, fix it and merge it back

#----------------------------#
#2.1) Select any string that contains a number (over 400 records)-----
#----------------------------#
to_recover1 = to_recover[grepl("\\d", to_recover$Mismatch),]
#Select any string with parenthesis (generally years)
str_pattern1 = to_recover1[grepl(" \\s*\\([^\\)]+\\)", to_recover1$Mismatch),]
#Select just first 2 words (the easiest for now)
str_pattern1$Fixed1 =  word(str_pattern1$Mismatch, 1,2, sep=" ")
#Merge back, unify cols when is not an NA and select just main fixed col
to_recover1 = left_join(to_recover1, str_pattern1) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!Fixed1)

#2.1.1)
#Select anything with sp that hasn't been fixed yet
str_pattern2 = to_recover1[grepl("sp", to_recover1$Mismatch),]
#Select only 1st word
str_pattern2$Fixed1 =  word(str_pattern2$Mismatch, 1)
#Small manual fix (micro moth, Andrena(Chlorandrena) and Butterfly)
str_pattern2 = str_pattern2 %>% 
mutate(Fixed1 = case_when(
  str_detect(Mismatch, "Micro") ~ "Heterocera",
  str_detect(Mismatch, "Andrena\\(Chlorandrena\\) sp_1") ~ "Andrena",
  str_detect(Mismatch, "Butterfly") ~ "Lepidoptera",
  T ~ Fixed1))  
  
#Merge back
to_recover1 = left_join(to_recover1, str_pattern2) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!Fixed1)

#2.1.2)
#Now select everything that has a number that hasn't been fixed yet
str_pattern3 = to_recover1[grepl("\\d", to_recover1$Mismatch) & is.na(to_recover1$Fixed),]
#From those, select the ones that have a coma
str_pattern3.1 = str_pattern3[grepl("\\,", str_pattern3$Mismatch),]
#Select first two words
str_pattern3.1$Fixed1 =  word(str_pattern3.1$Mismatch, 1, 2)
#Merge back
str_pattern3 = left_join(str_pattern3, str_pattern3.1) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!Fixed1)
#Now, select everything that does not contain a coma
str_pattern3.2 = str_pattern3[!grepl("\\,", str_pattern3$Mismatch),]
#Select just one word and fix manually weird cases
str_pattern3.2$Fixed1 =  word(str_pattern3.2$Mismatch, 1)
#Fix weird cases
str_pattern3.2 = str_pattern3.2 %>%  
mutate(Fixed1 = case_when(
    str_detect(Fixed1, "indet") ~ "Diptera", 
    str_detect(Fixed1, "Polyomm") ~ "Polyommatus icarus",
    str_detect(Mismatch, "lille") ~ "Unknown",
    str_detect(Fixed1, "0") ~ "Unknown",
    TRUE ~ Fixed1))
#Merge back
to_recover1 = left_join(to_recover1, str_pattern3.2) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!Fixed1)

#2.1.3)
#Filter records with a comma but without space
str_pattern4 = to_recover1 %>% mutate(Mismatch1 = gsub(",", " , ", Mismatch))
#Select records that have a comma but haven't been fixed yet
str_pattern4 = str_pattern4[grepl("\\,", str_pattern4$Mismatch1) & is.na(str_pattern4$Fixed),]
#Select 1st 2 words
str_pattern4$Fixed1 =  word(str_pattern4$Mismatch, 1, 2)
#Merge back
to_recover1 = left_join(to_recover1, str_pattern4) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1, Mismatch1))

#Merge back to recover dataset
#First rename and now merge
to_recover1 = to_recover1 %>%
rename(Fixed1 = Fixed)
#Now merge
to_recover = left_join(to_recover, to_recover1) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1))

#----------------------------#
#2.2) Select strings with sp that haven't been fixed yet-----
#----------------------------#
str_pattern5 = to_recover %>% 
filter(str_detect(Mismatch, "sp") & is.na(Fixed))

#Filter any 2nd word with 3 characters or shorter
#And fix manually some special cases
str_pattern5.1 = str_pattern5 %>% 
mutate(Mismatch_word2 = str_replace_all(Mismatch, "_", " ")) %>% 
mutate(Mismatch_word2 = word(Mismatch,2)) %>% 
filter(str_length(Mismatch_word2) <= 3) %>% 
mutate(Fixed1 = word(Mismatch, 1)) %>% 
mutate(Fixed1 = case_when(
    Fixed1 == "Crabronidae(Oxybelus)" ~ "Oxybelus",
    Fixed1 == "Crabronidae(Oxybelus?)" ~ "Oxybelus",
    TRUE ~ Fixed1)) %>% 
mutate(Fixed1 = case_when(
  str_detect(Fixed1, "Adscita/Jodanita") ~ "Adscita",
  str_detect(Mismatch, "Elateridae sp. Prosternon tesselatum\\?") ~ "Prosternon tesselatum",
  str_detect(Mismatch, "Anthophora cf. dispar") ~ "Anthophora dispar",
  T ~ Fixed1))

str_pattern5.1_fixed = str_pattern5.1 %>% 
select(!Mismatch_word2)

#Select second word with more than 3 characters
str_pattern5.2 = str_pattern5 %>% 
mutate(Mismatch_word2 = str_replace(Mismatch, "_", " ")) %>% 
mutate(Mismatch_word2 = word(Mismatch,2)) %>% 
filter(str_length(Mismatch_word2) > 3) 

#Fix specific characters
str_pattern5.2 = str_pattern5.2 %>% 
mutate(Fixed1 = case_when(
       Mismatch_word2 == "spec" ~ word(Mismatch,1),
       Mismatch_word2 == "spp." ~ word(Mismatch,1),
       Mismatch_word2 == "sp.?" ~ word(Mismatch,1),
       Mismatch_word2 == "spec." ~ word(Mismatch,1),
       Mismatch == "Syrphidae sp_Meliscaeva-alike" ~ "Meliscaeva",
       Mismatch == "Andrena sp_flavipes-alike" ~ "Andrena flavipes",
       Mismatch == "Lasioglossum sp_immunitum-alike" ~ "Lasioglossum immunitum",
       Mismatch == "Eucera sp_collaris-alike" ~ "Eucera collaris",
       Mismatch == "Halictus sp_scabiosae-alike" ~ "Halictus scabiosae",
       Mismatch == "Eristalis sp_tenax-alike" ~ "Eristalis tenax",

       TRUE ~ Fixed))

str_pattern5.2_fixed = str_pattern5.2 %>% 
filter(!is.na(Fixed1))  %>% 
select(!Mismatch_word2)
  
#Now fix the non-fixed ones
#First with underscore
str_pattern5.2.2 = str_pattern5.2 %>% 
filter(is.na(Fixed1)) %>% 
filter(str_detect(Mismatch, "_")) %>% 
mutate(Fixed1 = case_when(
    str_detect(Mismatch, "Syrphidae sp_Sphaerophoria\\?") ~ "Sphaerophoria",
    str_detect(Mismatch_word2, "sp_light-stripes_malachurum-sized") ~ "Lasioglossum malachurum",
    str_detect(Mismatch, "Lasioglossum sp_immunitum-alike") ~ "Lasioglossum immunitum",
    str_detect(Mismatch, "Lasioglossum sp_malachurum-alike") ~ "Lasioglossum malachurum",
    str_detect(Mismatch, "Lasioglossum sp_leucozonium_cedri-type") ~ "Lasioglossum leucozonium",
    str_detect(Mismatch, "Lasioglossum sp_immunitum/malachurum-alike") ~ "Lasioglossum immunitum",
    T ~ word(Mismatch, 1)
))

str_pattern5.2.2_fixed = str_pattern5.2.2 %>% 
select(!Mismatch_word2)

#Now the remaining ones
str_pattern5.2.3 = str_pattern5.2 %>% 
filter(is.na(Fixed1)) %>% 
filter(!str_detect(Mismatch, "_")) %>% 
mutate(Mismatch_word2 = Mismatch) %>% 
mutate(Mismatch_word2 = str_replace(Mismatch_word2, "/", " ")) %>% 
mutate(Fixed1 = case_when(
  Mismatch == "Pseudapis(Nomiapis) bispinosa" ~ "Pseudapis bispinosa",
  Mismatch == "Unidentified Vespidae" ~ "Vespidae",
  Mismatch == "Dolichovespula norwegica/Vespula rufa" ~ "Dolichovespula norwegica",
  Mismatch == "Dolichovespula saxonica/sylvestris" ~ "Dolichovespula saxonica",
  T ~ Fixed)) %>% 
  select(!Mismatch_word2)
#Fix last pattern, merge back one by one   
str_pattern5.2.3.1 = str_pattern5.2.3 %>%      
filter(str_detect(Mismatch, "[?]")) %>% 
mutate(Fixed2 = str_replace_all(Mismatch, "[?]", ""))  
#Merge
str_pattern5.2.3_fixed = left_join(str_pattern5.2.3, str_pattern5.2.3.1) %>% 
mutate(Fixed = coalesce(Fixed1, Fixed2)) %>% 
select(!c(Fixed1, Fixed2)) %>% 
rename(Fixed1 = Fixed) %>% 
mutate(Fixed = NA)
#Now merge all
str_pattern5_fixed = bind_rows(str_pattern5.2.3_fixed, str_pattern5.2.2_fixed,
          str_pattern5.2_fixed, str_pattern5.1_fixed)
#Check for missing ones
#Extract species
spp = str_pattern5_fixed %>% pull(Mismatch)
#Filter out those
to_fix_str_pattern_5 = str_pattern5 %>% 
filter(!Mismatch %in% spp) %>% 
mutate(to_fix = gsub("[.]", " ", Mismatch)) %>% 
mutate(to_fix = gsub("[_]", " ", to_fix)) %>% 
mutate(Fixed1 = word(to_fix, 1)) %>% 
select(!to_fix) 

#Now bind missing ones and its done!
str_pattern5 = bind_rows(str_pattern5_fixed, to_fix_str_pattern_5)

#Merge back
to_recover = left_join(to_recover, str_pattern5) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1)) %>% 
rename(Fixed1 = Fixed)

#----------------------------#
#2.3) Now fix remaining strings------- 
#----------------------------#
#Filter by specific strings
#cf, aff, dp, unknown, nymph
str_pattern6.1 = to_recover %>% 
filter(str_detect(Mismatch, 
paste(c("cf.", " aff. ", "dp", "unknown", "nymph", "agg"), collapse = "|")) &
is.na(Fixed1)) %>% 
mutate(Fixed1 = str_replace(Mismatch, " cf.", "")) %>% 
mutate(Fixed1 = gsub(" aff.", "", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Argynnis dp", "Argynnis", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = if_else(str_detect(Mismatch, "unknown") , word(Mismatch, 1), Fixed1)) %>% 
mutate(Fixed1 = gsub("Phaneroptera nana (nymph)", "Phaneroptera nana", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub(".agg.", "", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("_agg", "", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub(" agg.", "", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub(" agg", "", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub(".", " ", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("_", " ", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = case_when(str_detect(Mismatch, " cf ") ~ gsub(" cf ", " ", Mismatch),
       T ~ Fixed1)) %>% 
rename(Fixed2 = Fixed1)
#Merge back
to_recover = left_join(to_recover, str_pattern6.1) %>% 
mutate(Fixed = coalesce(Fixed1, Fixed2)) %>% 
select(!c(Fixed1, Fixed2)) 

#Select unidentified string
str_pattern6.2 = to_recover %>% 
filter(str_detect(Mismatch, "Unidentified") &
      is.na(Fixed)) %>% 
mutate(Fixed1 = gsub("Unidentified ", "", Mismatch))
#Now conduct manual fixes
#it can be done in a easier way but in case of new datasets
#I prefer to keep it simple, so we don't mess things later on
str_pattern6.2 = str_pattern6.2 %>% 
mutate(Fixed1 = gsub("Unidentified", "Unknown", Fixed1))  %>% 
mutate(Fixed1 = gsub("Hymenoptera/Diptera", "Unknown", Fixed1)) %>% 
mutate(Fixed1 = gsub("Muscidae s.lat.", "Muscidae", Fixed1))  %>% 
mutate(Fixed1 = gsub("Chelostoma?", "Chelostoma", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Bombyliidae?", "Bombyliidae", Fixed1, fixed=T))  %>% 
mutate(Fixed1 = gsub("Muscidae s", "Muscidae", Fixed1, fixed=T)) 
#Merge back
to_recover = left_join(to_recover, str_pattern6.2) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1)) 

#Filter the ones with "type and /"
#We are going to select the most common 
#morphospecies from the complex (in our list of polls)
str_pattern6.3 = to_recover %>% 
filter(str_detect(Mismatch, paste(c(" type", "/"), 
      collapse="|")) & is.na(Fixed)) %>% 
mutate(Fixed1 = gsub("Bombus terrestris/lucorum", 
      "Bombus terrestris", Mismatch, fixed=T)) %>% 
  mutate(Fixed1 = gsub("Halictus simplex/langobardicus/compressus", 
      "Halictus simplex", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Bombus lucorum/terrestris", 
      "Bombus terrestris", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Halictus simplex/compressus/langobardicus", 
      "Halictus simplex", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Halictus confusus/tumulorum", 
      "Halictus confusus", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Bombus cryptarum/lucorum/magnus/terrestris", 
      "Bombus terrestris", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Bombus ruderarius/sylvarum", 
      "Bombus sylvarum", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Bombus lucorum/magnus/cryptarum", 
      "Bombus lucorum", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Bombus lapidarius/ruderarius/sylvarum", 
      "Bombus lapidarius", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Bombus terrestris/soroeensis", 
      "Bombus terrestris", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Pieris brassicae/rapae", 
      "Pieris brassicae", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Bombus vestalis/bohemicus", 
      "Bombus bohemicus", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Bombus lapidarius type", 
      "Bombus lapidarius", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Bombus terrestris type", 
      "Bombus terrestris", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Platycheirus albimanus/muelleri", 
      "Platycheirus albimanus", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Cheilosia albitarsis/ranunculi", 
      "Cheilosia albitarsis", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Cheilosia ranunculi/albitarsis", 
      "Cheilosia albitarsis", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Pipiza noctiluca/bimaculata", 
      "Pipiza noctiluca", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Oedemera lurida/monticola", 
      "Oedemera lurida", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Osmia rufa/aurulenta-alike", 
      "Osmia rufa", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Osmia rufa/aurulenta", 
      "Osmia rufa", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Lasioglossum immunitum-type", 
      "Lasioglossum immunitum", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Lasioglossum malachurum/mediterraneum", 
      "Lasioglossum malachurum", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Panurgus dargius/cephalotes", 
      "Panurgus dargius", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Lasioglossum leucoleucozonium/immunitum", 
      "Lasioglossum leucozonium", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Lasioglossum lativentre/sexnotatum/laterale", 
      "Lasioglossum sexnotatum", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Melitaea athalia/britomartis", 
      "Melitaea athalia", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Zygaena minos/purpuralis", 
      "Zygaena minos", Fixed1, fixed=T)) %>% 
mutate(Fixed1 = gsub("Pyrgus alveus/armoricanus", 
      "Pyrgus alveus", Fixed1, fixed=T))

#Merge back
to_recover = left_join(to_recover, str_pattern6.3) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1)) 

#Fix now strings with "indet"
str_pattern6.4 = to_recover %>% 
filter(str_detect(Mismatch, "indet") & is.na(Fixed)) %>% 
mutate(Fixed1 = if_else(str_detect(Mismatch, "^indet"),
      NA_character_, word(Mismatch, 1))) %>% 
mutate(Fixed1 = case_when(str_detect(Mismatch, pattern=" flue") & 
                is.na(Fixed1) ~ "Diptera",
      str_detect(Mismatch, pattern=" svirreflue") & 
                is.na(Fixed1) ~ "Syrphidae",
      str_detect(Mismatch, pattern=" bille") & 
                is.na(Fixed1) ~ "Coleoptera",
      str_detect(Mismatch, pattern=" hveps") & 
                is.na(Fixed1) ~ "Vespidae",
       T ~ Fixed1))
#Merge back
to_recover = left_join(to_recover, str_pattern6.4) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1)) 

#Fix now strings with "."
str_pattern6.5= to_recover %>% 
filter(str_detect(Mismatch, "[.]") & is.na(Fixed)) %>% 
mutate(Fixed1 = case_when(
  str_detect(Mismatch, "_c.f") ~ gsub("_.*", "", Mismatch),
  str_detect(Mismatch, "Platycheirus scutatus s.l.") ~ "Platycheirus scutatus",
  str_detect(Mismatch, "Bombus P. bohemicus") ~ "Bombus bohemicus",
  str_detect(Mismatch, "Bombus P. sylvestris") ~ "Bombus sylvestris",
  str_detect(Mismatch, "Bombus P. sylvestris") ~ "Bombus sylvestris",
  T ~ gsub("[.]", " ", Mismatch))) 

#Merge back
to_recover = left_join(to_recover, str_pattern6.5) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1)) 

#Fix now strings with "_"
str_pattern6.6= to_recover %>% 
filter(str_detect(Mismatch, "[_]") & is.na(Fixed)) %>% 
mutate(Fixed1 = str_replace(Mismatch, "[_]", " ")) %>% 
mutate(Fixed1 = case_when(
  str_detect(Mismatch, "Andrena nigroaenea_nigrosericea") ~ "Andrena nigroaenea",
  str_detect(Mismatch, "Andrena angustior_impressa") ~ "Andrena angustior",
  str_detect(Mismatch, "Andrena rhyssonota_flava") ~ "Andrena rhyssonota",
  str_detect(Mismatch, "Panurgus calcaratus_lagopus") ~ "Panurgus calcaratus",
  str_detect(Mismatch, "Lasioglossum leucozonium_cedri") ~ "Lasioglossum leucozonium",
  str_detect(Mismatch, "Osmia latreillei_iberoafricana") ~ "Osmia latreillei",
  str_detect(Mismatch, "Chalicodoma sicula_hiendlmayri") ~ "Megachile sicula",
  str_detect(Mismatch, "Hoplitis adunca_contraria") ~ "Hoplitis adunca",
  str_detect(Mismatch, "Sphaerophoria_scripta_group") ~ "Sphaerophoria scripta",
  T ~ Fixed1))

#Merge back
to_recover = left_join(to_recover, str_pattern6.6) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1)) 

#Fix now strings with NA
str_pattern6.7 = to_recover %>% 
filter(str_detect(Mismatch, "NA")) %>% 
mutate(Fixed1 = str_replace(Mismatch, "NA", "")) %>% 
mutate(Fixed1 = str_replace(Fixed1, " NA", "Unknown"))

#Merge back
to_recover = left_join(to_recover, str_pattern6.7) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1)) 

#Now fix the remaining strings with parenthesis
str_pattern6.8 = to_recover %>% 
filter(str_detect(Mismatch, "\\(") & is.na(Fixed)) %>% 
mutate(Fixed1 = gsub("\\s*\\([^\\)]+\\)","", Mismatch)) %>% 
mutate(Fixed1 = str_replace(Fixed1, "Andrena helvola \\(introg", "Andrena helvola"))

#Merge back
to_recover = left_join(to_recover, str_pattern6.8) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1)) 

#Now fix the remaining strings with parenthesis
str_pattern6.9 = to_recover %>% 
filter(str_detect(Mismatch, "\\?") & is.na(Fixed)) %>% 
mutate(Fixed1 = str_replace_all(Mismatch, "\\?", "")) %>% 
mutate(Fixed1 =  case_when(str_detect(Fixed1, 
  "Sphaerophoria menthastri-group") ~ "Sphaerophoria menthastri",
   T ~ Fixed1))

#Merge back 
to_recover = left_join(to_recover, str_pattern6.9) %>% 
mutate(Fixed = coalesce(Fixed, Fixed1)) %>% 
select(!c(Fixed1)) 

#Conduct last fixes
#----------------------------#
#2)Fix synonyms typos (with help of worldflora)-----
#----------------------------#

#Note, each case has been searched manually and assessed
#In some circunstances these decissions are a personal guess
#Based on what is the most likely the possible mistake
to_recover = to_recover %>% 
mutate(Fixed = case_when(
  str_detect(Mismatch, "Coleottero") ~ "Coleoptera",
  str_detect(Mismatch, "Unidentifed Polyommatus") ~ "Polyommatus",
  str_detect(Mismatch, "BOMMUS\\b") ~ "Bombus",
  is.na(Mismatch) ~ "Unknown",
  str_detect(Mismatch, "Halictus confusus alpinus") ~ "Halictus confusus",
  str_detect(Mismatch, "Sphaerophoria menthastri-group") ~ "Sphaerophoria menthastri",
  str_detect(Mismatch, "thrips") ~ "Thysanoptera",
  str_detect(Mismatch, "foto IMG") ~ "Unknown",
  str_detect(Mismatch, "Syrphidae \"honningbisvirreflue\"") ~ "Syrphidae",
  str_detect(Mismatch, "Bombus ss\\b") ~ "Bombus",
  str_detect(Mismatch, "Bombus telu\\b") ~ "Bombus",
  str_detect(Mismatch, "Halictus simplex-compressus-longobardicus") ~ "Halictus simplex",
  str_detect(Mismatch, NA_character_) ~ "Unknown",
  str_detect(Mismatch, "Bombus missing\\b") ~ "Bombus",
  str_detect(Mismatch, "Halictus gr simplex") ~ "Halictus simplex",
  str_detect(Mismatch, "Lasioglossum immunitum-type") ~ "Lasioglossum immunitum",
  str_detect(Mismatch, "Bombus terrestris complex") ~ "Bombus terrestris",
  str_detect(Mismatch, "psithyrus sylevstris") ~ "Bombus sylvestris",
  str_detect(Mismatch, "Microlepidoptera") ~ "Lepidoptera",
  str_detect(Mismatch, "Ichnomonidae") ~ "Ichneumonidae",
  str_detect(Mismatch, "Selandriinae") ~ "Tenthredinidae",
  str_detect(Mismatch, "Polyommatini") ~ "Lycaenidae",
  str_detect(Mismatch, "Apiformes") ~ "Apoidea",
  str_detect(Mismatch, "Anthophora manicatum") ~ "Anthidium manicatum",
  str_detect(Mismatch, "Scaeva vitripennis") ~ "Syrphus vitripennis",
  str_detect(Mismatch, "Spathiogaster ambulans") ~ "Spazigaster ambulans",
  str_detect(Mismatch, "Empis longipes") ~ "Rhamphomyia longipes",
  str_detect(Mismatch, "Nacerda ustulata") ~ "Anogcodes melanurus",
  str_detect(Mismatch, "Mordella bipunctata") ~ "Mediimorda bipunctata",
  str_detect(Mismatch, "Paroxyna bidentis") ~ "Dioxyna bidentis",
  str_detect(Mismatch, "Melitta haemor") ~ "Melitta haemorrhoidalis",
  str_detect(Mismatch, "Chrysosomoxys macrocercus") ~ "Unknown",
  str_detect(Mismatch, "Leptura melanura") ~ "Stenurella melanura",
  str_detect(Mismatch, "Eupeodes strigatus") ~ "Eumerus strigatus",
  str_detect(Mismatch, "Apidea") ~ "Apidae",
  str_detect(Mismatch, "Polietes ladarius") ~ "Polietes lardaria",
  str_detect(Mismatch, "Lucila") ~ "Lucilia",
  str_detect(Mismatch, "Halicutus") ~ "Halictus",
  str_detect(Mismatch, "Zigena") ~ "Zygaena",
  str_detect(Mismatch, "Vermilionidae") ~ "Vermileonidae",
  str_detect(Mismatch, "Tenthridae") ~ "Tenthredinidae",
  str_detect(Fixed, "Hoplitis scutellaris") ~ "Osmia scutellaris",
  str_detect(Mismatch, "Lycaena corydon") ~ "Lysandra hispana",
  Mismatch == "Anthidium scapulare" ~ "Pseudoanthidium scapulare",
  Mismatch == "Eoseristalis lineata" ~ "Cheilosia morio",
  Fixed == "Sarginae/Sargus" ~ "Sargus",
  Fixed == "Sarcophagini" ~ "Sarcophagidae", #add at family level
  Fixed == "Lycaena hippothoe" ~ "Lycaena dispar", #synonym
  Fixed == "Melanomya Nana" ~ "Melanomya nana", #upper case
  Fixed == "Polistinae" ~ "Vespidae", #higher rank
  Fixed == "Zophomya Temula" ~ "Zophomyia temula", #typo
  Fixed == "Odontomyia Angulata" ~ "Odontomyia angulata", #upper case
  Fixed == "Protodexiini" ~ "Sarcophagidae", #higher rank
  Mismatch == "Brachymyia berberina" ~ "Criorhina berberina", #synonym
  Mismatch == "Euphydryas aurorina" ~ "Euphydryas aurinia", #typo
  Fixed == "Bombus lucorum-Komplex lucorum" ~ "Bombus lucorum", #typo
  Fixed == "Bombus lucorum-Komplex cryptarum" ~ "Bombus lucorum", #typo
  Mismatch == "Agynnis aglaia" ~ "Speyeria aglaja", #synonym
  Mismatch == "Delia lophota (syn. D. nuda)" ~ "Delia lophota", #synonym
  T ~ Fixed)) %>% 
  rename(Old_name = Mismatch, Name = Fixed) 


#Very few subfamilies (write them at family level)
#GBIF Does not find them at subfamily level
to_recover = to_recover %>% 
mutate(Name = case_when(
  Old_name == "Ctenopelmatinae" ~ "Ichneumonidae",
  Old_name == "Pimplinae" ~ "Ichneumonidae",
  Old_name == "Helconinae" ~ "Braconidae",
  Old_name == "Microgasterinae" ~ "Braconidae",
  T ~ Name))

#Add uncertainty cols (default=No)
to_recover1 = to_recover %>% 
mutate(Unsure_id = "No") %>% 
mutate(Uncertainty_type = NA)  

#For the fixed ones, add additional info
#For instance, unsure id. Then add another col where we explain
#the original status
unsure_id = c(" type","aggr.","agg.","[/]", 
              "aff(?![:alpha:])", " cf", " Cf", "[?]",
              "_agg", "_group", "-group",
              "_c.f", "malachurum-alike",
              "-.*-", " agg$", " complex", "-Komplex")

to_recover1 = to_recover1 %>% 
mutate(Unsure_id = case_when(
str_detect(Old_name, pattern = paste(unsure_id, collapse = "|")) ~ "Yes",
str_detect(Name, pattern = "Unknown") ~ "Yes",
T ~ "No"))

#Add manually uncertainty for specific strings 
to_recover1 = to_recover1 %>%
mutate(Unsure_id = case_when(
 Old_name == "Halictus sp_scabiosae-alike" ~ "Yes",
 Old_name == "Eristalis sp_tenax-alike" ~ "Yes",
 Old_name == "Eucera sp_collaris-alike" ~ "Yes",
 Old_name == "Syrphidae sp_Eristalis-alike" ~ "Yes",
 Old_name == "Andrena sp_flavipes-alike" ~ "Yes",
 Old_name == "Syrphidae sp_Meliscaeva-alike" ~ "Yes", 
 Old_name == "Lasioglossum sp_immunitum-alike" ~ "Yes",
 Old_name == "Halictus gr simplex" ~ "Yes",
 Old_name == "Lasioglossum immunitum-type" ~ "Yes",
 Old_name == "Chrysosomoxys macrocercus" ~ "Yes",
 Old_name == "Sarginae/Sargus sp.1" ~ "Yes",


 T ~Unsure_id

))


#Now add cols that specify level of uncertainty

species_complex = c("aggr.","agg.","[/]", 
                    "_agg", "_group", "-group",  
                     "-.*-", " agg$", " type",
                    "-type", " complex",
                    " gr ", "-Komplex")

affinis = c("aff(?![:alpha:])", "malachurum-alike")

confer = c(" cf", " Cf", "_c.f")

unsure = c("[?]")

# Add extra col with Uncertainty_type
to_recover1 = to_recover1 %>% 
mutate(Uncertainty_type = case_when(
  str_detect(Old_name, pattern = 
  paste(species_complex, collapse = "|")) ~ "species_complex",
  str_detect(Old_name, pattern = 
  paste(affinis, collapse = "|")) ~ "affinis",
  str_detect(Old_name, pattern = 
  paste(confer, collapse = "|")) ~ "confer",
  str_detect(Old_name, pattern = 
  paste(unsure, collapse = "|")) ~ "unsure",
  str_detect(Old_name, "alike") & Unsure_id == "Yes"  ~ "alike",
  str_detect(Name, pattern = "Unknown") ~ "unknown",
  str_detect(Name, pattern = "Sarginae/Sargus") ~ "unsure",

  T ~ NA_character_
))

#Merge both datasets now
unmatched = to_recover1 %>% 
mutate(Name = case_when(is.na(Name) ~ Old_name,
T ~ Name))


matched = tibble(Old_name = matching, Name = matching) %>% 
mutate(Unsure_id = "No") %>% 
mutate(Uncertainty_type = NA) 

#Rename manually some synonyms (Following GBIF taxonomy)
matched = 
matched %>% 
mutate(Name = case_when(
  Name == "Lycaena hippothoe" ~ "Palaeochrysophanus hippothoe",
  T ~ Name))

#Let's keep matched and unmatched separated for now
#Current issues: We are not keeping subgenus and subspecies.

#----------------------------#
#Time to retrieve from GBIF taxonomic info
#Objetive: standardise spp names, get tax info and 
#recover other spp with fuzzy matching 
#that were not in our master list 
#it contained only syrphids, butterflies and bees
#----------------------------#

#----------------------------#
#3)Retrieve GBIF data-----
#----------------------------#
library(rgbif)  # To lookup names in the GBIF backbone taxonomy
#Let's start with the spp found in our master list
#(Should be easy)
name = matched %>% 
select(Name) %>% 
distinct() %>%
pull()
#Download taxonomic info from GBIF
matched_gbif = name_backbone_checklist(name= name, kingdom='animals')
#Organise structure of data
matched_gbif1 = change_str1(matched_gbif)
#Check species that haven't been found
matched_gbif1 %>% 
filter(is.na(Accepted_name)) %>%
pull(Fixed_name)
#Here we rename the species
#Many lines of code so we keep it in a nother script
source("Scripts/Processing/Pollinators/2_1_Rename_matched_pollinators.R")
#With this 
#we can conclude for now the edits on the matched spp
name1 = unmatched %>% 
select(Name) %>% 
distinct() %>%
pull()
#Download taxonomic info from GBIF
unmatched_gbif = name_backbone_checklist(name= name1, kingdom='animals')
#Rename and filter out exact matches
unmatched_gbif1 = change_str1(unmatched_gbif)
clean = c("EXACT", "FUZZY")
#Select matched records and pass Canonical_name to accepted
#When necessary
unmatched_gbif1_found = unmatched_gbif1 %>% 
filter(Matchtype %in% clean) %>% 
mutate(Accepted_name = 
if_else(is.na(Accepted_name), Canonical_name, Accepted_name))
#Now select not found records and fix manually
unmatched_gbif1_not_found = unmatched_gbif1 %>% 
filter(!Matchtype %in% clean)
#Here we rename the species
#Many lines of code so we keep it in a nother script
source("Scripts/Processing/Pollinators/2_2_Rename_unmatched_pollinators.R")

#Bind datasets
gbif_data = 
bind_rows(unmatched_gbif1_found, 
          unmatched_gbif1_not_found,
          matched_gbif1)
#Bind matched and unmatched ones
processed = bind_rows(matched, unmatched) %>% 
rename(Fixed_name = Name)
#Rename cols before merging!!
poll_data = left_join(gbif_data, processed)
#Add this info 
#This wasn't added because it was accepted with
#the thesaurus (as it is a correct plant species)
poll_data = poll_data %>% 
mutate(Unsure_id = case_when(
  Fixed_name == "Vaccinium vitis-idaea" ~ "Yes",
  T ~ Unsure_id)) %>% 
mutate(Uncertainty_type = case_when(
  Fixed_name == "Vaccinium vitis-idaea" ~ "Mistake",
  T ~ Uncertainty_type))
#Some final safety checkings
s= poll_data %>% 
filter(is.na(Accepted_name))
poll_data %>% 
filter(is.na(Fixed_name))

#Check number 
poll_data %>% 
mutate(n_word = 
str_count(Accepted_name, '\\w+')) %>% 
filter(n_word>1) %>% 
filter(is.na(Unsure_id)) %>% 
distinct(Accepted_name) 
#1580 accepted different species

#----------------------------#
#4)Save pollinator taxonomy----
#----------------------------#
#check colnames
colnames(poll_data)
saveRDS(poll_data, "Data/Species_taxonomy/Pollinator_taxonomy.rds")

#----------------------------#
#5)Safety check by study ID!-----
#----------------------------#
#Each new study is manually evaluated here
#The last study added on filter is the last one checked

#Rename master to old name before merging back
master = master%>%  
rename(Old_name = Pollinator_species)
#Merge
all = left_join(master, poll_data)
colnames(all)
#check levels
levels(factor(all$Study_id))
#Quick subset to explore the new pollinators added
#Do this fo every new dataset that we add
#Last one being checked is written within the filter argument
subset_check = all %>% 
filter(Study_id == "47_Benadi") %>% 
select(Old_name, Fixed_name, Rank, Status, Matchtype, Accepted_name, Unsure_id) %>% 
distinct()

