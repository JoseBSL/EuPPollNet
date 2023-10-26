# ---- EU METAWEB PREPARATION PROCESS ----
# A master file is created combining SafeNet (JBL data compilation) and other datasets.
# Overview of datasets included (29/sep)----
    # - SafeNet (40 networks, years?, countries?)
    # - Umons (Alham, Museum collections and other pairwise interactions, 1850-2021, countries?)
    # - Globi: https://github.com/Big-Bee-Network/global-bee-interaction-data (Not done)
    # - Literature: https://github.com/Big-Bee-Network/bee-interaction-database

# Overview of the structure of Plants and Pollinator to be considered-----
# Pollinators1: Bee + Syrphids + Butterflies (with taxonomy and reference species names)
# Pollinators2: Bombilids + Beetles + Sawflies (missing taxonomy and reference species names) (not included, 29/sep)
# Plants: first taxonomic reference obtained, missing synonyms? 


# 0 Read SafeNet data----
#Ideally from the repo once public. Now I made a copy in my personal computer

#Example of one file.
temp <- readRDS("scripts/temp_data/Clean_data/1_Bartomeus.rds")
#str(temp)
#temp$Metadata #Most fields already in interaction data (e.g. where and when)
#temp$Authorship #Most fields already in interaction data (e.g. where and when)
#merge all sites
out<- temp$InteractionData[[1]]
for(i in 2:length(temp$InteractionData)){
  temp2 <- temp$InteractionData[[i]]
  out <- rbind(temp2, out)
}
#head(out)
#dim(out)
colnames(out)

library(dplyr)
library(stringr)
library(data.table)
library(taxize)
library(cleanR)
#loop through all files
#Read all file names to loop.
files <- list.files(path = "Data/Clean_data")
master <- data.frame(Plant_species = NA   ,Pollinator_species = NA   ,Interaction = NA   ,      
                     Sampling_method = NA ,Site_id = NA              ,Habitat = NA       ,     
                     Country = NA         ,Locality = NA             ,Latitude = NA      ,      
                     Longitude = NA       ,Coordinate_precision = NA ,Elevation = NA     ,        
                     Day = NA             ,Month = NA                ,Year = NA          ,      
                     Comments = NA        ,Temperature = NA          ,Humidity = NA      ,
                     Study = NA)
for(k in 1:length(files)){
  temp <- readRDS(paste0("Data/Clean_data/", files[k]))
  #merge all sites
  out<- temp$InteractionData[[1]]
  if(length(temp$InteractionData) > 1){
    for(i in 2:length(temp$InteractionData)){
      temp2 <- temp$InteractionData[[i]]
      out <- rbind(temp2, out)
    }
  }
  out$Study <- files[k]
  if(colnames(out)[1] == "Survey"){
    out <- out[,-1]
  }
  colnames(out)[18] <- ifelse(colnames(out)[18] == "Humidiy", "Humidity", "Humidity")
  master <- rbind(master, out)
}
master <- master[-1,]
head(master)
master <- master %>% mutate(observation = 1:n()) %>% select(observation, everything())

# Bind extra datasets (Umons, GLobi, etc.)
# yet to be done when Umons dataset is moved to strange data folder (29/sep)

#comented for security
#write.csv(master, file = "scripts/cleandata/data.csv")

#________________________________________________________________________________----
# 1. Pollinator taxomony cleaning ----
#- 1.a Pollinator names check and mismatches extraction ----
pols <- unique(master$Pollinator_species)
length(pols) #3799!!
# Need a deeeeep cleaning.

#To do this we will use cleanR
#devtools::install_github("RadicalCommEcol/CleanR", build_vignettes = TRUE)
library(cleanR)

#Go old school as devtools is not installing
tesaurus <- readRDS(file = "Data/Species_thesaurus/taxonomy.rds")
head(tesaurus)
#check_sp <- function(template, Gen_sp, k = 2){ #if Gen_sp, we can add an if.
  #Gen_sp <- paste(trimws(Genus),
  #                trimws(Species))
  Gen_sp <- pols
  species_tesaurus <- tesaurus #edited to pass a dataframe with Genus, Species
  species_tesaurus$Gen_sp <- paste(trimws(species_tesaurus$Genus),
                                   trimws(species_tesaurus$Species))
  matching <- Gen_sp[which(Gen_sp %in% species_tesaurus$Gen_sp)]
  unmatching <- Gen_sp[which(!Gen_sp %in% species_tesaurus$Gen_sp)]
  mismatches <- unique(unmatching) #speed up the process
  #print(paste("the following species do not match:", mismatches))
  fixed <- c()
  #agrep is too lax, and I can't make it to work, adist is better
  #agrep(c("Coleoxys"), genus, value = TRUE, max = list(all = 2))
  #agrep(c("Lasius"), genus, value = TRUE, max = list(ins = 3, del = 3, sub = 2))
  k = 2
  for(i in 1:length(mismatches)){
    temp2 <- species_tesaurus$Gen_sp[as.logical(adist(mismatches[i],
                                                      species_tesaurus$Gen_sp) <= k)]
    if(length(temp2) == 1){
      fixed[i] <- temp2
    } else {
      fixed[i] <- NA
    }
  }
to_recover1 <- data.frame(mismatches, fixed, stringsAsFactors = FALSE)
to_recover1

head(to_recover1)
to_recover1[which(!is.na(to_recover1$fixed)),]

# Takes a while to run, create rds every new dataset addition to clean faster
#saveRDS(to_recover1, file="Data/Processing/Pollinator_names.rds")

# 1.b Fix mismatches: manual fix and synonyms using cleanR/taxize----
to_recover1 <- readRDS(file="Data/Processing/Pollinator_names.rds")

# Clean by strings
to_recover1 <- to_recover1[!grepl("\\d", to_recover1$mismatches),]
to_recover1$mismatches <- str_replace(to_recover1$mismatches, "\\_", " ") # delete parenthesis of the string              

unwanted <- c("sp. ","sp.","sp", "spp.","spec. ","spec","sp_","spp"," dp"," ss",
              "Unidentified","Unidentified ", "P. "," aff. ", "unknown",  "NA", "indet", "IMG") 
# Vector of the patterns we want to delete: multiple variations of sp, / for the Bombus complex

to_recoverpol <- to_recover1 %>% 
  filter(!str_detect(mismatches, str_c("(?i)\\b(", str_c(unwanted, collapse = "|"), ")\\b"))) 
# Extract list of names with that don't fit those patterns

to_recoverpol$wanted <- str_replace(to_recoverpol$mismatches, "\\([^\\)]+\\)", "") # delete parenthesis of the string              
to_recoverpol$wanted <- str_replace(to_recoverpol$wanted, "\\s+\\s", " ") # delete parenthesis of the string              
to_recoverpol$wanted <- str_remove(to_recoverpol$wanted, "-type")
to_recoverpol$wanted <- str_remove(to_recoverpol$wanted, "-alike")

dot <- "\\." #code to recognise the . as a real . in a string pattern 
writeLines(dot) #code to recognise the . as a real . in a string pattern 
to_recoverpol$wanted <- str_replace(to_recoverpol$wanted, "\\s+cf\\.", "") #to delete the cf. of the string
to_recoverpol$wanted <- str_replace(to_recoverpol$wanted, "\\s+cf", "") #to delete the cf. of the string
to_recoverpol$wanted <- str_replace(to_recoverpol$wanted, "\\.", " ") #delete parenthesis of the string              
to_recoverpol$wanted <- str_replace(to_recoverpol$wanted, "\\s+gr", "") #to delete the cf. of the string
to_recoverpol$wanted <- str_replace(to_recoverpol$wanted, "\\.agg\\.", "") #to delete the cf. of the string

to_recoverpol$wanted <- str_remove(to_recoverpol$wanted, "[?]") #to delete the ? of the string
to_recoverpol$wanted <- str_remove(to_recoverpol$wanted, "[?]") #to delete the ? of the string
to_recoverpol$wanted <- str_remove(to_recoverpol$wanted, "[?]") #to delete the ? of the string

# This is not pretty, each time you run it deletes one ?, so trhee lines for ???
# There should be a better way to write it

to_recoverpol$wanted <- word(to_recoverpol$wanted, 1, 2, sep = " ") #leave just the two first words of all entries

to_recoverpol$goodr_id <- ifelse(is.na(to_recoverpol$fixed), to_recoverpol$wanted,
                                 to_recoverpol$fixed)

# Manual checks of fixed species
# Changes in correction misspelling
to_recoverpol$manual <- ifelse(is.na(to_recoverpol$wanted), to_recoverpol$goodr_id, to_recoverpol$goodr_id)
to_recoverpol$manual[to_recoverpol$wanted == "Andrena ovulata"] <- "Andrena ovatula"
to_recoverpol$manual[to_recoverpol$fixed == "Osmia nuda"] <- "Osmia rufa"

to_recoverpol$manual <- str_replace(to_recoverpol$manual, "Chalicodoma", "Megachile")
to_recoverpol$manual <- str_replace(to_recoverpol$manual, "croceus", "crocea")
to_recoverpol$manual <- str_replace(to_recoverpol$manual, "altercator", "hirtipes")
to_recoverpol$manual <- str_replace(to_recoverpol$manual, "sinufascia", "penicillata")
to_recoverpol$manual <- str_replace(to_recoverpol$manual, "patella", "patellatus")
to_recoverpol$manual <- str_replace(to_recoverpol$manual, "imminutus", "immunitum")
to_recoverpol$manual <- str_replace(to_recoverpol$manual, pattern = "Psithyrus |psithyrus |Psythrus ", "Bombus ")
to_recoverpol$manual <- str_replace(to_recoverpol$manual, pattern = "Tetraloniella", "Tetralonia")
to_recoverpol$manual <- str_replace(to_recoverpol$manual, pattern = "Evylaeus ", "Lasioglossum ")

# Add column for unsure ids: yes/no
# Entries that fall in unsure are: the ones with ?, aggr and variants, complex (or /) and cf.
unsure <- "type| agg |.+_agg|.+?|[/]"
cf <- c(" cf", " cf.")
to_recoverpol$unsureID <-  NA
to_recoverpol <- to_recoverpol %>% mutate(unsureID = 
                                            ifelse(str_detect(mismatches, str_c("(?i)\\b(", str_c(unsure, collapse = "|"), ")\\b")),                                                       "Yes", 
                                            ifelse(str_detect(mismatches, str_c("(?i)\\b(", str_c(cf, collapse = "|"), ")\\b")), "cf","No")))


to_mergepol <- to_recoverpol[,-c(2:4)] # total of 1732 cases
to_mergepolNA <- to_mergepol[!is.na(to_mergepol$manual),] # without the NAs 1649

check_syn <- merge(to_mergepol, tesaurus[, c("GenusAndSpecies", "Order")], by.x = "manual", by.y = "GenusAndSpecies", all.x = T, all.y= F)
check_snNA <- check_syn[is.na(check_syn$Order),]

# 1.c Synonyms check (NOT RUN IF to_recoverpol has =< 1732 observations(up to 29/sep))----
# Functions like synonyms or tax_name will take a long time to run
# Cleaning with cleanR looking for synonyms
# CleanR not working, thus using the funcion directly
# -- This is the funcion clean_species from CleanR package
# getAnywhere(clean_species)
# fixed_synonyms <- clean_species(check_snNA$manual, verbose = t) 

specnames <- gnr_resolve(check_snNA$manual, canonical = F, best_match_only = T)
dat <- merge(data.frame(check_snNA), specnames[, c("user_supplied_name", 
                                            "matched_name")], by.x = "manual", by.y = "user_supplied_name", 
             all.x = TRUE)
species3 <- unique(dat$matched_name)
species3 <- species3[!is.na(species3)]
syn <- synonyms(specnames$matched_name, db = "itis")
synonym_ids <- grep(pattern = "acc_name", syn)
accepted_names <- unlist(lapply(syn[synonym_ids], "[", "acc_name"), 
                         use.names = FALSE)
synonym_names <- species3
synonym_names[synonym_ids] <- accepted_names[1]
key <- data.frame(species3, synonym_names)
dat <- merge(dat, key, by.x = "matched_name", by.y = "species3", 
             all.x = TRUE)
species4 <- unique(dat$synonym_names)
species4 <- species4[!is.na(species4)]
out2 <- tax_name(as.character(species4), get = "species", 
                 db = "itis")
out2_u <- unique(out2$species)
final_names <- species4
final_names[which(!species4 %in% out2_u)] <- NA
key2 <- data.frame(species4, final_names)
dat <- merge(dat, key2, by.x = "synonym_names", by.y = "species4", 
             all.x = TRUE)
dat <- merge(data.frame(check_snNA), dat, by.x = "manual", 
             by.y = "manual", all.x = TRUE)
dat[, c(1, 3, 2, 4)]

# Final product saved to a RDS file as it takes forever
#saveRDS(dat, file = "Data/Processing/Taxize_check.rds")

# 1.d Last adjustments and merge with master data ----
checked_names <- readRDS("Data/Species_thesaurus/Taxize_check.rds")[,-c(4, 7:9)]
unsure <- c("type","aggr.","agg.","[/]")
cf <- c(" cf", " cf.")
checked_names <- checked_names %>% mutate(unsureID_Pollinator = 
                                            ifelse(str_detect(mismatches.x, pattern = str_c("(?i)\\b(", str_c(unsure, collapse = "|"), ")\\b")),"Yes", 
                                            ifelse(str_detect(mismatches.x, str_c("(?i)\\b(", str_c(cf, collapse = "|"), ")\\b")), "cf","No")))

# Extra step adding ? to the unsure data: it seems like ? can't be recognized in the previous step
checked_names <- checked_names %>% mutate(unsureID_Pollinator = 
                                            ifelse(str_detect(mismatches.x, '[[:punct:]]'),"Yes", unsureID_Pollinator))


checked_names <- checked_names[,-c(3:4, 7:9)]
clean_data1 <- merge(master, checked_names, by.x = "Pollinator_species", by.y = "mismatches.x", all.x = TRUE)
# Creates some kind of duplicates, over 200000 records more than the master
clean_data1$used_Gen_sp_Pollinator <- ifelse(is.na(clean_data1$final_names), ifelse(is.na(clean_data1$manual), 
                                                                                    clean_data1$Pollinator_species, clean_data1$manual),
                                clean_data1$final_names)

#clean_data$fixed <- NULL #keep track
#Remove non recognizes sp.
head(clean_data1)
unmatching <- clean_data1$used_Gen_sp_Pollinator[which(!clean_data1$used_Gen_sp_Pollinator %in% species_tesaurus$Gen_sp)]
#this throws out a lot of good species e.g. with subspecies in it.
clean_data2 <- clean_data1[-which(clean_data1$used_Gen_sp_Pollinator %in% unmatching),]
clean_data2
head(clean_data2)
length(unique(clean_data2$used_Gen_sp)) #1178 pollinators!

#________________________________________________________________________________----
# 2. Plant taxonomy cleaning -----
#- 2.a Plant names check and mismatches extraction ----
plants <- unique(master$Plant_species)
length(plants) #3582!!
plants

#Go old school as devtools is not installing
tesaurus <- readRDS(file = "Data/Species_thesaurus/taxonomy.rds")
head(tesaurus)
Gen_sp <- plants
species_tesaurus <- tesaurus #edited to pass a dataframe with Genus, Species
species_tesaurus$Gen_sp <- paste(trimws(species_tesaurus$Genus),
                                 trimws(species_tesaurus$Species))
matching <- Gen_sp[which(Gen_sp %in% species_tesaurus$Gen_sp)]
unmatching <- Gen_sp[which(!Gen_sp %in% species_tesaurus$Gen_sp)]
mismatches <- unique(unmatching) #speed up the process
#print(paste("the following species do not match:", mismatches))
fixed <- c()
#agrep is too lax, and I can't make it to work, adist is better
#agrep(c("Coleoxys"), genus, value = TRUE, max = list(all = 2))
#agrep(c("Lasius"), genus, value = TRUE, max = list(ins = 3, del = 3, sub = 2))
k = 2
for(i in 1:length(mismatches)){
  temp2 <- species_tesaurus$Gen_sp[as.logical(adist(mismatches[i],
                                                    species_tesaurus$Gen_sp) <= k)]
  if(length(temp2) == 1){
    fixed[i] <- temp2
  } else {
    fixed[i] <- NA
  }
}

to_recover2 <- data.frame(mismatches, fixed, stringsAsFactors = FALSE)
to_recover2
head(to_recover2)
to_recover2[which(!is.na(to_recover2$fixed)),] #1952

#saveRDS(to_recover2, file="Data/Processing/Plants_names.rds")

# 2.b Fix mismatches: manual fix and synonyms using cleanR/taxize (plants yet to be cleaned with CleanR----
to_recover2 <- readRDS(file="Data/Processing/Plants_names.rds")

# Clean by strings
to_recover2 <- to_recover2[!grepl("\\d", to_recover2$mismatches),] #remove all entries with numbers in

to_recoverplant <- to_recover2[!is.na(to_recover2$mismatches) & !to_recover2$mismatches == "NA NA",]
# 1938

dot <- "\\." #code to recognise the . as a real . in a string pattern 
writeLines(dot) #code to recognise the . as a real . in a string pattern 
to_recoverplant$wanted <- str_replace(to_recoverplant$mismatches, "\\.", " ") #delete parenthesis of the string              
to_recoverplant$wanted <- str_replace(to_recoverplant$wanted, "\\_", " ") #delete parenthesis of the string              
to_recoverplant$wanted <- str_remove(to_recoverplant$wanted, "\\.agg\\.") #to delete the cf. of the string
to_recoverplant$wanted <- str_remove(to_recoverplant$wanted, "\\.aggr\\.") #to delete the cf. of the string
to_recoverplant$wanted <- str_remove(to_recoverplant$wanted, "\\.aggr") #to delete the cf. of the string
to_recoverplant$wanted <- str_remove(to_recoverplant$wanted, "\\(") # delete parenthesis of the string              
to_recoverplant$wanted <- str_remove(to_recoverplant$wanted, "\\)") # delete parenthesis of the string              
to_recoverplant$wanted <- str_remove(to_recoverplant$wanted, " [?]") #to delete the ? of the string

# Create pattern vector to filter out those entries that match the pattern
unwanted <- "sp.| sp| Unknown| spp| NA| spec|_sp|_cult| rara| cult" 
# Vector of the patterns we want to delete
to_recoverplant <- to_recoverplant %>% 
  filter(!str_detect(wanted,str_c("(?i)\\b(", str_c(unwanted), ")\\b"))) 
# Extract list of names with those patterns: 1477

to_recoverplant$wanted <- word(to_recoverplant$wanted, 1, 2, sep = " ") #leave just the two first words of all entries

#- Create pattern vector to filter out those entries that match the pattern
# (We run this twice for a few cases that were no recognized before)
unwanted <- "sp.| sp| Unknown| spp| NA| spec|_sp|_cult| rara| cult| agg" 
# Vector of the patterns we want to delete
to_recoverplant <- to_recoverplant %>% 
  filter(!str_detect(wanted,str_c("(?i)\\b(", str_c(unwanted), ")\\b"))) 

# Create a pre manual check column with all good ids: fixed ones and cleaned wanted ones
to_recoverplant$goodr_id <- ifelse(is.na(to_recoverplant$fixed), to_recoverplant$wanted,
                                to_recoverplant$fixed)
# Manual checks of fixed species
to_recoverplant$manual_plant <- to_recoverplant$goodr_id

patterns <- " x| X "
to_recoverplant <- to_recoverplant %>% 
  mutate(manual = ifelse(str_detect(goodr_id, str_c("(?i)\\b(", str_c(patterns), ")\\b")), mismatches, goodr_id))
to_recoverplant$manual[to_recoverplant$manual == "Rosmarinus officinalis"] <- 'Salvia rosmarinus'

# Add column for unsure ids: yes/no
# Entries that fall in unsure are: the ones with ?, aggr and variants, complex (or /) and cf.
unsure <- "aggr.|agg.| [/]" 
to_recoverplant$unsureID_Plants <-  NA
to_recoverplant <- to_recoverplant %>% mutate(unsureID_Plants = ifelse(str_detect(mismatches, str_c("(?i)\\b(", str_c(unsure), ")\\b")),
                                                 "Yes", "No"))
# Similar to the pollinator step, 
to_recoverplant <- to_recoverplant %>% mutate(unsureID_Plants = ifelse(str_detect(mismatches, '[[:punct:]]'),
                                                                       "Yes", unsureID_Plants))
# Columns to merge
to_mergeplant <- to_recoverplant[,-c(2:4)] 

#Roughly clean, a lot of names missing from the tesaurus or synonyms?

# 2.c Last adjustments and merge with master data ----
clean_data3 <- merge(master, to_mergeplant, by.x = "Plant_species", by.y = "mismatches", all.x = TRUE)
clean_data3$used_Gen_sp_plants <- ifelse(is.na(clean_data3$manual), clean_data3$Plant_species,
                                 clean_data3$manual)

#clean_data$fixed <- NULL #keep track
#Remove non recognizes sp.
head(clean_data3)
unmatching <- clean_data3$used_Gen_sp_plants[which(!clean_data3$used_Gen_sp_plants %in% species_tesaurus$Gen_sp)]
#this throws out a lot of good species e.g. with subspecies in it.
clean_data4 <- clean_data3[-which(clean_data3$used_Gen_sp_plants %in% unmatching),]
clean_data4
head(clean_data4)
length(unique(clean_data4$used_Gen_sp_plants)) #1727 plants

#________________________________________________________________________________----
# 3. Create names cleaned dataset: merge plant and pollinator results----

clean_data <- merge(clean_data2[, -c(21:23)], clean_data4[,c(1:3,23,24)], by = c("observation", "Pollinator_species", "Plant_species"), 
                    all.y = F, all.x = F)
interactions <- paste0(clean_data$used_Gen_sp_plants, "_", clean_data$used_Gen_sp_Pollinator)

# For a total of 121333 records
length(unique(interactions)) #17502 unique interactions
length(unique(clean_data$used_Gen_sp_plants)) # 1527 plant species
length(unique(clean_data$used_Gen_sp_Pollinator)) # 1096 pollinator species

# Save preliminary dataset before other further cleaning
#saveRDS(clean_data, "Data/Processing/Preliminary_cleandata.rds")

#____________________________________________________________________
# 4. Final touches: clean countries----


#____________________________________________________________________
# 5. Plotting----
#Can we plot it nicely?

library(reshape2)
master <- clean_data2
master$Interaction <- as.numeric(master$Interaction) #WARNING: This means something is wrong in the original data
master$Interaction[which(is.na(master$Interaction))] <- 1
metaweb <- dcast(data = master, Plant_species ~ used_Gen_sp, 
                 fun.aggregate = sum, value.var = "Interaction")
head(metaweb)
#heatmap(as.matrix(metaweb[,-1])) #slow and ugly!

# reduce matrix size, using a summarising function (default, mean)
redim_matrix <- function( #from https://gdevailly.netlify.app/post/plotting-big-matrices-in-r/
    mat,
    target_height = 100,
    target_width = 100,
    summary_func = function(x) mean(x, na.rm = TRUE),
    output_type = 0.0, #vapply style
    n_core = 1 # parallel processing
    ) {
  if(target_height > nrow(mat) | target_width > ncol(mat)) {
    stop("Input matrix must be bigger than target width and height.")
  }
  seq_height <- round(seq(1, nrow(mat), length.out = target_height + 1))
  seq_width  <- round(seq(1, ncol(mat), length.out = target_width  + 1))
  # complicated way to write a double for loop
  do.call(rbind, parallel::mclapply(seq_len(target_height), function(i) { # i is row
    vapply(seq_len(target_width), function(j) { # j is column
      summary_func(
        mat[
          seq(seq_height[i], seq_height[i + 1]),
          seq(seq_width[j] , seq_width[j + 1] )
        ]
      )
    }, output_type)
  }, mc.cores = n_core))
}
metawebred <- redim_matrix(as.matrix(metaweb[,-1]), target_height = 600, target_width = 50,
                           summary_func = function(x) max(x, na.rm = TRUE)) # 600 is very roughly the pixel height of the image.
#heatmap(metawebred) #nicer
#sorted by rowSums
metawebred <- metawebred[order(rowSums(metawebred), decreasing = T),]
metawebred <- metawebred[,order(colSums(metawebred), decreasing = F)]

#nicer plot
image(
  metawebred,
  axes = FALSE,
  col = colorRampPalette(c("white", "darkorange", "black"))(30),
  breaks = c(seq(0, 3, length.out = 30), 100),
  main = "EU metaweb"
)


#Globi----


#Iberian bees----






