#create a EU metaweb by 
  #complie all interactions in a single file from
    #safenet  
    #Alham
    #Globi: https://github.com/Big-Bee-Network/global-bee-interaction-data
    #Literature: https://github.com/Big-Bee-Network/bee-interaction-database
  #transform it into a matrix
  #Plot it nicely (if this is possible?)


#create the structure of Plants and pol to be considered:-----
#Polinators: Bee + sirphids 
#Pollinators: Bombilids + beetles? Skip?
#Plants: ??


#start by reading SafeNet data----
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

#comented for security
#write.csv(master, file = "scripts/cleandata/data.csv")
#nice.

#-Pollinator taxomony cleaning ----
#check pollinator names...
pols <- unique(master$Pollinator_species)
length(pols) #2858!!
# Need a deeeeep cleaning.

#To do this we will use cleanR
devtools::install_github("RadicalCommEcol/CleanR", build_vignettes = TRUE)
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

# Clean by strings
to_recover1 <- to_recover1[!grepl("\\d", to_recover1$mismatches),]
to_recover1 <- to_recover1[!grepl("\\_", to_recover1$mismatches),]

unwanted <- c("sp. ","sp.","sp", "spp.","spec. ","spec","sp_","spp"," dp"," ss",
              "Unidentified", "P. ", "aff. ", "unknown",  "NA", "indet", "IMG") 
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
to_recoverpol$manual <- ifelse(is.na(to_recoverpol$wanted), to_recoverpol$goodr_id, to_recoverpol$goodr_id)
to_recoverpol$manual[to_recoverpol$wanted == "Andrena ovulata"] <- "Andrena ovatula"
to_recoverpol$manual[to_recoverpol$fixed == "Osmia nuda"] <- "Osmia rufa"

# Add column for unsure ids: yes/no
# Entries that fall in unsure are: the ones with ?, aggr and variants, complex (or /) and cf.
unsure <- c("type", "agg","[/]" ," cf", " cf.") #no me gusta nada tener que poner spinosa pero no sé por qué falla
to_recoverpol$unsureID <-  NA
to_recoverpol <- to_recoverpol %>% mutate(unsureID = ifelse(str_detect(mismatches, str_c("(?i)\\b(", str_c(unsure, collapse = "|"), ")\\b")),
                                                          "Yes", "No"))
to_mergepol <- to_recoverpol[[-2:4]]

#need to go one by one, some ? removed,   
#Osmia rufa                  Osmia nuda is wrong - DONE
#But in general good job!

clean_data1 <- merge(master, to_mergepol, by.x = "Pollinator_species", by.y = "mismatches", all.x = TRUE)
clean_data1$used_Gen_sp_Pollinator <- ifelse(is.na(clean_data1$manual), clean_data1$Pollinator_species,
                                clean_data1$fixed)

#clean_data$fixed <- NULL #keep track
#Remove non recognizes sp.
head(clean_data1)
unmatching <- clean_data1$used_Gen_sp[which(!clean_data1$used_Gen_sp %in% species_tesaurus$Gen_sp)]
#this throws out a lot of good species e.g. with subspecies in it.
clean_data2 <- clean_data[-which(clean_data1$used_Gen_sp %in% unmatching),]
clean_data2 <- clean_data2[,-20:-22] # just to remove extra fixed columns that were redundant
clean_data2
head(clean_data2)
length(unique(clean_data2$used_Gen_sp)) #864 pollinators!


#____________________________________________________________________
#- Plant taxonomy cleaning -----
plants <- unique(master$Plant_species)
length(plants) #1739!!
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
to_recover <- data.frame(mismatches, fixed, stringsAsFactors = FALSE)
to_recover
#}
head(to_recover)
to_recover[which(!is.na(to_recover$fixed)),]

# Clean by strings
to_recover <- to_recover[!grepl("\\d", to_recover$mismatches),] #remove all entries with numbers in

to_recoverplant <- to_recover2[which(!is.na(to_recover2$mismatches)) & !to_recover2$mismatches == "?" & 
                           !to_recover2$mismatches == "NA NA",]

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

unwanted <- "sp.| sp| Unknown| spp| NA| spec|_sp|_cult| rara| cult" 
# Vector of the patterns we want to delete

to_recoverplant <- to_recoverplant %>% 
  filter(!str_detect(wanted,str_c("(?i)\\b(", str_c(unwanted), ")\\b"))) 
# Extract list of names with those patterns

to_recoverplant$wanted <- word(to_recoverplant$wanted, 1, 2, sep = " ") #leave just the two first words of all entries

unwanted <- "sp.| sp| Unknown| spp| NA| spec|_sp|_cult| rara| cult| agg" 
# Vector of the patterns we want to delete

to_recoverplant <- to_recoverplant %>% 
  filter(!str_detect(wanted,str_c("(?i)\\b(", str_c(unwanted), ")\\b"))) 


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
char_class("?")
unsure <- "aggr.|agg.| spinosa?|[/]" #no me gusta nada tener que poner spinosa pero no sé por qué falla
to_recoverplant$unsureID <-  NA
to_recoverplant <- to_recoverplant %>% mutate(unsureID = ifelse(str_detect(mismatches, str_c("(?i)\\b(", str_c(unsure), ")\\b")),
                                                 "Yes", "No"))
# Columns to merge
to_mergeplant <- to_recoverplant[[2:4]]

#Roughly clean, a lot of names missing from the tesaurus or synonyms?

clean_data3 <- merge(master, to_mergeplant, by.x = "Plant_species", by.y = "mismatches", all.x = TRUE)
clean_data3$used_Gen_sp_plants <- ifelse(is.na(clean_data3$manual), clean_data3$Pollinator_species,
                                 clean_data3$fixed)

#clean_data$fixed <- NULL #keep track
#Remove non recognizes sp.
head(clean_data3)
unmatching <- clean_data3$used_Gen_sp_plants[which(!clean_data3$used_Gen_sp_plants %in% species_tesaurus$Gen_sp)]
#this throws out a lot of good species e.g. with subspecies in it.
clean_data4 <- clean_data[-which(clean_data$used_Gen_sp_plants %in% unmatching),]
clean_data4
head(clean_data4)
length(unique(clean_data4$used_Gen_sp_plants)) #1155 plants

#____________________________________________________________________
#Clean data for plants and pollinators
clean_data <- merge(clean_data1, clean_data3, by.x = "Pollinator_species", by.y = "", all.x = TRUE)%>% 
              merge(to_recoverpol, by.y = mismatches)
clean_data$used_Gen_sp_plants <- ifelse(is.na(clean_data$manual), clean_data$Pollinator_species,
                                        clean_data$fixed)



#____________________________________________________________________

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

#Read UMONS data----
mons <- read.csv("scripts/temp_data/Network_data_Umons_27_III_2023_V1.csv")
head(mons)
#need to recover pol names (most in caps and with subspecies)
#Sex and N seems easy
#Date is DAT1 and 2? 
#Plant (unify)
#Lat/long and Topo, Loc


#Globi----


#Iberian bees----






