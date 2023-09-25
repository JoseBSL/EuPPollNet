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
files <- list.files(path = "scripts/temp_data/Clean_data")
master <- data.frame(Plant_species = NA   ,Pollinator_species = NA   ,Interaction = NA   ,      
                     Sampling_method = NA ,Site_id = NA              ,Habitat = NA       ,     
                     Country = NA         ,Locality = NA             ,Latitude = NA      ,      
                     Longitude = NA       ,Coordinate_precision = NA ,Elevation = NA     ,        
                     Day = NA             ,Month = NA                ,Year = NA          ,      
                     Comments = NA        ,Temperature = NA          ,Humidity = NA      ,
                     Study = NA)
for(k in 1:length(files)){
  temp <- readRDS(paste0("scripts/temp_data/Clean_data/", files[k]))
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

#check pollinator names...
pols <- unique(master$Pollinator_species)
length(pols) #2858!!
# Need a deeeeep cleaning.
plants <- unique(master$Plant_species)
length(plants) #1739!!

#To do this we will use cleanR
devtools::install_github("RadicalCommEcol/CleanR", build_vignettes = TRUE)
library(cleanR)

#Go old school as devtools is not installing
#tesaurus <- read.csv(file = "scripts/temp_data/Master_bees_syrphids.csv")
tesaurus <- readRDS(file = "scripts/cleandata/taxonomy.rds")
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
  to_recover <- data.frame(mismatches, fixed, stringsAsFactors = FALSE)
  to_recover
#}
head(to_recover)
to_recover[which(!is.na(to_recover$fixed)),]

# Clean by strings
to_recover <- to_recover[!grepl("\\d", to_recover$mismatches),]
to_recover <- to_recover[!grepl("\\_", to_recover$mismatches),]

unwanted <- c("sp. ","sp.","sp", "spp.","spec. ","spec","sp_","spp"," dp"," ss",
              "Unidentified",  "/", "P. ", "aff. ", "unknown",  "NA", "indet", "IMG") 
# Vector of the patterns we want to delete: multiple variations of sp, / for the Bombus complex

to_recoverunwanted <- to_recover %>% 
  filter(str_detect(mismatches, str_c("(?i)\\b(", str_c(unwanted, collapse = "|"), ")\\b"))) 
# Extract list of names with those patterns

to_recoversp <- to_recover[!to_recover$mismatches %in% to_recoverunwanted$mismatches,] #leave only the wanted entries (no sp, spp, unidentified etc.)

to_recoversp$wanted <- str_replace(to_recoversp$mismatches, "\\([^\\)]+\\)", "") # delete parenthesis of the string              
to_recoversp$wanted <- str_replace(to_recoversp$wanted, "\\s+\\s", " ") # delete parenthesis of the string              
to_recoversp$wanted <- str_replace(to_recoversp$wanted, "-type", "")

dot <- "\\." #code to recognise the . as a real . in a string pattern 
writeLines(dot) #code to recognise the . as a real . in a string pattern 
to_recoversp$wanted <- str_replace(to_recoversp$wanted, "\\s+cf\\.", "") #to delete the cf. of the string
to_recoversp$wanted <- str_replace(to_recoversp$wanted, "\\s+cf", "") #to delete the cf. of the string
to_recoversp$wanted <- str_replace(to_recoversp$wanted, "\\.", " ") #delete parenthesis of the string              
to_recoversp$wanted <- str_replace(to_recoversp$wanted, "\\s+gr", "") #to delete the cf. of the string
to_recoversp$wanted <- str_replace(to_recoversp$wanted, "\\.agg\\.", "") #to delete the cf. of the string

to_recoversp$wanted <- str_replace(to_recoversp$wanted, "[?]", "") #to delete the ? of the string
to_recoversp$wanted <- str_replace(to_recoversp$wanted, "[?]", "") #to delete the ? of the string
to_recoversp$wanted <- str_replace(to_recoversp$wanted, "[?]", "") #to delete the ? of the string
# This is not pretty, each time you run it deletes one ?, so trhee lines for ???
# There should be a better way to write it

to_recoversp$wanted <- word(to_recoversp$wanted, 1, 2, sep = " ") #leave just the two first words of all entries

to_recoversp$goodr_id <- ifelse(is.na(to_recoversp$fixed), to_recoversp$wanted,
                                 to_recoversp$fixed)
# Manual checks of fixed species
to_recoversp$manual <- ifelse(is.na(to_recoversp$wanted), to_recoversp$goodr_id, to_recoversp$goodr_id)
to_recoversp$manual[to_recoversp$wanted == "Andrena ovulata"] <- "Andrena ovatula"
to_recoversp$manual[to_recoversp$fixed == "Osmia nuda"] <- "Osmia rufa"

#need to go one by one, some ? removed,   
#Osmia rufa                  Osmia nuda is wrong
#But in general good job!

clean_data <- merge(master, to_recoversp, by.x = "Pollinator_species", by.y = "mismatches", all.x = TRUE)
clean_data$used_Gen_sp <- ifelse(is.na(clean_data$manual), clean_data$Pollinator_species,
                                clean_data$fixed)

#clean_data$fixed <- NULL #keep track
#Remove non recognizes sp.
head(clean_data)
unmatching <- clean_data$used_Gen_sp[which(!clean_data$used_Gen_sp %in% species_tesaurus$Gen_sp)]
#this throws out a lot of good species e.g. with subspecies in it.
clean_data2 <- clean_data[-which(clean_data$used_Gen_sp %in% unmatching),]
clean_data2 <- clean_data2[,-20:-22] # just to remove extra fixed columns that were redundant
clean_data2
head(clean_data2)
length(unique(clean_data2$used_Gen_sp)) #864 pollinators!

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






