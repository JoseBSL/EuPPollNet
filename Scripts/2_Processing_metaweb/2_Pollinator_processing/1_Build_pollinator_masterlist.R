#Create master taxonomic and distribution files:

#Load libraries
library(reshape2)

#Bees and syrphids----
#Load data
polBS <- read.csv(file = "Data/Species_thesaurus/Master_bees_syrphids.csv")
head(polBS)
#extract taxonomy
taxonomy <- polBS[,1:9]
#extract distribution
colnames(polBS) #countries to match.
#write.csv(colnames(polBS), "scripts/temp_data/country_tesaurus_init.csv")
#This file form the bases to create manually country_tesaurus.csv
#Make long format
distribution1 <- melt(data = polBS, id.vars = "GenusAndSpecies", measure.vars = 10:ncol(polBS), 
      value.name = "status", variable.name = "country")
head(distribution1)
#We remove absences
distribution2 <- subset(distribution1, status != "A")
head(distribution2)
#we unify country names according to tesaurus
tesaurus <- read.csv("Data/Species_thesaurus/country_tesaurus.csv")
head(tesaurus)
distribution <- merge(distribution2, tesaurus[,2:3], by.x = "country", by.y = "Bees_syrphids")
head(distribution)
distribution <- distribution[,-1]
head(distribution)

#Buterflies----
#taxonomy
taxBt <- read.delim(file = "Data/Species_thesaurus/Butterfly_taxon.txt",
                    header = T)
head(taxBt) #can be merged by id
#subset species as taxonRank
unique(taxBt$taxonRank) #unecessary, but alas...
taxBt <- subset(taxBt, taxonRank == "species")
taxonomyBT <-  taxBt[, c("order", "family", "genus",
                         "specificEpithet", "scientificName", 
                         "scientificNameAuthorship")] 
#merge with taxonomy
head(taxonomy)
taxonomyBT$Subfamily <- NA
taxonomyBT$Tribe <- NA
taxonomyBT$Subgenus <- NA
taxonomyBT <-  taxonomyBT[, c("order", "family", "Subfamily", "Tribe", "genus",
                              "Subgenus", "specificEpithet", "scientificName", 
                              "scientificNameAuthorship")] 
colnames(taxonomyBT) <- colnames(taxonomy)
taxonomy <- rbind(taxonomy, taxonomyBT)
head(taxonomy); tail(taxonomy)

#distribution
polBt <- read.delim(file = "Data/Species_thesaurus/Butterfly_distribution.txt",
                    header = T)
head(polBt)
unique(polBt$countryCode) #We need to match this... 
#export to create country_tesaurus.csv
#write.csv(unique(polBt[,c(3,4)]), "scripts/temp_data/country_temp.csv")
head(taxBt) #can be merged by id
polBt <- merge(polBt, taxBt[, c(2,3)])
head(polBt)
polBt <- polBt[, c("scientificName", "locality", "occurrenceStatus")]
distribution <- distribution[,c("GenusAndSpecies", "Countries", "status")]
#unify countries
head(tesaurus)
unique(tesaurus$Buterflies)
polBt <- merge(polBt, tesaurus[,c(2,4)], by.x = "locality", by.y = "Buterflies")
head(polBt)
polBt <- polBt[, c("scientificName", "Countries", "occurrenceStatus")]
head(polBt)
head(distribution)
colnames(polBt) <- colnames(distribution)
distribution <- rbind(distribution, polBt)
head(distribution); tail(distribution)

distribution <- subset(distribution, !status %in% c("absent", "location_doubtful", 
                                                    "doubtful", "excluded", "extinct", "RE", "PE"))
distribution$status <- as.factor(distribution$status)
levels(distribution$status) <- c("non_native", "present", "present", "non_native",               
                                   "present", "present") 

# Save an object to a file
saveRDS(distribution, file = "Data/Species_thesaurus/distribution.rds")
saveRDS(taxonomy, file = "Data/Species_thesaurus/taxonomy.rds")



