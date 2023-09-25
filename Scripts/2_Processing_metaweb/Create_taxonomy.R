#create master taxonomic and distribution files:

#Bees and syrphids----
polBS <- read.csv(file = "Data/temp_data/Master_bees_syrphids.csv")
head(polBS)
#extract taxonomy
taxonomy <- polBS[,1:9]
#extract distribution
colnames(polBS) #countries to match.
#write.csv(colnames(polBS), "scripts/temp_data/country_tesaurus_init.csv")
#This file form the bases to create manually country_tesaurus.csv
#Make long format
library(reshape2)
distribution1 <- melt(data = polBS, id.vars = "GenusAndSpecies", measure.vars = 10:ncol(polBS), 
      value.name = "status", variable.name = "country")
head(distribution1)
#We remove absences
distribution2 <- subset(distribution1, status != "A")
head(distribution2)
#we unify country names according to tesaurus
tesaurus <- read.csv("Data/temp_data/country_tesaurus.csv")
head(tesaurus)
distribution <- merge(distribution2, tesaurus[,2:3], by.x = "country", by.y = "Bees_syrphids")
head(distribution)
distribution <- distribution[,-1]
head(distribution)

#Buterflies----
#taxonomy
taxBt <- read.delim(file = "Data/temp_data/Butterfly_taxon.txt",
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
polBt <- read.delim(file = "Data/temp_data/Butterfly_distribution.txt",
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

#Plants----
plants <- read.table("Data/temp_data/wcvp_distribution.csv", sep="|", 
                     header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8") #laaarge file
head(plants)
unique(plants$continent)
unique(plants$region)
unique(plants$area)
plants2 <- subset(x = plants, subset = continent == "EUROPE")
unique(plants2$region)
unique(plants2$area)
#write.csv(unique(plants2[,c(7,8)]), "scripts/temp_data/country_temp_plants.csv")
#As plants at a bit more coarse resultion e.g. Baltic states, many entries map
#into this items... this should be tracked.
head(plants2)

#Taxonomy
taxPl <- read.table("scripts/temp_data/wcvp_names.csv", sep="|", 
                    header=TRUE, quote = "", fill=TRUE, 
                    encoding = "UTF-8") #laaarge file

head(taxPl)
unique(taxPl$geographic_area) #descriptive... difficult to use.
#Se pueden unir por plant_name_id
# filter plants in EU
taxPl2 <- taxPl[which(taxPl$plant_name_id %in% plants2$plant_name_id),]
head(taxPl2)
#accepted_plant_name_id might be relevant to resolve synonims.
#filter species
unique(taxPl2$taxon_rank)
taxPl2 <- subset(taxPl2, taxon_rank == "Species")
taxPl3 <- taxPl2[,c("family", "genus", "species", "taxon_name", "taxon_authors", "first_published")]
head(taxonomy)
taxPl3$TaxonomicAuthority <- paste(taxPl3$taxon_authors, taxPl3$first_published)
#merge with taxonomy
head(taxonomy)
taxPl3$order <- NA
taxPl3$Subfamily <- NA
taxPl3$Tribe <- NA
taxPl3$Subgenus <- NA
taxPl3 <-  taxPl3[, c("order", "family", "Subfamily", "Tribe", "genus",
                              "Subgenus", "species", "taxon_name", 
                              "TaxonomicAuthority")] 
colnames(taxPl3) <- colnames(taxonomy)
taxonomy <- rbind(taxonomy, taxPl3)
head(taxonomy); tail(taxonomy)

#distribution
head(taxPl2); head(plants2)
dist_plants <- merge(x = taxPl2[, c("taxon_name", "plant_name_id")], y = plants2, by = "plant_name_id")
head(dist_plants)
dist_plants <- dist_plants[,c("taxon_name", "area", "introduced", "extinct", "location_doubtful")]
#unify area
head(tesaurus)
unique(dist_plants$area)
dist_plants <- dist_plants[-which(dist_plants$area == ""),] #I am removing those.
dist_plants$status <- ifelse(dist_plants$introduced == 1, "introduced", "present")
dist_plants$status <- ifelse(dist_plants$extinct == 1, "extinct", dist_plants$status)
dist_plants$status <- ifelse(dist_plants$location_doubtful == 1, "location_doubtful", dist_plants$status)
dist_plants2 <- merge(dist_plants[,c(1,2,6)], tesaurus[,c(2,5,7)], by.x = "area", by.y = "Plants")
dim(dist_plants); dim(dist_plants2)
## How this would treat one to many matches? I want to create duplicates.
head(dist_plants2)
#quick check that merge works as expected
#a <- data.frame(country = c("croatia", "bosnia", "france"),
 #               area = c("yougoslavia", "yougoslavia", "France"))
#b <- data.frame(plant = c("plant1", "Plant2", "Plant3"),
 #               area = c("yougoslavia", "yougoslavia", "France"))
#merge(a, b)
#merge
head(distribution)
head(dist_plants2)
dist_plants2 <- dist_plants2[,c("taxon_name", "Countries", "status")]
colnames(dist_plants2) <- colnames(distribution)
distribution <- rbind(distribution, dist_plants2)
unique(distribution$status)
distribution <- subset(distribution, !status %in% c("absent", "location_doubtful", 
                                                    "doubtful", "excluded", "extinct", "RE", "PE"))
distribution$status <- as.factor(distribution$status)
levels(distribution$status) <- c("non_native", "present", "present", "non_native",               
                                   "present", "present") 

#write data----
write.csv(distribution, "scripts/cleandata/distribution.csv")
write.csv(taxonomy, "scripts/cleandata/taxonomy.csv")

# Save an object to a file
saveRDS(distribution, file = "scripts/cleandata/distribution.rds")
saveRDS(taxonomy, file = "scripts/cleandata/taxonomy.rds")
#load it via readRDS()
