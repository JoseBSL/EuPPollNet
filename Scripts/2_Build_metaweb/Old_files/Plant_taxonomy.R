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
