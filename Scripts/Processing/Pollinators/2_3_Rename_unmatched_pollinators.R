
#As this is quite long, 
#We keep this process in a separate script
#We have searched and renamed every unmatched poll
#In some cases we haven't found the spp
#And we rename to next higher rank

#Add additional info
#Polietes ladarius
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Scientific_name = case_when(
Fixed_name == "Polietes ladarius" ~ "Polietes lardarius (Fabricius, 1781)",
T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
Fixed_name == "Polietes ladarius" ~ "Polietes lardarius",
T ~ Canonical_name)) %>% 
mutate(Accepted_name = case_when(
Fixed_name == "Polietes ladarius" ~ "Polietes lardarius",
T ~ Accepted_name))   
#Ichneumoninae
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Ichneumoninae" ~ "Ichneumoninae",
   T ~ Accepted_name)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Ichneumoninae" ~ "SUBFAMILY",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Ichneumoninae" ~ "EXACT",
   T ~ Matchtype)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Ichneumoninae" ~ "Ichneumoninae",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Ichneumoninae" ~ "Ichneumoninae",
   T ~ Canonical_name)) %>% 
mutate(Order = case_when(
   Fixed_name == "Ichneumoninae" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Ichneumoninae" ~ "Ichneumonidae",
   T ~ Family))
#Leucozona
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Leucozona" ~ "Leucozona",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Leucozona" ~ "Leucozona (Schiner, 1860)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Leucozona" ~ "Leucozona",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Leucozona" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Leucozona" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Leucozona" ~ "Syrphidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Leucozona" ~ "Leucozona",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Leucozona" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Leucozona" ~ "EXACT",
   T ~ Matchtype))
#Symphyta
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Symphyta" ~ "Symphyta",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Symphyta" ~ "Symphyta (Turner, 1902)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Symphyta" ~ "Symphyta",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Symphyta" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Symphyta" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Symphyta" ~ "SUBORDER",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Symphyta" ~ "EXACT",
   T ~ Matchtype)) %>% 
mutate(Status = case_when(
   Fixed_name == "Symphyta" ~ "ACCEPTED",
   T ~ Status)) 
#Melitta
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Melitta" ~ "Melitta",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Melitta" ~ "Melitta (Kirby, 1802)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Melitta" ~ "Melitta",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Melitta" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Melitta" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Melitta" ~ "Melittidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Melitta" ~ "Melitta",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Melitta" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Melitta" ~ "EXACT",
   T ~ Matchtype))
#Colias
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Colias" ~ "Colias",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Colias" ~ "Colias (Fabricius, 1807)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Colias" ~ "Colias",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Colias" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Colias" ~ "Lepidoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Colias" ~ "Pieridae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Colias" ~ "Colias",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Colias" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Colias" ~ "EXACT",
   T ~ Matchtype))
#Leucostoma
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Leucostoma" ~ "Leucostoma",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Leucostoma" ~ "Leucostoma (Meigen, 1803)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Leucostoma" ~ "Leucostoma",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Leucostoma" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Leucostoma" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Leucostoma" ~ "Tachinidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Leucostoma" ~ "Leucostoma",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Leucostoma" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Leucostoma" ~ "EXACT",
   T ~ Matchtype))
#Melitaea
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Melitaea" ~ "Melitaea",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Melitaea" ~ "Melitaea (Fabricius, 1807)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Melitaea" ~ "Melitaea",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Melitaea" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Melitaea" ~ "Lepidoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Melitaea" ~ "Nymphalidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Melitaea" ~ "Melitaea",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Melitaea" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Melitaea" ~ "EXACT",
   T ~ Matchtype))
#Pyrgus
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Pyrgus" ~ "Pyrgus",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Pyrgus" ~ "Pyrgus (Hübner, 1819)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Pyrgus" ~ "Pyrgus",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Pyrgus" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Pyrgus" ~ "Lepidoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Pyrgus" ~ "Hesperiidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Pyrgus" ~ "Melitaea",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Pyrgus" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Pyrgus" ~ "EXACT",
   T ~ Matchtype))
#Zygaena
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Zygaena" ~ "Zygaena",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Zygaena" ~ "Zygaena (Fabricius, 1775)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Zygaena" ~ "Zygaena",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Zygaena" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Zygaena" ~ "Lepidoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Zygaena" ~ "Zygaenidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Zygaena" ~ "Zygaena",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Zygaena" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Zygaena" ~ "EXACT",
   T ~ Matchtype))
#Melanostoma
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Melanostoma" ~ "Melanostoma",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Melanostoma" ~ "Melanostoma (Schiner, 1860)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Melanostoma" ~ "Melanostoma",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Melanostoma" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Melanostoma" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Melanostoma" ~ "Syrphidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Melanostoma" ~ "Melanostoma",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Melanostoma" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Melanostoma" ~ "EXACT",
   T ~ Matchtype))

#Anthophora
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Anthophora" ~ "Anthophora",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Anthophora" ~ "Anthophora (Latreille, 1803)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Anthophora" ~ "Anthophora",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Anthophora" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Anthophora" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Anthophora" ~ "Apidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Anthophora" ~ "Anthophora",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Anthophora" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Anthophora" ~ "EXACT",
   T ~ Matchtype))

#Volucella
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Volucella" ~ "Volucella",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Volucella" ~ "Volucella (Geoffroy, 1762)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Volucella" ~ "Volucella",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Volucella" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Volucella" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Volucella" ~ "Syrphidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Volucella" ~ "Volucella",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Volucella" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Volucella" ~ "EXACT",
   T ~ Matchtype))
#Ceratina
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Ceratina" ~ "Ceratina",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Ceratina" ~ "Ceratina (Latreille, 1802)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Ceratina" ~ "Ceratina",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Ceratina" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Ceratina" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Ceratina" ~ "Apidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Ceratina" ~ "Ceratina",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Ceratina" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Ceratina" ~ "EXACT",
   T ~ Matchtype))
#Bicellaria
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Bicellaria" ~ "Bicellaria",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Bicellaria" ~ "Bicellaria (Macquart, 1823)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Bicellaria" ~ "Bicellaria",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Bicellaria" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Bicellaria" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Bicellaria" ~ "Hybotidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Bicellaria" ~ "Bicellaria",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Bicellaria" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Bicellaria" ~ "EXACT",
   T ~ Matchtype))
#Oxybelus
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Oxybelus" ~ "Oxybelus",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Oxybelus" ~ "Oxybelus (Latreille, 1797)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Oxybelus" ~ "Oxybelus",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Oxybelus" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Oxybelus" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Oxybelus" ~ "Crabronidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Oxybelus" ~ "Oxybelus",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Oxybelus" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Oxybelus" ~ "EXACT",
   T ~ Matchtype))

checks =
  unmatched_gbif1_not_found %>% 
filter(is.na(Accepted_name))

#Phania
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Phania" ~ "Phania",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Phania" ~ "Phania (Meigen, 1824)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Phania" ~ "Phania",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Phania" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Phania" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Phania" ~ "Tachinidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Phania" ~ "Phania",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Phania" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Phania" ~ "EXACT",
   T ~ Matchtype))

#Fannia
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Fannia" ~ "Fannia",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Fannia" ~ "Fannia (Robineau-Desvoidy, 1830)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Fannia" ~ "Fannia",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Fannia" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Fannia" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Fannia" ~ "Fanniidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Fannia" ~ "Fannia",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Fannia" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Fannia" ~ "EXACT",
   T ~ Matchtype))
#Heringia
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Heringia" ~ "Heringia",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Heringia" ~ "Heringia (Rondani, 1856)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Heringia" ~ "Heringia",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Heringia" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Heringia" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Heringia" ~ "Syrphidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Heringia" ~ "Heringia",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Heringia" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Heringia" ~ "EXACT",
   T ~ Matchtype))

#Polyommatus
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Polyommatus" ~ "Polyommatus",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Polyommatus" ~ "Polyommatus (Latreille, 1804)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Polyommatus" ~ "Polyommatus",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Polyommatus" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Polyommatus" ~ "Lepidoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Polyommatus" ~ "Lycaenidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Polyommatus" ~ "Polyommatus",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Polyommatus" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Polyommatus" ~ "EXACT",
   T ~ Matchtype))

#Limonia
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Limonia" ~ "Limonia",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Limonia" ~ "Limonia (Meigen, 1803)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Limonia" ~ "Limonia",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Limonia" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Limonia" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Limonia" ~ "Limoniidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Limonia" ~ "Limonia",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Limonia" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Limonia" ~ "EXACT",
   T ~ Matchtype))

#Ancistrocerus
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Ancistrocerus" ~ "Ancistrocerus",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Ancistrocerus" ~ "Ancistrocerus (Wesmael, 1836)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Ancistrocerus" ~ "Ancistrocerus",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Ancistrocerus" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Ancistrocerus" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Ancistrocerus" ~ "Vespidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Ancistrocerus" ~ "Ancistrocerus",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Ancistrocerus" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Ancistrocerus" ~ "EXACT",
   T ~ Matchtype))

#Eumerus
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Eumerus" ~ "Eumerus",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Eumerus" ~ "Eumerus (Meigen, 1822)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Eumerus" ~ "Eumerus",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Eumerus" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Eumerus" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Eumerus" ~ "Syrphidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Eumerus" ~ "Eumerus",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Eumerus" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Eumerus" ~ "EXACT",
   T ~ Matchtype))

#Trichodes
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Trichodes" ~ "Trichodes",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Trichodes" ~ "Trichodes (Herbst, 1792)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Trichodes" ~ "Trichodes",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Trichodes" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Trichodes" ~ "Coleoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Trichodes" ~ "Cleridae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Trichodes" ~ "Trichodes",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Trichodes" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Trichodes" ~ "EXACT",
   T ~ Matchtype))

#Trichodes
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Euscelis" ~ "Euscelis",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Euscelis" ~ "Euscelis (Brullé, 1832)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Euscelis" ~ "Euscelis",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Euscelis" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Euscelis" ~ "Hemiptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Euscelis" ~ "Cicadellidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Euscelis" ~ "Euscelis",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Euscelis" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Euscelis" ~ "EXACT",
   T ~ Matchtype))
#Dolichopus
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Dolichopus" ~ "Dolichopus",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Dolichopus" ~ "Dolichopus (Latreille, 1796)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Dolichopus" ~ "Dolichopus",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Dolichopus" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Dolichopus" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Dolichopus" ~ "Dolichopodidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Dolichopus" ~ "Dolichopus",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Dolichopus" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Dolichopus" ~ "EXACT",
   T ~ Matchtype))
#Plecoptera
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Plecoptera" ~ "Plecoptera",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Plecoptera" ~ "Plecoptera (Burmeister, 1839)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Plecoptera" ~ "Plecoptera",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Plecoptera" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Plecoptera" ~ "Plecoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Plecoptera" ~ NA_character_,
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Plecoptera" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Plecoptera" ~ "ORDER",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Plecoptera" ~ "EXACT",
   T ~ Matchtype))

#Pristiphora
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Pristiphora" ~ "Pristiphora",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Pristiphora" ~ "Pristiphora (Latreille, 1810)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Pristiphora" ~ "Pristiphora",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Pristiphora" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Pristiphora" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Pristiphora" ~ "Tenthredinidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Pristiphora" ~ "Pristiphora",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Pristiphora" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Pristiphora" ~ "EXACT",
   T ~ Matchtype))

#Microgaster
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Microgaster" ~ "Microgaster",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Microgaster" ~ "Microgaster (Latreille, 1804)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Microgaster" ~ "Microgaster",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Microgaster" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Microgaster" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Microgaster" ~ "Braconidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Microgaster" ~ "Microgaster",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Microgaster" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Microgaster" ~ "EXACT",
   T ~ Matchtype))

#Ichneumon
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Ichneumon" ~ "Ichneumon",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Ichneumon" ~ "Ichneumon (Linnaeus, 1758)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Ichneumon" ~ "Ichneumon",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Ichneumon" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Ichneumon" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Ichneumon" ~ "Ichneumonidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Ichneumon" ~ "Ichneumon",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Ichneumon" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Ichneumon" ~ "EXACT",
   T ~ Matchtype))


checks =
  unmatched_gbif1_not_found %>% 
filter(is.na(Accepted_name))

#Phoridae
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Phoridae" ~ "Phoridae",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Phoridae" ~ "Phoridae (Latreille, 1796)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Phoridae" ~ "Phoridae",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Phoridae" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Phoridae" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Phoridae" ~ "Phoridae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Phoridae" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Phoridae" ~ "FAMILY",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Phoridae" ~ "EXACT",
   T ~ Matchtype))

#Apidae
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Apidae" ~ "Apidae",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Apidae" ~ "Apidae (Latreille, 1802)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Apidae" ~ "Apidae",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Apidae" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Apidae" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Apidae" ~ "Apidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Apidae" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Apidae" ~ "FAMILY",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Apidae" ~ "EXACT",
   T ~ Matchtype))

#Cecidomyiidae
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Cecidomyiidae" ~ "Cecidomyiidae",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Cecidomyiidae" ~ "Cecidomyiidae",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Cecidomyiidae" ~ "Cecidomyiidae",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Cecidomyiidae" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Cecidomyiidae" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Cecidomyiidae" ~ "Cecidomyiidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Cecidomyiidae" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Cecidomyiidae" ~ "FAMILY",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Cecidomyiidae" ~ "EXACT",
   T ~ Matchtype))

#Sciaridae
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Sciaridae" ~ "Sciaridae",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Sciaridae" ~ "Sciaridae",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Sciaridae" ~ "Sciaridae",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Sciaridae" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Sciaridae" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Sciaridae" ~ "Sciaridae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Sciaridae" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Sciaridae" ~ "FAMILY",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Sciaridae" ~ "EXACT",
   T ~ Matchtype))

#Apoidea
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Apoidea" ~ "Apoidea",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Apoidea" ~ "Apoidea (Latreille, 1802)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Apoidea" ~ "Apoidea",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Apoidea" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Apoidea" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Apoidea" ~ NA_character_,
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Apoidea" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Apoidea" ~ "SUPERFAMILY",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Apoidea" ~ "EXACT",
   T ~ Matchtype))

#Acari
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Acari" ~ "Arachnida",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Acari" ~ "Arachnida",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Acari" ~ "Arachnida",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Acari" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Acari" ~ NA_character_,
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Acari" ~ NA_character_,
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Acari" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Acari" ~ "CLASS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Acari" ~ "HIGHERRANK",
   T ~ Matchtype))

#Brachycera
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Brachycera" ~ "Brachycera",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Brachycera" ~ "Brachycera (Schiner, 1862)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Brachycera" ~ "Brachycera",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Brachycera" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Brachycera" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Brachycera" ~ NA_character_,
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Brachycera" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Brachycera" ~ "SUBORDER",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Brachycera" ~ "HIGHERRANK",
   T ~ Matchtype))

#Empis nigripes
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Empis nigripes" ~ "Empis nigripes",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Empis nigripes" ~ "Empis nigripes (Fabricius, 1794)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Empis nigripes" ~ "Empis nigripes",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Empis nigripes" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Empis nigripes" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Empis nigripes" ~ "Empididae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Empis nigripes" ~ "Empis",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Empis nigripes" ~ "SPECIES",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Empis nigripes" ~ "EXACT",
   T ~ Matchtype))

#Empis admontensis
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Empis admontensis" ~ "Empis",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Empis admontensis" ~ "Empis (Linnaeus, 1758)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Empis admontensis" ~ "Empis",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Empis admontensis" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Empis admontensis" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Empis admontensis" ~ "Empididae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Empis admontensis" ~ "Empis",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Empis admontensis" ~ "SPECIES",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Empis admontensis" ~ "HIGHERRANK",
   T ~ Matchtype))


#Halictus allratus
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Halictus allratus" ~ "Halictus",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Halictus allratus" ~ "Halictus (Latreille, 1804)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Halictus allratus" ~ "Halictus",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Halictus allratus" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Halictus allratus" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Halictus allratus" ~ "Halictidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Halictus allratus" ~ "Halictus",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Halictus allratus" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Halictus allratus" ~ "HIGHERRANK",
   T ~ Matchtype))

#Lasioglossum lapponica
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Lasioglossum lapponica" ~ "Lasioglossum",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Lasioglossum lapponica" ~ "Lasioglossum (Curtis, 1833)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Lasioglossum lapponica" ~ "Lasioglossum",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Lasioglossum lapponica" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Lasioglossum lapponica" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Lasioglossum lapponica" ~ "Halictidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Lasioglossum lapponica" ~ "Lasioglossum",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Lasioglossum lapponica" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Lasioglossum lapponica" ~ "HIGHERRANK",
   T ~ Matchtype))

#Tenthredo corynetes
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Tenthredo corynetes" ~ "Tenthredo",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Tenthredo corynetes" ~ "Tenthredo (Linnaeus, 1758)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Tenthredo corynetes" ~ "Tenthredo",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Tenthredo corynetes" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Tenthredo corynetes" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Tenthredo corynetes" ~ "Tenthredinidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Tenthredo corynetes" ~ "Tenthredo",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Tenthredo corynetes" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Tenthredo corynetes" ~ "HIGHERRANK",
   T ~ Matchtype))

#Unknown
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Unknown" ~ "Unknown",
   T ~ Accepted_name))

#Gymnodia
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Gymnodia" ~ "Gymnodia",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Gymnodia" ~ "Gymnodia (Robineau-Desvoidy, 1830)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Gymnodia" ~ "Gymnodia",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Gymnodia" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Gymnodia" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Gymnodia" ~ "Muscidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Gymnodia" ~ "Gymnodia",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Gymnodia" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Gymnodia" ~ "EXACT",
   T ~ Matchtype))

#Thecophora
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Thecophora" ~ "Thecophora",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Thecophora" ~ "Thecophora (Rondani, 1845)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Thecophora" ~ "Thecophora",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Thecophora" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Thecophora" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Thecophora" ~ "Conopidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Thecophora" ~ "Thecophora",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Thecophora" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Thecophora" ~ "EXACT",
   T ~ Matchtype))

#Rhinophora
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Rhinophora" ~ "Rhinophora",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Rhinophora" ~ "Rhinophora (Robineau-Desvoidy, 1830)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Rhinophora" ~ "Rhinophora",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Rhinophora" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Rhinophora" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Rhinophora" ~ "Calliphoridae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Rhinophora" ~ "Rhinophora",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Rhinophora" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Rhinophora" ~ "EXACT",
   T ~ Matchtype))

#Thecophora
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Thecophora" ~ "Thecophora",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Thecophora" ~ "Thecophora (Rondani, 1845)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Thecophora" ~ "Thecophora",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Thecophora" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Thecophora" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Thecophora" ~ "Conopidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Thecophora" ~ "Thecophora",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Thecophora" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Thecophora" ~ "EXACT",
   T ~ Matchtype))

#Potamia
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Potamia" ~ "Potamia",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Potamia" ~ "Potamia (Robineau-Desvoidy, 1830)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Potamia" ~ "Potamia",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Potamia" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Potamia" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Potamia" ~ "Muscidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Potamia" ~ "Potamia",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Potamia" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Potamia" ~ "EXACT",
   T ~ Matchtype))

#Ctenopelmatinae
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Ctenopelmatinae" ~ "Ctenopelmatinae",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Ctenopelmatinae" ~ "Ctenopelmatinae (Förster, 1869)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Ctenopelmatinae" ~ "Ctenopelmatinae",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Ctenopelmatinae" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Ctenopelmatinae" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Ctenopelmatinae" ~ "Ichneumonidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Ctenopelmatinae" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Ctenopelmatinae" ~ "SUBFAMILY",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Ctenopelmatinae" ~ "EXACT",
   T ~ Matchtype))

#Sargus
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Sargus" ~ "Sargus",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Sargus" ~ "Sargus (Fabricius, 1798)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Sargus" ~ "Sargus",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Sargus" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Sargus" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Sargus" ~ "Stratiomyidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Sargus" ~ "Sargus",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Sargus" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Sargus" ~ "EXACT",
   T ~ Matchtype))

#Bellardia
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Bellardia" ~ "Bellardia",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Bellardia" ~ "Sargus (Robineau-Desvoidy, 1863)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Bellardia" ~ "Bellardia",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Bellardia" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Bellardia" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Bellardia" ~ "Calliphoridae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Bellardia" ~ "Bellardia",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Bellardia" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Bellardia" ~ "EXACT",
   T ~ Matchtype))

#Macroglossum
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Macroglossum" ~ "Macroglossum",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Macroglossum" ~ "Macroglossum (Scopoli, 1777)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Macroglossum" ~ "Macroglossum",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Macroglossum" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Macroglossum" ~ "Lepidoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Macroglossum" ~ "Sphingidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Macroglossum" ~ "Macroglossum",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Macroglossum" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Macroglossum" ~ "EXACT",
   T ~ Matchtype))

#Anaspis
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Anaspis" ~ "Anaspis",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Anaspis" ~ "Anaspis (Geoffroy, 1762)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Anaspis" ~ "Anaspis",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Anaspis" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Anaspis" ~ "Coleoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Anaspis" ~ "Scraptiidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Anaspis" ~ "Anaspis",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Anaspis" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Anaspis" ~ "EXACT",
   T ~ Matchtype))

#Zygoptera
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Zygoptera" ~ "Zygoptera",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Zygoptera" ~ "Zygoptera (Selys, 1854)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Zygoptera" ~ "Zygoptera",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Zygoptera" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Zygoptera" ~ "Odonata",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Zygoptera" ~ NA_character_,
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Zygoptera" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Zygoptera" ~ NA_character_,
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Zygoptera" ~ "EXACT",
   T ~ Matchtype))

#Zygoptera
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Order = case_when(
   Fixed_name == "Podarcis lilfordi" ~ "Squamata",
   T ~ Order)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Podarcis lilfordi" ~ "EXACT",
   T ~ Matchtype)) 

#Zygoptera
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Order = case_when(
   Fixed_name == "Cheloninae" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Cheloninae" ~ "EXACT",
   T ~ Matchtype)) 

#Eumerus clavatus uncipes
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Rank = case_when(
   Fixed_name == "Eumerus clavatus uncipes" ~ "SPECIES",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Eumerus clavatus uncipes" ~ "EXACT",
   T ~ Matchtype)) 

#Cheilosia albitarsis ranunculi
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Cheilosia albitarsis ranunculi" ~ "EXACT",
   T ~ Matchtype)) 

#Platycheirus albimanus muelleri
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Platycheirus albimanus muelleri" ~ "EXACT",
   T ~ Matchtype)) 

#Meria
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Meria" ~ "Meria",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Meria" ~ "Meria (Illiger, 1807)",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Meria" ~ "Meria",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Meria" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Meria" ~ "Hymenoptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Meria" ~ "Tiphiidae",
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Meria" ~ "Meria",
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Meria" ~ "GENUS",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Meria" ~ "EXACT",
   T ~ Matchtype))

#Diptera
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Diptera" ~ "Diptera",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Diptera" ~ "Diptera",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Diptera" ~ "Diptera",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Diptera" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Diptera" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Diptera" ~ NA_character_,
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Diptera" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Diptera" ~ "ORDER",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Diptera" ~ "EXACT",
   T ~ Matchtype))


#Diptera
unmatched_gbif1_not_found = 
unmatched_gbif1_not_found %>% 
mutate(Accepted_name = case_when(
   Fixed_name == "Diptera" ~ "Diptera",
   T ~ Accepted_name)) %>% 
mutate(Scientific_name = case_when(
   Fixed_name == "Diptera" ~ "Diptera",
   T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
   Fixed_name == "Diptera" ~ "Diptera",
   T ~ Canonical_name)) %>% 
mutate(Phylum = case_when(
   Fixed_name == "Diptera" ~ "Arthropoda",
   T ~ Phylum)) %>% 
mutate(Order = case_when(
   Fixed_name == "Diptera" ~ "Diptera",
   T ~ Order)) %>% 
mutate(Family = case_when(
   Fixed_name == "Diptera" ~ NA_character_,
   T ~ Family)) %>% 
mutate(Genus = case_when(
   Fixed_name == "Diptera" ~ NA_character_,
   T ~ Genus)) %>% 
mutate(Rank = case_when(
   Fixed_name == "Diptera" ~ "ORDER",
   T ~ Rank)) %>% 
mutate(Matchtype = case_when(
   Fixed_name == "Diptera" ~ "EXACT",
   T ~ Matchtype))




