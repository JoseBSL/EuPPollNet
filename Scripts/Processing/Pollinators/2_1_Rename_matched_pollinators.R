
#As this is quite long, 
#We keep this process in a separate script
#We have searched and renamed every unmatched poll
#In some cases we haven't found the spp
#And we rename to next higher rank

#Vaccinium vitis-idaea is a plant species (mistake)
matched_gbif1 = matched_gbif1 %>% 
mutate(Accepted_name = case_when(
  Fixed_name == "Coenonympha gardetta" ~ "Coenonympha gardetta",
  T ~ Accepted_name)) %>% 
 mutate(Rank = case_when(
  Fixed_name == "Coenonympha gardetta" ~ "SPECIES",
  T ~ Rank)) %>% 
 mutate(Matchtype = case_when(
  Fixed_name == "Coenonympha gardetta" ~ "EXACT",
  T ~ Matchtype)) %>% 
 mutate(Scientific_name = case_when(
  Fixed_name == "Coenonympha gardetta" ~ "Coenonympha gardetta (Prunner, 1798)",
  T ~ Scientific_name)) %>% 
mutate(Accepted_name = case_when(
  Fixed_name == "Vaccinium vitis-idaea" ~ "Unknown",
  T ~ Accepted_name
))
#Fix other 5 matchtypes with higher rank
#Look manually for the accepted name
#1)"Seladonia semitecta"
#We are going to consider Seladonia as a subgenus
matched_gbif1 = matched_gbif1 %>% 
mutate(Accepted_name = case_when(
    Fixed_name == "Seladonia semitecta" ~ "Halictus semitectus",
    T ~ Accepted_name)) %>% 
mutate(Status = case_when(
    Fixed_name == "Seladonia semitecta" ~ "SYNONYM",
    T ~ Status)) %>% 
mutate(Scientific_name = case_when(
    Fixed_name == "Seladonia semitecta" ~ "Seladonia semitecta (Morawitz, 1874)",
    T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
    Fixed_name == "Seladonia semitecta" ~ "Seladonia semitecta",
    T ~ Canonical_name)) %>% 
mutate(Genus = case_when(
    Fixed_name == "Seladonia semitecta" ~ "Halictus",
    T ~ Genus)) %>% 
mutate(Rank = case_when(
    Fixed_name == "Seladonia semitecta" ~ "SPECIES",
    T ~ Rank)) %>% 
mutate(Matchtype = case_when(
    Fixed_name == "Seladonia semitecta" ~ "EXACT",
    T ~ Matchtype)) 
#Fix other Seladonia cases
matched_gbif1 = matched_gbif1 %>% 
mutate(Genus = case_when(
    Fixed_name == "Seladonia submediterranea" ~ "Halictus",
    T ~ Genus)) %>% 
mutate(Accepted_name = case_when(
    Fixed_name == "Seladonia submediterranea" ~ "Halictus submediterranea",
    T ~ Accepted_name)) 
#Limenitis camilla
matched_gbif1 = matched_gbif1 %>% 
mutate(Status = case_when(
  Fixed_name == "Limenitis camilla" ~ "SYNONYM",
  T ~ Status)) %>% 
mutate(Scientific_name = case_when(
  Fixed_name == "Limenitis camilla" ~ "Limenitis camilla (Denis & Schiffermüller, 1775)",
  T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
  Fixed_name == "Limenitis camilla" ~ "Limenitis camilla",
  T ~ Canonical_name)) %>% 
mutate(Accepted_name = case_when(
  Fixed_name == "Limenitis camilla" ~ "Limenitis reducta",
  T ~ Accepted_name)) %>% 
mutate(Genus = case_when(
  Fixed_name == "Limenitis camilla" ~ "Limenitis",
  T ~ Genus)) %>% 
mutate(Rank = case_when(
  Fixed_name == "Limenitis camilla" ~ "SPECIES",
  T ~ Rank)) %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Limenitis camilla" ~ "EXACT",
  T ~ Matchtype))

#Polyommatus dorylas, update to Plebicula dorylas
matched_gbif1 = matched_gbif1 %>% 
mutate(Status = case_when(
  Fixed_name == "Polyommatus dorylas" ~ "SYNONYM",
  T ~ Status)) %>% 
mutate(Scientific_name = case_when(
  Fixed_name == "Polyommatus dorylas" ~ "Polyommatus dorylas (Denis & Schiffermüller, 1775)",
  T ~ Scientific_name)) %>% 
mutate(Canonical_name = case_when(
  Fixed_name == "Polyommatus dorylas" ~ "Polyommatus dorylas",
  T ~ Canonical_name)) %>% 
mutate(Accepted_name = case_when(
  Fixed_name == "Polyommatus dorylas" ~ "Plebicula dorylas",
  T ~ Accepted_name)) %>% 
mutate(Genus = case_when(
  Fixed_name == "Polyommatus dorylas" ~ "Plebicula",
  T ~ Genus)) %>% 
mutate(Matchtype = case_when(
  Fixed_name == "Polyommatus dorylas" ~ "EXACT",
  T ~ Matchtype)) %>% 
mutate(Rank = case_when(
  Fixed_name == "Polyommatus dorylas" ~ "SPECIES",
  T ~ Rank))