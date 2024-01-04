

#Lasioglossum discum fertoni
unmatched_gbif1 = 
unmatched_gbif1 %>% 
mutate(Rank = case_when(
   Fixed_name == "Lasioglossum discum fertoni" ~ "SPECIES",
   T ~ Rank)) %>% 
mutate(Status = case_when(
   Fixed_name == "Lasioglossum discum fertoni" ~ "ACCEPTED",
   T ~ Status)) 

#Bombus sylvarum nigrescens
unmatched_gbif1 = 
unmatched_gbif1 %>% 
mutate(Rank = case_when(
   Fixed_name == "Bombus sylvarum nigrescens" ~ "SPECIES",
   T ~ Rank))



