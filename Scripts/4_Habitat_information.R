#Extract habitat type by coordinates and create a "homogeneous"
#sampling habitat classification across studies

library(dplyr)
#Load data 
data = readRDS("Data/3_Final_data/Interactions_uncounted.rds")

#Make sure that each site has only a single coordinate
#I have fixed some coords on the processing scripts
coord_checks = data %>% 
select(Study_id, Network_id, Latitude, Longitude) %>% 
group_by(Study_id, Network_id) %>% 
summarise(Latitude_counts = n_distinct(Latitude), 
Longitude_counts = n_distinct(Longitude)) 
#Print maximum value (cannot be greater than 1)
coord_checks %>% 
ungroup() %>% 
summarise(max_lat = max(Latitude_counts),
          max_lon = max(Longitude_counts))


#Read raster data
library(terra)
r <- rast("Data/Working_files/Raster_Europe/DATA/U2018_CLC2018_V2020_20u1.tif")
head(cats(r)[[1]])

#Generate a tibble with coordinates
coords = data %>% 
select(Longitude, Latitude) %>% 
distinct()

#Create vector with coordinates
lon <- coords %>% pull(Longitude)
lat <- coords %>% pull(Latitude)
xy <- cbind(lon, lat) 
v <- vect(xy, crs="+proj=longlat")
vv <- project(v, crs(r)) 

#Extract
activeCat(r) <- "LABEL3" #this has the categories of interest
#Extract land use
land_cover = extract(r, vv)

#Check levels
land_cover %>% 
group_by(LABEL3) %>% 
summarize(n_rows = length(LABEL3))

#Check levels
factor(levels(land_cover$LABEL3))

#Merge datasets
all = cbind(coords, land_cover)
colnames(all)
head(all)

#Restore dataset (to raw conditions to run this)
data = data %>% 
select(!c(SafeNet_habitat)) %>% 
rename("Habitat" = "Authors_habitat")

#Merge back to dataset with everything
data1 = left_join(data, all, by=c("Longitude", "Latitude")) %>% 
rename("Corine_land_cover" = "LABEL3")

#Explore data (data1)
habitat = data1 %>% 
select(Study_id, Network_id, Habitat, Corine_land_cover, Latitude, Longitude) %>% 
distinct() %>% 
rename(Land_cover = Corine_land_cover)

#Check by study
#We are going to standardise according to authors matching
#Edits are conducted on Land_cover column

#Create definitions here based on Corine 
#and my personal view to make things easier:

#1)Pastures: Any type of low growing plant 
#community that is highly influenced by human disturbance.
#For instance, agriculture, mowing, moderate to 
#high grazing or urban environment. Note that this category also includes old pastures
#with regrowth of woody vegetation.

#2)Agricultural margings: Sides of crops that can include 
#any type of vegetation from low growing plants to trees.

#3)Agricultural land: Includes any type of crop and any 
#type of vegetation growing within them. 

#4)Semi-natural grassland: Low growing plant community with
#relatively low disturbances but under low pressure such as seasonal
#mowing or extensive grazing.

#5)Ruderal vegetation: Plants growing on highly disturbed sites such as
#road sides or mineral extraction sites.

#6)Sclerophyllous vegetation: Any type of system with a dominant shrub community
#adapted to drought. Typical of the Mediterranean region. Note, that we 
#have include in this category also woodlands (open coniferous forest) where
#the shrub community was the main focus of the study.

#7) Green urban areas: Parks, private gardens or small pastures within an 
#urban setting. For simplicity, we have also included botanical gardens.

#8) Forest/woodland understory: Any plant community sampled under a wooded group of plants 
#The forest could be embedded in an agricultural setting or in a fully
#natural scenario. We have included here agro-forestry areas, open forest to dense forest but try to
#exclude forest that contains typical sclerophyllous vegetation. 

#9) Riparian vegetation: Plant communities growing on river margins. 

#10): Low growing plant communities with little or none human disturbance. Often located
#in high elevation areas within Europe.

#11) Beaches, dunes, sands: Plant communities growing on sandy soil.

#12) Moors and heathland: Low growing woody vegetation 
#characteristic from low fertile soils near the coast or in alpine areas.



#1_Bartomeus looks good!
#2_Petanidou 
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "2_Petanidou" & Habitat == "scrubland" ~ "Sclerophyllous vegetation",
  Study_id == "2_Petanidou" & Habitat == "agricultural margins" ~ "Agricultural margins",
    TRUE ~ Land_cover))


#3_Michez 
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "3_Michez" & Habitat == "Prairie" ~ "Pastures (agricultural/urban matrix)",
            Study_id == "3_Michez" & Habitat == "Rudéral" ~ "Road side",
            Study_id == "3_Michez" & Habitat == "Carrière" ~ "Mineral extraction sites",    
            Study_id == "3_Michez" & Habitat == "Parc" ~ "Green urban areas",    
            Study_id == "3_Michez" & Habitat == "Terril" ~ "Mineral extraction sites",    
            TRUE ~ Land_cover))

#4_Marini could be further checked but nothing super weird
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "4_Marini" & Habitat == "Riparian" ~ "Riparian vegetation",
            TRUE ~ Land_cover))

#5_Marini
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "5_Marini" & Network_id == "UNIPD02 A" ~ "Pastures (agricultural/urban/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AA" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AA" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AB" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AB" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AC" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AD" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AE" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AF" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AF" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AG" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AI" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AJ" ~ "Road side",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AK" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AL" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AM" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AN" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AO" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AP" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AQ" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AR" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AS" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AT" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AU" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AV" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AW" ~ "Pastures (agricultural matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AX" ~ "Riparian",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AZ" ~ "Road side",
    Study_id == "5_Marini" & Network_id == "UNIPD02 B" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 C" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 D" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 E" ~ "Riparian",
    Study_id == "5_Marini" & Network_id == "UNIPD02 F" ~ "Pastures (agricultural/urban matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 G" ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 H" ~ "Pastures (agricultural/urban/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 I" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 J" ~ "Riparian",
    Study_id == "5_Marini" & Network_id == "UNIPD02 K" ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 L" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 M" ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 N" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 O" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 P" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 Q" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 R" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 S" ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 T" ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 U" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 V" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 W" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 X" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 Y" ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 Z" ~ "Riparian",

        TRUE ~ Land_cover))

#6_Marini
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "6_Marini" & Habitat == "Crop/Semi-nat" ~ "Cropland",
    TRUE ~ Land_cover))
#7_Scheper 
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "7_Scheper"  & Habitat == "Grassland" ~ "Pastures (agricultural matrix)",
            Study_id == "7_Scheper"  & Habitat == "Field boundary" ~ "Agricultural margins",
    TRUE ~ Land_cover))
#8_Biella
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "8_Biella" ~ "Alpine grasslands",
    TRUE ~ Land_cover))
#9_Heleno
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "9_Heleno" ~ "Botanical garden",
    TRUE ~ Land_cover))
#10_Vanbergen 
vanbergen_habitats = data %>% 
filter(Study_id == "10_Vanbergen") %>% 
distinct(Habitat) %>% pull()
#vanbergen_habitats

habitat = habitat %>% 
 mutate(Land_cover = 
  case_when(Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[1] ~ "Cropland",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[2] ~ "Pastures (agricultural matrix)",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[3] ~ "Transitional woodland-shrub",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[4] ~ "Riparian vegetation",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[5] ~ "Sclerophyllous vegetation",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[6] ~ "Cropland",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[7] ~ "Pastures (agricultural/vegetation matrix)", 
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[8] ~ "Agricultural margins",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[9] ~ "Riparian vegetation",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[10] ~ "Agricultural margins",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[11] ~ "Pastures (agricultural/vegetation matrix)", 
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[12] ~ "Agro-forestry areas",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[13] ~ "Pastures (agricultural/vegetation matrix)",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[14] ~ "Cropland",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[15] ~ "Green urban areas",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[16] ~ "Green urban areas",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[17] ~ "Cropland",
            Study_id == "10_Vanbergen" & Habitat == vanbergen_habitats[18] ~ "Green urban areas",
            TRUE ~ Land_cover))

#11_Clough
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
   Study_id == "11_Clough" & Network_id == "ASK_2020" ~ "Semi-natural grassland",
   Study_id == "11_Clough" & Network_id == "ASK_2021" ~ "Semi-natural grassland",
   Study_id == "11_Clough" & Network_id == "DAL_2020" ~ "Semi-natural grassland",
   Study_id == "11_Clough" & Network_id == "DAL_2021" ~ "Semi-natural grassland",
   Study_id == "11_Clough" & Network_id == "GLA_2020" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "GLA_2021" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "GYL_2020" ~ "Semi-natural grassland",
   Study_id == "11_Clough" & Network_id == "GYL_2021" ~ "Semi-natural grassland",
   Study_id == "11_Clough" & Network_id == "HAM_2020" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "HAM_2021" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "HAR_2020" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "HAR_2021" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "HAS_2020" ~ "Pastures (vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "HAS_2021" ~ "Pastures (vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "KAG_2020" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "KAG_2021" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "KAR_2020" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "KAR_2021" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "KOM_2020" ~ "Semi-natural grassland",
   Study_id == "11_Clough" & Network_id == "KOM_2021" ~ "Semi-natural grassland",
   Study_id == "11_Clough" & Network_id == "LOB_2020" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "LOB_2021" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "LYB_2020" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "LYB_2021" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "PET_2020" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "PET_2021" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "ROR_2020" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "ROR_2021" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "SPJ_2020" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "SPJ_2021" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "STE_2020" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "STE_2021" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "TAG_2020" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "TAG_2021" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "TOM_2020" ~ "Pastures (agricultural matrix)",
   Study_id == "11_Clough" & Network_id == "TOM_2021" ~ "Pastures (agricultural matrix)",

             TRUE ~ Land_cover))


#12_Ockinger (Pastures, author information)
ockinger_habitats = data %>% 
filter(Study_id == "12_Ockinger") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[1] ~ "Semi-natural grassland",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[2] ~ "Semi-natural grassland",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[3] ~ "Semi-natural grassland",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[4] ~ "Semi-natural grassland",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[5] ~ "Semi-natural grassland",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[6] ~ "Semi-natural grassland",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[7] ~ "Semi-natural grassland",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[8] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[9] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[10] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[11] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[12] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[13] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[14] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[15] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[16] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[17] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[18] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[19] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[20] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[21] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[22] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[23] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[24] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[25] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[26] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[27] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[28] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[29] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[30] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[31] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[31] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[32] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[33] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[34] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[35] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[36] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[37] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[38] ~ "Pastures (agricultural/vegetation matrix)",
            TRUE ~ Land_cover))

#Ahlezons_hage


#13_Karise (Agricultural land, author information)
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "13_Karise" ~ "Agricultural margins",
    TRUE ~ Land_cover))
#14_Dupont (heathland)
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "14_Dupont" ~ "Moors and heathland",
    TRUE ~ Land_cover))
#15_Magrach (Grasslands) By the look of it maybe semi-alpine 
#add it at alpine for simplicity
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "15_Magrach" ~ "Alpine grasslands",
    TRUE ~ Land_cover))

#16_Manincor (Grasslands)
manincor_habitats = data %>% 
filter(Study_id == "16_Manincor") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "16_Manincor" & Network_id == manincor_habitats[1] ~ "Semi-natural grassland",
    Study_id == "16_Manincor" & Network_id == manincor_habitats[2] ~ "Semi-natural grassland",
    Study_id == "16_Manincor" & Network_id == manincor_habitats[3] ~ "Semi-natural grassland",
    Study_id == "16_Manincor" & Network_id == manincor_habitats[4] ~ "Semi-natural grassland",
    Study_id == "16_Manincor" & Network_id == manincor_habitats[5] ~ "Semi-natural grassland",
    Study_id == "16_Manincor" & Network_id == manincor_habitats[6] ~ "Semi-natural grassland",

    TRUE ~ Land_cover))

#17_Fisogni (Grasslands)
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "17_Fisogni" ~ "Transitional woodland-shrub",
    TRUE ~ Land_cover))


#18_Bartomeus 
bartomeus_habitats = data %>% 
filter(Study_id == "18_Bartomeus") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
  Study_id == "18_Bartomeus" ~ "Open forest understory",
  
    TRUE ~ Land_cover))





#19_Jauker (Calcareous grasslands) It is close to an urban environment
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "19_Jauker" ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))

#20_Hoiss
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "20_Hoiss" ~ "Pastures (vegetation matrix)",
    TRUE ~ Land_cover))

#21_Hopfenmuller
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "21_Hopfenmuller" ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))

#22_Kallnik
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "22_Kallnik" ~ "Pastures (vegetation matrix)",
    TRUE ~ Land_cover))

#23_Holzschuh
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "23_Holzschuh" ~ "Agricultural margins",
    TRUE ~ Land_cover))
#24_Holzschuh
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "24_Holzschuh" ~ "Cropland",
    TRUE ~ Land_cover))
#25_Holzschuh
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "25_Holzschuh" ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))

#26_Castro (seems ok with Corine)
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "26_Castro" ~ "Cropland",
    TRUE ~ Land_cover))

#27_Castro 
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "27_Castro" & Habitat == "pasture" ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "27_Castro" & Habitat == "semi natural area" ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "27_Castro" & Habitat == "shrubland 1" ~ "Sclerophyllous vegetation",
    Study_id == "27_Castro" & Habitat == "shrubland 2" ~ "Sclerophyllous vegetation",
    TRUE ~ Land_cover))

#28_Sutter
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "28_Sutter" ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))

#29_Magrach
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "29_Magrach" ~ "Open forest understory",
    TRUE ~ Land_cover))

#30_Smith
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "30_Smith" ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))

#31_Roberts
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "31_Roberts" ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover)) %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "31_Roberts" & Network_id == "Chamberlain" ~ "Agricultural margins",
    Study_id == "31_Roberts" & Network_id == "Hunt" ~ "Semi-natural grassland",
    TRUE ~ Land_cover))


#32_ORourke

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "32_ORourke" ~ "Beaches, dunes, sands",
    TRUE ~ Land_cover))

#33_Power
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "33_Power" ~ "Pastures",
    TRUE ~ Land_cover))

#Habitats have changed... Doing it with less resolution
#power_habitats = data %>% 
#filter(Study_id == "33_Power") %>% 
#distinct(Network_id) %>% pull()
#
#habitat = habitat %>% 
#mutate(Land_cover = 
#  case_when(
#    Study_id == "33_Power" & Network_id == power_habitats[1]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[2]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[3]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[4]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[5]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[6]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[7]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[8]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[9]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[10]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[11]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[12]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[13]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[14]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[15]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[16]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[17]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[18]~ "Pastures (agricultural matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[19]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "33_Power" & Network_id == power_habitats[20]~ "Pastures (agricultural/vegetation matrix)",
#    TRUE ~ Land_cover))

#34_Stanley
#Pastures 
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "34_Stanley" ~ "Pastures",
    TRUE ~ Land_cover))

#stanley_habitats = data %>% 
#filter(Study_id == "34_Stanley") %>% 
#distinct(Network_id) %>% pull()
#
#habitat = habitat %>% 
#mutate(Land_cover = 
#  case_when(
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[1]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[2]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[3]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[4]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[5]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[6]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[7]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[8]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[9]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[10]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[11]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[12]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[13]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[14]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[15]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[16]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[17]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[18]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[19]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[20]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[21]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[22]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[23]~ "Pastures (agricultural matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[24]~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "34_Stanley" & Network_id == stanley_habitats[25]~ "Pastures (agricultural matrix)",
#        TRUE ~ Land_cover))
#

#35_Mullen
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "35_Mullen" ~ "Pastures (agricultural matrix)",
    TRUE ~ Land_cover))

#36_Larkin
larkin_habitats = data %>% 
filter(Study_id == "36_Larkin") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "36_Larkin" & Network_id == larkin_habitats[1]~ "Pastures (agricultural matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[2]~ "Pastures (agricultural matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[3]~ "Pastures (agricultural matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[4]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[5]~ "Pastures (agricultural matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[6]~ "Pastures (agricultural matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[7]~ "Pastures (agricultural matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[8]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[9]~ "Pastures (agricultural matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[10]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[11]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[12]~ "Pastures (agricultural matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[13]~ "Pastures (agricultural matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[14]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[15]~ "Pastures (vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[16]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[17]~ "Pastures (agricultural matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[18]~ "Pastures (vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[19]~ "Pastures (vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[20]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[21]~ "Pastures (vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[22]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "36_Larkin" & Network_id == larkin_habitats[23]~ "Pastures (vegetation matrix)",

        TRUE ~ Land_cover))

#37_White
white_habitats = data %>% 
filter(Study_id == "37_White") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "37_White" & Network_id == white_habitats[1]~ "Pastures (agricultural matrix)",
    Study_id == "37_White" & Network_id == white_habitats[2]~ "Pastures (agricultural matrix)",
    Study_id == "37_White" & Network_id == white_habitats[3]~ "Pastures (agricultural matrix)",
    Study_id == "37_White" & Network_id == white_habitats[4]~ "Pastures (agricultural matrix)",
    Study_id == "37_White" & Network_id == white_habitats[5]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "37_White" & Network_id == white_habitats[6]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "37_White" & Network_id == white_habitats[7]~ "Pastures (vegetation matrix)",
    Study_id == "37_White" & Network_id == white_habitats[8]~ "Pastures (agricultural matrix)",
    Study_id == "37_White" & Network_id == white_habitats[9]~ "Pastures (agricultural matrix)",
    Study_id == "37_White" & Network_id == white_habitats[10]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "37_White" & Network_id == white_habitats[11]~ "Pastures (agricultural matrix)",
    Study_id == "37_White" & Network_id == white_habitats[12]~ "Pastures (agricultural matrix)",
    Study_id == "37_White" & Network_id == white_habitats[13]~ "Pastures (agricultural matrix)",
    Study_id == "37_White" & Network_id == white_habitats[14]~ "Pastures (agricultural matrix)",
    Study_id == "37_White" & Network_id == white_habitats[15]~ "Pastures (vegetation matrix)",
    Study_id == "37_White" & Network_id == white_habitats[16]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "37_White" & Network_id == white_habitats[17]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "37_White" & Network_id == white_habitats[18]~ "Pastures (vegetation matrix)",
    Study_id == "37_White" & Network_id == white_habitats[19]~ "Pastures (agricultural/urban/vegetation matrix)",
    Study_id == "37_White" & Network_id == white_habitats[20]~ "Green urban areas",
    Study_id == "37_White" & Network_id == white_habitats[21]~ "Pastures (agricultural matrix)",
     TRUE ~ Land_cover))

#38_Maurer
maurer_habitats = data %>% 
filter(Study_id == "38_Maurer") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "38_Maurer" & Network_id == maurer_habitats[1]~ "Pastures (agricultural matrix)",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[2]~ "Pastures (agricultural matrix)",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[3]~ "Pastures (agricultural matrix)",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[4]~ "Pastures (agricultural matrix)",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[5]~ "Pastures (agricultural matrix)",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[6]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[7]~ "Pastures (agricultural/urban matrix)",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[8]~ "Pastures (agricultural matrix)",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[9]~ "Green urban areas",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[10]~ "Green urban areas",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[11]~ "Green urban areas",
    Study_id == "38_Maurer" & Network_id == maurer_habitats[12]~ "Green urban areas",
     TRUE ~ Land_cover))

#39_Schweiger
schweiger_habitats = data %>% 
filter(Study_id == "39_Schweiger") %>% 
distinct(Habitat) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "39_Schweiger" & Habitat == schweiger_habitats[1]~ "Cropland",
    Study_id == "39_Schweiger" & Habitat == schweiger_habitats[2]~ "Road side",
    Study_id == "39_Schweiger" & Habitat == schweiger_habitats[3]~ "Agro-forestry areas",
    Study_id == "39_Schweiger" & Habitat == schweiger_habitats[4]~ "Transitional woodland-shrub",
    Study_id == "39_Schweiger" & Habitat == schweiger_habitats[5]~ "Pastures (agricultural/vegetation matrix)",
        TRUE ~ Land_cover)) %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "39_Schweiger" & Network_id == "U1_semi.natural.grass_1" ~ "Green urban areas",  
    Study_id == "39_Schweiger" & Network_id == "U3_semi.natural.grass_4" ~ "Green urban areas",  
    Study_id == "39_Schweiger" & Network_id == "U3_semi.natural.grass_6" ~ "Green urban areas",  
    Study_id == "39_Schweiger" & Network_id == "U4_semi.natural.grass_5" ~ "Green urban areas",  
TRUE ~ Land_cover)) %>% 
mutate(Land_cover =  case_when(
    Study_id == "39_Schweiger" & Habitat == schweiger_habitats[6] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "39_Schweiger" & Habitat == schweiger_habitats[7] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "39_Schweiger" & Habitat == schweiger_habitats[8] ~ "Coniferous forest",
    Study_id == "39_Schweiger" & Habitat == schweiger_habitats[9] ~ "Coniferous forest",
    Study_id == "39_Schweiger" & Habitat == schweiger_habitats[10] ~ "Green urban areas",
      TRUE ~ Land_cover))

#40_Knight    
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "40_Knight" ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover)) 

#41_Knight
knight_habitats = data %>% 
filter(Study_id == "41_Knight") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "41_Knight" & Network_id == knight_habitats[1] ~ "Forest/woodland",
    Study_id == "41_Knight" & Network_id == knight_habitats[2] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "41_Knight" & Network_id == knight_habitats[3] ~ "Pastures (vegetation matrix)",
    Study_id == "41_Knight" & Network_id == knight_habitats[4] ~ "Forest/woodland",
    Study_id == "41_Knight" & Network_id == knight_habitats[5] ~ "Forest/woodland",
    Study_id == "41_Knight" & Network_id == knight_habitats[6] ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover)) 

#42_Knight
knight2_habitats = data %>% 
filter(Study_id == "42_Knight") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "42_Knight" & Network_id == knight2_habitats[1] ~ "Pastures (agricultural/urban/vegetation matrix)",
    Study_id == "42_Knight" & Network_id == knight2_habitats[2] ~ "Pastures (agricultural/urban/vegetation matrix)",
    Study_id == "42_Knight" & Network_id == knight2_habitats[3] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "42_Knight" & Network_id == knight2_habitats[4] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "42_Knight" & Network_id == knight2_habitats[5] ~ "Pastures (agricultural/urban/vegetation matrix)",
    Study_id == "42_Knight" & Network_id == knight2_habitats[6] ~ "Pastures (agricultural/urban/vegetation matrix)",
    Study_id == "42_Knight" & Network_id == knight2_habitats[7] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "42_Knight" & Network_id == knight2_habitats[8] ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))    

#43_Knight
knight3_habitats = data %>% 
filter(Study_id == "43_Knight") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "43_Knight" & Network_id == knight3_habitats[1] ~ "Pastures (vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[2] ~ "Pastures (vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[3] ~ "Pastures (urban/vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[4] ~ "Pastures (urban/vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[5] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[6] ~ "Pastures (vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[7] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[8] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[9] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[10] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[11] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "43_Knight" & Network_id == knight3_habitats[12] ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))  

#44_Knight
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "44_Knight" ~ "Pastures",
    TRUE ~ Land_cover))

#knight4_habitats = data %>% 
#filter(Study_id == "44_Knight") %>% 
#distinct(Network_id) %>% pull()
    
#habitat = habitat %>% 
#mutate(Land_cover = 
#  case_when(
#    Study_id == "44_Knight" & Network_id == knight4_habitats[1] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[2] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[3] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[4] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight3_habitats[5] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[6] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[7] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[8] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[9] ~ "Pastures (vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[10] ~ "Pastures (agricultural matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[11] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[12] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[13] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[14] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[15] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[16] ~ "Pastures (agricultural matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[17] ~ "Pastures (agricultural/vegetation matrix)",
#    Study_id == "44_Knight" & Network_id == knight4_habitats[18] ~ "Pastures (agricultural/vegetation matrix)",
#    TRUE ~ Land_cover))  

#45_Knight
knight5_habitats = data %>% 
filter(Study_id == "45_Knight") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "45_Knight" & Network_id == knight5_habitats[1] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "45_Knight" & Network_id == knight5_habitats[2] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "45_Knight" & Network_id == knight5_habitats[3] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "45_Knight" & Network_id == knight5_habitats[4] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "45_Knight" & Network_id == knight5_habitats[5] ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))  

#46_Knight
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "46_Knight" ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))

#47_Benadi
benadi_habitats = data %>% 
filter(Study_id == "47_Benadi") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "47_Benadi" ~ "Alpine grasslands",
    TRUE ~ Land_cover))

#habitat = habitat %>% 
#mutate(Land_cover = 
#  case_when(
#    Study_id == "47_Benadi" & Network_id == benadi_habitats[1] ~ "Natural grasslands",
#    Study_id == "47_Benadi" & Network_id == benadi_habitats[2] ~ "Pastures (vegetation matrix)",
#    Study_id == "47_Benadi" & Network_id == benadi_habitats[3] ~ "Pastures (vegetation matrix)",
#    Study_id == "47_Benadi" & Network_id == benadi_habitats[4] ~ "Pastures (vegetation matrix)",
#    Study_id == "47_Benadi" & Network_id == benadi_habitats[5] ~ "Natural grasslands",
#    Study_id == "47_Benadi" & Network_id == benadi_habitats[6] ~ "Natural grasslands",
#    TRUE ~ Land_cover))  

#48_Lara-Romero
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "48_Lara-Romero" ~ "Alpine grasslands",
    TRUE ~ Land_cover))

#49_Hervias-Parejo
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "49_Hervias-Parejo" ~ "Sclerophyllous vegetation",
    TRUE ~ Land_cover))

#50_Hervias-Parejo
hervias_parejo_habitats = data %>% 
filter(Study_id == "50_Hervias-Parejo") %>% 
distinct(Habitat) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "50_Hervias-Parejo" & Habitat == hervias_parejo_habitats[1] ~ "Beaches, dunes, sands",
    Study_id == "50_Hervias-Parejo" & Habitat == hervias_parejo_habitats[2] ~ "Sclerophyllous vegetation",
    Study_id == "50_Hervias-Parejo" & Habitat == hervias_parejo_habitats[3] ~ "Sclerophyllous vegetation",
    TRUE ~ Land_cover))  

#51_Petanidou
habitat = habitat %>% 
mutate(Land_cover = 
case_when(Study_id == "51_Petanidou" ~ "Sclerophyllous vegetation",
TRUE ~ Land_cover))


#Check for mistakes
#Rename coniferous forest to forest
habitat = habitat %>% 
mutate(Land_cover = recode_factor(Land_cover,  
              "Coniferous forest" = "Forest/woodland",
              "Open forest understory" = "Sclerophyllous vegetation",
              "Pastures (vegetation matrix)" = "Semi-natural grassland",
              "Pastures (agricultural/vegetation matrix)" = "Pastures",
              "Pastures (agricultural matrix)" = "Pastures",
              "Pastures (urban/vegetation matrix)" = "Pastures",
               "Pastures (agricultural/urban/vegetation matrix)" = "Pastures",
              "Pastures (agricultural/urban matrix)" = "Pastures",
              "Road side" = "Ruderal vegetation",
              "Mineral extraction sites" = "Ruderal vegetation",
               "Cropland" = "Agricultural land",
               "Botanical garden" = "Green urban areas",
              "Agro-forestry areas" = "Forest/woodland",
              "Forest" = "Forest/woodland",
              "Transitional woodland-shrub" = "Forest/woodland"))

#Check levels
levels(factor(habitat$Land_cover))

pasture = habitat %>% 
filter(Land_cover == "Pastures") 

library(forcats)
library(ggplot2)
ggplot(habitat, aes(fct_infreq(Land_cover))) +
geom_bar(fill = "gray20") +
theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
ylab("Number of networks") +
xlab(NULL)


#Now rename cols and organise the dataset before saving it
habitat = habitat %>% 
rename("Authors_habitat" = "Habitat",
        "SafeNet_habitat" = "Land_cover") %>% 
select(Study_id, Network_id, Authors_habitat, SafeNet_habitat, Latitude,
       Longitude)

check = habitat %>% 
filter(is.na(SafeNet_habitat))

#Save data
saveRDS(habitat, "Data/Working_files/Habitat.rds")
