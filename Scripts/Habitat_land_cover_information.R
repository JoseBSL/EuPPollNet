#Extract habitat type by coordinates

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

#Merge back to dataset with everything
data1 = left_join(data, all, by=c("Longitude", "Latitude")) %>% 
rename("Corine_land_cover" = "LABEL3")

#Explore data (data1)
habitat = data1 %>% 
select(Study_id, Network_id, Habitat, Corine_land_cover, Latitude, Longitude) %>% 
distinct() %>% 
mutate(Land_cover = Corine_land_cover)

#Check by study
#We are going to standardise according to authors matching
#Edits are conducted on Land_cover column


#Cropland: 
#cultivars, margins or pastures embedded in an agricultural matrix
#Pastures
#Grassy patches with low height plants that tend to be grazed or
#mowed in a diverse type of environments


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
    Study_id == "5_Marini" & Network_id == "UNIPD02 AJ" ~ "Pastures (agricultural/vegetation matrix)",
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
    Study_id == "5_Marini" & Network_id == "UNIPD02 AX" ~ "Pastures (urban/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 AZ" ~ "Pastures (agricultural matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 B" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 C" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 D" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 E" ~ "Pastures (agricultural/urban/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 F" ~ "Pastures (agricultural/urban matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 G" ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 H" ~ "Pastures (agricultural/urban/vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 I" ~ "Pastures (vegetation matrix)",
    Study_id == "5_Marini" & Network_id == "UNIPD02 J" ~ "Pastures (agricultural/vegetation matrix)",
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
    Study_id == "5_Marini" & Network_id == "UNIPD02 Z" ~ "Pastures (agricultural/vegetation matrix)",

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
  case_when(Study_id == "8_Biella" ~ "Natural grasslands",
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
   Study_id == "11_Clough" & Network_id == "ASK_2020" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "ASK_2021" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "DAL_2020" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "DAL_2021" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "GLA_2020" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "GLA_2021" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "GYL_2020" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "GYL_2021" ~ "Pastures (agricultural/vegetation matrix)",
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
   Study_id == "11_Clough" & Network_id == "KOM_2020" ~ "Pastures (agricultural/vegetation matrix)",
   Study_id == "11_Clough" & Network_id == "KOM_2021" ~ "Pastures (agricultural/vegetation matrix)",
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
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[1] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[2] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[3] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[4] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[5] ~ "Pastures (vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[6] ~ "Pastures (agricultural/vegetation matrix)",
  Study_id == "12_Ockinger" & Network_id == ockinger_habitats[7] ~ "Pastures (agricultural/vegetation matrix)",
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
#15_Magrach (Grasslands) Quite happy with the classification of Corine
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "15_Magrach" ~ "Natural grasslands",
    TRUE ~ Land_cover))

#16_Manincor (Grasslands)
manincor_habitats = data %>% 
filter(Study_id == "16_Manincor") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "16_Manincor" & Network_id == manincor_habitats[1] ~ "Natural grasslands",
    Study_id == "16_Manincor" & Network_id == manincor_habitats[2] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "16_Manincor" & Network_id == manincor_habitats[3] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "16_Manincor" & Network_id == manincor_habitats[4] ~ "Natural grasslands",
    Study_id == "16_Manincor" & Network_id == manincor_habitats[5] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "16_Manincor" & Network_id == manincor_habitats[6] ~ "Pastures (agricultural/vegetation matrix)",

    TRUE ~ Land_cover))

#17_Fisogni (Grasslands)
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "17_Fisogni" ~ "Pastures (vegetation matrix)",
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
    TRUE ~ Land_cover))

#32_ORourke

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "32_ORourke" ~ "Beaches, dunes, sands",
    TRUE ~ Land_cover))

#33_Power
power_habitats = data %>% 
filter(Study_id == "33_Power") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "33_Power" & Network_id == power_habitats[1]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[2]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[3]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[4]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[5]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[6]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[7]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[8]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[9]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[10]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[11]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[12]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[13]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[14]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[15]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[16]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[17]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[18]~ "Pastures (agricultural matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[19]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "33_Power" & Network_id == power_habitats[20]~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))

#34_Stanley
stanley_habitats = data %>% 
filter(Study_id == "34_Stanley") %>% 
distinct(Network_id) %>% pull()

habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "34_Stanley" & Network_id == stanley_habitats[1]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[2]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[3]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[4]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[5]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[6]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[7]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[8]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[9]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[10]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[11]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[12]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[13]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[14]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[15]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[16]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[17]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[18]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[19]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[20]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[21]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[22]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[23]~ "Pastures (agricultural matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[24]~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "34_Stanley" & Network_id == stanley_habitats[25]~ "Pastures (agricultural matrix)",
        TRUE ~ Land_cover))

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
    Study_id == "41_Knight" & Network_id == knight_habitats[1] ~ "Pastures (vegetation matrix)",
    Study_id == "41_Knight" & Network_id == knight_habitats[2] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "41_Knight" & Network_id == knight_habitats[3] ~ "Pastures (vegetation matrix)",
    Study_id == "41_Knight" & Network_id == knight_habitats[4] ~ "Pastures (vegetation matrix)",
    Study_id == "41_Knight" & Network_id == knight_habitats[5] ~ "Pastures (agricultural/vegetation matrix)",
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
knight4_habitats = data %>% 
filter(Study_id == "44_Knight") %>% 
distinct(Network_id) %>% pull()
    
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(
    Study_id == "44_Knight" & Network_id == knight4_habitats[1] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[2] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[3] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[4] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight3_habitats[5] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[6] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[7] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[8] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[9] ~ "Pastures (vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[10] ~ "Pastures (agricultural matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[11] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[12] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[13] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[14] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[15] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[16] ~ "Pastures (agricultural matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[17] ~ "Pastures (agricultural/vegetation matrix)",
    Study_id == "44_Knight" & Network_id == knight4_habitats[18] ~ "Pastures (agricultural/vegetation matrix)",
    TRUE ~ Land_cover))  

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
  case_when(
    Study_id == "47_Benadi" & Network_id == benadi_habitats[1] ~ "Natural grasslands",
    Study_id == "47_Benadi" & Network_id == benadi_habitats[2] ~ "Pastures (vegetation matrix)",
    Study_id == "47_Benadi" & Network_id == benadi_habitats[3] ~ "Pastures (vegetation matrix)",
    Study_id == "47_Benadi" & Network_id == benadi_habitats[4] ~ "Pastures (vegetation matrix)",
    Study_id == "47_Benadi" & Network_id == benadi_habitats[5] ~ "Natural grasslands",
    Study_id == "47_Benadi" & Network_id == benadi_habitats[6] ~ "Natural grasslands",
    TRUE ~ Land_cover))  

#48_Lara-Romero
habitat = habitat %>% 
mutate(Land_cover = 
  case_when(Study_id == "48_Lara-Romero" ~ "Natural grasslands",
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
    Study_id == "50_Hervias-Parejo" & Habitat == hervias_parejo_habitats[3] ~ "Agro-forestry areas",
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
              "Coniferous forest" = "Forest",
              "Open forest understory" = "Sclerophyllous vegetation" ))

#Check levels
levels(factor(habitat$Land_cover))

library(forcats)
ggplot(habitat, aes(fct_infreq(Land_cover))) +
geom_bar(fill = "gray20") +
theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
ylab("Number of sites")
