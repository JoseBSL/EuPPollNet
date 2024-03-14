


#23_hols next to check

#Check floral counts
#Floral count datasets
no_flower_counts = c("3_Michez", "13_Karise", "14_Dupont", "17_Fisogni")

problematic_floral_counts =c("1_Bartomeus","10_Vanbergen","16_Manincor", "18_Bartomeus",
                             "21_Hopfenmuller")

floral_counts =c("2_Petanidou", "4_Marini", "5_Marini", 
                 "6_Marini", "7_Scheper", "8_Biella","9_Heleno",
                 "11_Clough", "12_Ockinger",
                 "15_Magrach","20_Hoiss", "22_Kallnik")

#Things to do!
#Flower count data issues: 1_Bartomeus
a = bind_rows(InteractionData1)
b = bind_rows(flower_count1)

non_unique_df <- b %>%
  filter(duplicated(b) | duplicated(b, fromLast = TRUE))


str(b)
str(a)

d = left_join(a, b, by = join_by(Plant_species, Site_id, Day, Month, Year))


d = left_join(a, b, by = join_by(Flower_data_merger),  suffix=c("",".y"))  %>% 
select(-ends_with(".y"))

many_to_many = d %>% 
  group_by(Plant_species,Site_id,Day,Month,Year) %>% 
  summarise(length(Plant_species))


check = a %>% 
  group_by(Plant_species,Site_id,Day,Month,Year) %>% 
  summarise(length(Plant_species))

z = tibble(species = many_to_many$Plant_species,x = many_to_many$`length(Plant_species)`,x1 = check$`length(Plant_species)`)

check1 = z %>% 
filter(!x==x1)
