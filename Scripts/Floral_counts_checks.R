


#23_hols next to check

#Check floral counts
#Floral count datasets
no_flower_counts = c("3_Michez", "13_Karise", "14_Dupont", "17_Fisogni")
problematic_floral_counts =c("1_Bartomeus", "9_Heleno", "16_Manincor", "18_Bartomeus",
                             "20_Hoiss", "21_Hopfenmuller")
floral_counts =c("2_Petanidou", "4_Marini", "5_Marini", 
                 "6_Marini", "7_Scheper", "8_Biella",
                 "10_Vanbergen", "11_Clough", "12_Ockinger",
                 "15_Magrach", "22_Kallnik")

#Things to do!
#Flower count data issues: 1_Bartomeus
a = bind_rows(InteractionData)
b = bind_rows(FlowerCount)
str(b)
str(a)
d = left_join(a, b, by = join_by(Plant_species, Site_id, Year, Month, Day))


unique(a$Year)
