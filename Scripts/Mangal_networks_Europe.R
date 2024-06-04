
#Check number of plant-pollinator networks in Europe (Mangal)
library(rmangal)
#Download all datasets
all_datasets = search_datasets("")
#Create tibble with info of interest
d = tibble(name = all_datasets$name, description = all_datasets$description)
#This seems the only plant-pollinator studies within Europe
v = c("bartomeus_2005",
      "elberling_olesen_1999")






  