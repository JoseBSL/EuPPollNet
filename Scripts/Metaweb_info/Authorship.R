#Script to prepare author information

#Load libraries
library('gtools') #Library that allows to sort files by numerical order
library(tidyverse)
library(readr)
library(tibble)


#Prepare tibble with author data----
#Read all files with their paths
files <- dir("Data/2_Processed_data", pattern="*.rds", full.names=T)
files <- mixedsort(files)
#Generate list of lists
all_data = files %>%
map(readRDS) 
#Read all file names
file_names <- dir("Data/2_Processed_data", pattern="*.rds", full.names=F)
file_names <- mixedsort(file_names)
#Delete extension
file_names = str_replace(file_names, ".rds", "")
#Rename all elements of the list
names(all_data) <- file_names
#Select coordinates from the list of lists
data <- list()
for (i in 1:length(file_names)) {
  
  int_list <- all_data[[file_names[i]]]$Authorship
  data[[i]] <- bind_rows(int_list)
}

#Rename list and 
names(data) <- file_names
data = as_tibble(bind_rows(data,  .id = 'Study_id'))

#Save metadata file
write_csv(data, "Data/Manuscript_info/Authorship.csv")

#Order authors in the right order----
#Read data
data = read_csv("Data/Manuscript_info/Authorship.csv")
#Include Nerea Montes and Will Glenny
colnames(data)
#Exclude Gita Benadi. Gita didn't want to be included as is longer active in science
data = data %>% 
filter(!Coauthor_name == "Gita Benadi")
#Exlude Tiffany and Nacho to place them in the right order
second_info = data %>% 
filter(Coauthor_name == "Tiffany M. Knight") %>% 
  distinct(Coauthor_name, .keep_all = TRUE)
last_info = data %>% 
filter(Coauthor_name == "Ignasi Bartomeus") %>% 
  distinct(Coauthor_name, .keep_all = TRUE)
data = data %>% 
filter(!Coauthor_name == "Tiffany M. Knight" & !Coauthor_name =="Ignasi Bartomeus")
#Fix one name
data = data %>% 
mutate(Coauthor_name = case_when(
  Coauthor_name == "Elena Motivans Švara" ~ "Elena Motivans",
  Coauthor_name == "Paula Dominguez Lapido" ~ "Paula Dominguez-Lapido",
  T ~ Coauthor_name))


#Prepare author data for article
authors = data
#Create alternative column to order authors
authors$authors_ordered = word(authors$Coauthor_name,-1)
#Fix manually some authors 
#as the last name is not the correct one to assign order
authors = authors %>% 
mutate(authors_ordered = case_when(
  authors_ordered == "Manincor" ~ "de",
  T~authors_ordered))
#Order dataset by last word of author's name
ordered_data = authors[order(authors$authors_ordered),]
#Filter for duplicated author names
ordered_authors = ordered_data %>% 
  distinct(Coauthor_name, .keep_all = TRUE)
#Add Nacho to last
ordered_authors = bind_rows(ordered_authors, last_info)
#Add Tiffany second 
ordered_authors = bind_rows(second_info, ordered_authors)

#Add rows
ordered_authors = ordered_authors %>% 
add_row(Study_id = NA_character_, 
        Coauthor_name = "Will Glenny", 
        Orcid = "0000-0002-2522-3775", 
        E_mail = "willglenny@gmail.com", .before = 2)

#Add rows
ordered_authors = ordered_authors %>% 
add_row(Study_id = NA_character_, 
        Coauthor_name = "Nerea Montes-Perez", 
        Orcid = "0000-0002-5212-4537", 
        E_mail = "montespereznerea@gmail.com", .before = 2)
#Add myself
ordered_authors = ordered_authors %>% 
add_row(Study_id = NA_character_, 
        Coauthor_name = "Jose B. Lanuza", 
        Orcid = "0000-0002-0287-409X", 
        E_mail = "barragansljose@gmail.com", .before=1)

ordered_authors_unique = ordered_authors %>% 
distinct(Coauthor_name)
#Print authors
ordered_authors %>% 
summarise(col = paste(Coauthor_name, collapse=", ")) %>% 
pull()
#Save 
#write_csv(ordered_authors, "Data/Manuscript_info/Authorship_ordered.csv")


#Add affiliations following order of authors------
##Jose B. Lanuza----
affiliations = ordered_authors %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Jose B. Lanuza"  ~ 
"Estación Biológica de Doñana (EBD-CSIC), Seville, Spain",
TRUE ~ NA_character_)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Jose B. Lanuza"  ~ 
"Martin Luther University Halle-Wittenberg, Institute of Biology, Halle, Germany",
TRUE ~ NA_character_)) %>% 
mutate(Affiliation3 = case_when(Coauthor_name == "Jose B. Lanuza"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ NA_character_)) 
##Tiffany Knight-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Tiffany M. Knight"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Tiffany M. Knight"  ~ 
"Martin Luther University Halle-Wittenberg, Institute of Biology, Halle, Germany",
TRUE ~ Affiliation2)) %>% 
mutate(Affiliation3 = case_when(Coauthor_name == "Tiffany M. Knight"  ~ 
"Department of Community Ecology, Helmholtz Centre for Environmental Research – UFZ, Halle, Germany",
TRUE ~ Affiliation3))
##Nerea Montes-Perez-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Nerea Montes-Perez"  ~ 
"Estación Biológica de Doñana (EBD-CSIC), Seville, Spain",
TRUE ~ Affiliation1))
##Will Glenny-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Will Glenny"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Will Glenny"  ~ 
"Department of Community Ecology, Helmholtz Centre for Environmental Research – UFZ, Halle, Germany",
TRUE ~ Affiliation2))

##Paola Acuña-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Paola Acuña"  ~ 
"Department of Life Sciences, Centre for Functional Ecology, University of Coimbra, Coimbra, Portugal",
TRUE ~ Affiliation1))
##Matthias Albrecht-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Matthias Albrecht"  ~ 
"Agroecology and Environment, Agroscope, Zürich, Switzerland",
TRUE ~ Affiliation1))
##Maddi Artamendi----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Maddi Artamendi" ~ 
"Basque Centre for Climate Change-BC3, Leioa, Spain",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Maddi Artamendi" ~ 
"Universidad del País Vasco, EuskalHerriko Unibertsitatea (UPV-EHU), Leioa, Spain",
TRUE ~ Affiliation2))
##Isabelle Badenhausser----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Isabelle Badenhausser"  ~ 
"Centre d'Etudes Biologiques de Chizé, Université de La Rochelle, Villiers en Bois, France",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Isabelle Badenhausser"  ~ 
"LTSER \"ZA Plaine & Val de Sèvre\", CNRS, Villiers en Bois, France",
TRUE ~ Affiliation2)) %>% 
mutate(Affiliation3 = case_when(Coauthor_name == "Isabelle Badenhausser"  ~ 
"Unité de Recherche Pluridisciplinaire Prairies Plantes Fourragères, INRA, Lusignan, France",
TRUE ~ Affiliation3))
##Joanne M. Bennett----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Joanne M. Bennett"  ~ 
"Fenner School of Environment & Society, The Australian National University, Canberra, Australia",
TRUE ~ Affiliation1))%>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Joanne M. Bennett"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation2)) %>% 
mutate(Affiliation3 = case_when(Coauthor_name == "Joanne M. Bennett"  ~ 
"Martin Luther University Halle-Wittenberg, Institute of Biology, Halle, Germany",
TRUE ~ Affiliation3))
##Paolo Biella-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Paolo Biella"  ~ 
"ZooPlantLab, Department of Biotechnology and Biosciences, University of Milano-Bicocca, Milan, Italy",
TRUE ~ Affiliation1)) 
##Ricardo Bommarco----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Ricardo Bommarco"  ~ 
"Department of Ecology, Swedish University of Agricultural Sciences, Uppsala, Sweden",
TRUE ~ Affiliation1)) 
##Andree Cappellari-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Andree Cappellari"  ~ 
"Department of Agronomy, Food, Natural Resources, Animals and Environment, University of Padua, Padua, Italy",
TRUE ~ Affiliation1)) 
##Sílvia Castro-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Sílvia Castro"  ~ 
"Department of Life Sciences, Centre for Functional Ecology, University of Coimbra, Coimbra, Portugal",
TRUE ~ Affiliation1)) 
##Yann Clough-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Yann Clough"  ~ 
"Centre for Environmental and Climate Science, Lund University, Lund, Sweden",
TRUE ~ Affiliation1)) 
##Pau Colom-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Pau Colom"  ~ 
"Mediterranean Institute for Advanced Studies (IMEDEA, UIB-CSIC), Esporles, Spain",
TRUE ~ Affiliation1)) 
##Joana Costa-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Joana Costa"  ~ 
"Department of Life Sciences, Centre for Functional Ecology, University of Coimbra, Coimbra, Portugal",
TRUE ~ Affiliation1)) 
##Christophe Dominik----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Christophe Dominik"  ~ 
"Department of Community Ecology, Helmholtz Centre for Environmental Research – UFZ, Halle, Germany",
TRUE ~ Affiliation1)) 
##Yoko L. Dupont----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Yoko L. Dupont"  ~ 
"Department of Ecoscience, University of Aarhus, Aarhus, Denmark",
TRUE ~ Affiliation1)) 
##Reinart Feldmann----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Reinart Feldmann"  ~ 
"Helmholtz Centre for Environmental Research – UFZ, Leipzig, Germany",
TRUE ~ Affiliation1)) 
##Emeline Felten----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Emeline Felten"  ~ 
"Agroécologie, INRAE, Institut Agro, Université de Bourgogne, Université de Bourgogne Franche-Comté, Dijon, France",
TRUE ~ Affiliation1)) 
##Victoria Ferrero----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Victoria Ferrero"  ~ 
"Department of Biodiversity and Environmental Management, University of León, León, Spain",
TRUE ~ Affiliation1))
##William Fiordaliso----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "William Fiordaliso"  ~ 
"Ecologie des Interactions et Changements Globaux, Institut de recherche des Biosciences, Université de Mons, Mons, Belgium",
TRUE ~ Affiliation1))
##Alessandro Fisogni----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Alessandro Fisogni"  ~ 
"Univ. Lille, CNRS, UMR 8198 - Evo-Eco-Paleo, F-59000 Lille, France",
TRUE ~ Affiliation1))
##Una Fitzpatrick----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Úna Fitzpatrick"  ~ 
"National Biodiversity Data Centre, County Waterford, Ireland",
TRUE ~ Affiliation1))
##Marta Galloni----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Marta Galloni"  ~ 
"Department of Biological, Geological and Environmental Sciences (BiGeA), University of Bologna, Bologna, Italy",
TRUE ~ Affiliation1))
##Hugo Gaspar----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Hugo Gaspar"  ~ 
"Department of Life Sciences, Centre for Functional Ecology, University of Coimbra, Coimbra, Portugal",
TRUE ~ Affiliation1))
##Elena Gazzea-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Elena Gazzea"  ~ 
"Department of Agronomy, Food, Natural Resources, Animals and Environment, University of Padua, Padua, Italy",
TRUE ~ Affiliation1))

##Irina Goia-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Irina Goia"  ~ 
"Faculty of Biology and Geology, Babeș-Bolyai University, Cluj-Napoca, Romania",
TRUE ~ Affiliation1))
##Juan Pedro González-Varo-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Juan Pedro González-Varo"  ~ 
"Departamento de Biología – INMAR, Universidad de Cádiz, Puerto Real, Spain",
TRUE ~ Affiliation1)) 

##Nina Hautekèete-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Jiří Hadrava"  ~ 
"Univ. Lille, CNRS, UMR 8198 - Evo-Eco-Paleo, F-59000 Lille, France",
TRUE ~ Affiliation1))

##Nina Hautekèete-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Nina Hautekèete"  ~ 
"Evo-Eco-Paleo, CNRS, Université de Lille, Lille, France",
TRUE ~ Affiliation1))
##Veronica Hederström-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Veronica Hederström"  ~ 
"Centre for Environmental and Climate Science, Lund University, Lund, Sweden",
TRUE ~ Affiliation1))
##Ruben Heleno----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Ruben Heleno"  ~ 
"Department of Life Sciences, Centre for Functional Ecology, University of Coimbra, Coimbra, Portugal",
TRUE ~ Affiliation1))
##Sandra Hervias-Parejo----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Sandra Hervias-Parejo"  ~ 
"Mediterranean Institute for Advanced Studies (IMEDEA, UIB-CSIC), Esporles, Spain",
TRUE ~ Affiliation1))
##Jonna Heuschele-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Jonna Heuschele"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Jonna Heuschele"  ~ 
"ESS Department, Helmholtz Centre for Environmental Research - UFZ, Leipzig,
Germany",
TRUE ~ Affiliation2)) %>% 
mutate(Affiliation3 = case_when(Coauthor_name == "Jonna Heuschele"  ~ 
"BZF Department, Helmholtz Centre for Environmental Research - UFZ, Halle
(Saale), Germany",
TRUE ~ Affiliation3))
##Bernhard Hoiss----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Bernhard Hoiss"  ~ 
"Department of Animal Ecology and Tropical Biology, Biocenter, University of Würzburg, Würzburg, Germany",
TRUE ~ Affiliation1))
##Andrea Holzschuh-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Andrea Holzschuh"  ~ 
"Department of Animal Ecology and Tropical Biology, Biocenter, University of Würzburg, Würzburg, Germany",
TRUE ~ Affiliation1))
##Sebastian Hopfenmüller----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Sebastian Hopfenmüller"  ~ 
"Cultural Landscape Günztal Foundation, Ottobeuren, Germany",
TRUE ~ Affiliation1))
##José M. Iriondo----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "José M. Iriondo"  ~ 
"Area of Biodiversity and Conservation, ESCET, Universidad Rey Juan Carlos, Madrid, Spain",
TRUE ~ Affiliation1))
##Birgit Jauker-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Birgit Jauker"  ~ 
"Justus Liebig University Giessen, Giessen, Germany",
TRUE ~ Affiliation1))
##Frank Jauker-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Frank Jauker"  ~ 
"Institute of Landscape Ecology and Resource Management, Justus Liebig University Giessen, Giessen, Germany",
TRUE ~ Affiliation1))
##Jana Jersáková-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Jana Jersáková"  ~ 
"Department of Ecosystems Biology, Faculty of Science, University of South Bohemia, České Budějovice, Czech Republic",
TRUE ~ Affiliation1))
##Katharina Kallnik------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Katharina Kallnik"  ~ 
"Department of Animal Ecology and Tropical Biology, Biocenter, University of Würzburg, Würzburg, Germany",
TRUE ~ Affiliation1))
##Reet Karise------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Reet Karise"  ~ 
"Institute of Agricultural and Environmental Sciences, Estonian University of Life Sciences, Tartu, Estonia",
TRUE ~ Affiliation1))
##David Kleijn------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "David Kleijn"  ~ 
"Nature Conservation and Plant Ecology Group, Wageningen University, Wageningen, The Netherlands",
TRUE ~ Affiliation1))
##Stefan Klotz-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Stefan Klotz"  ~ 
"Department of Community Ecology, Helmholtz Centre for Environmental Research – UFZ, Halle, Germany",
TRUE ~ Affiliation1))
##Theresia Krausl-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Theresia Krausl"  ~ 
"Centre for Environmental and Climate Science, Lund University, Lund, Sweden",
TRUE ~ Affiliation1))
##Elisabeth Kühn-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Elisabeth Kühn"  ~ 
"Department of Community Ecology, Helmholtz Centre for Environmental Research – UFZ, Halle, Germany",
TRUE ~ Affiliation1))
##Paula Dominguez Lapido-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Paula Dominguez Lapido"  ~ 
"Basque Centre for Climate Change-BC3, Leioa, Spain",
TRUE ~ Affiliation1))
##Carlos Lara-Romero-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Carlos Lara-Romero"  ~ 
"Area of Biodiversity and Conservation, ESCET, Universidad Rey Juan Carlos, Madrid, Spain",
TRUE ~ Affiliation1))
##Emilien Laurent-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Emilien Laurent"  ~ 
"Agroécologie, INRAE, Institut Agro, Université de Bourgogne, Université de Bourgogne Franche-Comté, Dijon, France",
TRUE ~ Affiliation1))
##Michelle Larkin-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Michelle Larkin"  ~ 
"Botany and Plant Science, School of Natural Sciences and Ryan Institute, National University of Ireland Galway, Galway, Ireland",
TRUE ~ Affiliation1))
##Yicong Liu-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Yicong Liu"  ~ 
"Department of Community Ecology, Helmholtz Centre for Environmental Research – UFZ, Halle, Germany",
TRUE ~ Affiliation1))%>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Yicong Liu"  ~ 
"Martin Luther University Halle-Wittenberg, Institute of Biology, Halle, Germany",
TRUE ~ Affiliation2))
##Sara Lopes-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Sara Lopes"  ~ 
"Department of Life Sciences, Centre for Functional Ecology, University of Coimbra, Coimbra, Portugal",
TRUE ~ Affiliation1))
##Francisco López-Núñez-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Francisco López-Núñez"  ~ 
"Department of Life Sciences, Centre for Functional Ecology, University of Coimbra, Coimbra, Portugal",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Francisco López-Núñez"  ~ 
"Research Centre for Natural Resources Environment and Society (CERNAS), Polytechnic Institute of Coimbra, Coimbra Agriculture School, Coimbra, Portugal",
TRUE ~ Affiliation2))

##João Loureiro----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "João Loureiro"  ~ 
"Department of Life Sciences, Centre for Functional Ecology, University of Coimbra, Coimbra, Portugal",
TRUE ~ Affiliation1))
##Ainhoa Magrach-------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Ainhoa Magrach"  ~ 
"Basque Centre for Climate Change-BC3, Leioa, Spain",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Ainhoa Magrach"  ~ 
"IKERBASQUE, Basque Foundation forScience, Bilbao, Spain",
TRUE ~ Affiliation2))
##Marika Mänd-------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Marika Mänd"  ~ 
"Institute of Agricultural and Environmental Sciences, Estonian University of Life Sciences, Tartu, Estonia",
TRUE ~ Affiliation1))
##Natasha de Manincor-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Natasha de Manincor"  ~ 
"Univ. Lille, CNRS, UMR 8198 - Evo-Eco-Paleo, F-59000 Lille, France",
TRUE ~ Affiliation1))%>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Natasha de Manincor"  ~ 
"Laboratoire de Zoologie, Institut de recherche des Biosciences, Université de Mons, Mons, Belgium",
TRUE ~ Affiliation2))


##Lorenzo Marini------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Lorenzo Marini"  ~ 
"Department of Agronomy, Food, Natural Resources, Animals and Environment, University of Padua, Padua, Italy",
TRUE ~ Affiliation1))
##Rafel Beltran Mas------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Rafel Beltran Mas"  ~ 
"Mediterranean Institute for Advanced Studies (IMEDEA, UIB-CSIC), Esporles, Spain",
TRUE ~ Affiliation1))
##François Massol------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "François Massol"  ~ 
"Univ. Lille, CNRS, UMR 8198 - Evo-Eco-Paleo, F-59000 Lille, France",
TRUE ~ Affiliation1))
##Corina Maurer------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Corina Maurer"  ~ 
"Agroecology and Environment, Agroscope, Zürich, Switzerland",
TRUE ~ Affiliation1)) 
##Denis Michez------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Denis Michez"  ~ 
"Laboratoire de Zoologie, Institut de recherche des Biosciences, Université de Mons, Mons, Belgium",
TRUE ~ Affiliation1))
##Francisco P. Molina------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Francisco P. Molina"  ~ 
"Estación Biológica de Doñana (EBD-CSIC), Seville, Spain",
TRUE ~ Affiliation1))
##Javier Morente-López------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Javier Morente-López"  ~ 
"Area of Biodiversity and Conservation, ESCET, Universidad Rey Juan Carlos, Madrid, Spain",
TRUE ~ Affiliation1))
##Sarah Mullen------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Sarah Mullen"  ~ 
"Botany Department, Trinity College Dublin, Dublin, Ireland",
TRUE ~ Affiliation1))
##Georgios Nakas------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Georgios Nakas"  ~ 
"Laboratory of Biogeography & Ecology, Department of Geography, University of the Aegean, Mytilene, Greece",
TRUE ~ Affiliation1))
##Lena Neuenkamp-------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Lena Neuenkamp"  ~ 
"Department of Botany, Institute of Ecology and Earth Sciences, University of Tartu, Tartu, Estonia",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Lena Neuenkamp"  ~ 
"Institute of Plant Sciences, University of Bern, Bern, Switzerland",
TRUE ~ Affiliation2))
##Arkadiusz Nowak-------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Arkadiusz Nowak"  ~ 
"Center for Biological Diversity Conservation, Polish Academy of Sciences, Botanical Garden, Warsaw, Poland",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Arkadiusz Nowak"  ~ 
"Laboratory of Geobotany and Plant Conservation, Department of Biosystematics, Opole University, Opole, Poland",
TRUE ~ Affiliation2))
##Catherine J. O’Connor-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Catherine J. O’Connor"  ~ 
"Department of Life Sciences, Centre for Functional Ecology, University of Coimbra, Coimbra, Portugal",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Catherine J. O’Connor"  ~ 
"Cardif School of Biosciences, Cardif University, Cardif, UK",
TRUE ~ Affiliation2))
##Aoife O’Rourke-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Aoife O’Rourke"  ~ 
"Botany Department, Trinity College Dublin, Dublin, Ireland",
TRUE ~ Affiliation1))
##Erik Öckinger------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Erik Öckinger"  ~ 
"Department of Ecology, Swedish University of Agricultural Sciences, Uppsala, Sweden",
TRUE ~ Affiliation1))
##Jens M. Olesen------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Jens M. Olesen"  ~ 
"Department of Biology, University of Aarhus, Aarhus, Denmark",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Jens M. Olesen"  ~ 
"Department of Ecoscience, University of Aarhus, Aarhus, Denmark",
TRUE ~ Affiliation2))
##Øystein H. Opedal------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Øystein H. Opedal"  ~ 
"Division of Biodiversity and Evolution, Department of Biology, Lund University, Lund, Sweden",
TRUE ~ Affiliation1))
##Theodora Petanidou------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Theodora Petanidou"  ~ 
"Laboratory of Biogeography & Ecology, Department of Geography, University of the Aegean, Mytilene, Greece",
TRUE ~ Affiliation1))
##Yves Piquot------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Yves Piquot"  ~ 
"Univ. Lille, CNRS, UMR 8198 - Evo-Eco-Paleo, F-59000 Lille, France",
TRUE ~ Affiliation1))
##Simon G. Potts------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Simon G. Potts"  ~ 
"Centre for Agri-Environmental Research, School of Agriculture, Policy and Development, University of Reading, Reading, UK",
TRUE ~ Affiliation1))
##Eileen Power-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Eileen Power"  ~ 
"Botany Department, Trinity College Dublin, Dublin, Ireland",
TRUE ~ Affiliation1))
##Willem Proesmans-----
affiliations = affiliations %>%  
mutate(Affiliation1 = case_when(Coauthor_name == "Willem Proesmans"  ~ 
"Laboratoire de Zoologie, Institut de recherche des Biosciences, Université de Mons, Mons, Belgium",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Willem Proesmans"  ~ 
"Agroécologie, INRAE, Institut Agro, Université de Bourgogne, Université de Bourgogne Franche-Comté, Dijon, France",
TRUE ~ Affiliation2))

##Demetra Rakosy-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Demetra Rakosy"  ~ 
"Department of Community Ecology, Helmholtz Centre for Environmental Research – UFZ, Halle, Germany",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Demetra Rakosy"  ~ 
"Department for Integrative Zoology, Faculty of Life Sciences, University Vienna, Vienna, Austria",
TRUE ~ Affiliation2))
##Sara Reverte----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Sara Reverte"  ~ 
"Laboratoire de Zoologie, Institut de recherche des Biosciences, Université de Mons, Mons, Belgium",
TRUE ~ Affiliation1))
##Stuart P. M. Roberts----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Stuart P. M. Roberts"  ~ 
"Centre for Agri-Environmental Research, School of Agriculture, Policy and Development, University of Reading, Reading, UK",
TRUE ~ Affiliation1))
##Maj Rundlöf------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Maj Rundlöf"  ~ 
"Department of Biology, Lund University, Lund, Sweden",
TRUE ~ Affiliation1))
##Laura Russo------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Laura Russo"  ~ 
"Department of Biology, Lund University, Lund, Sweden",
TRUE ~ Affiliation1))
##Bertrand Schatz-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Bertrand Schatz"  ~ 
"CEFE, CNRS, University of Montpellier, EPHE, IRD, Montpellier, France",
TRUE ~ Affiliation1))
##Jeroen Scheper-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Jeroen Scheper"  ~ 
"Nature Conservation and Plant Ecology Group, Wageningen University, Wageningen, The Netherlands",
TRUE ~ Affiliation1))
##Oliver Schweiger------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Oliver Schweiger"  ~ 
"Department of Community Ecology, Helmholtz Centre for Environmental Research – UFZ, Halle, Germany",
TRUE ~ Affiliation1))
##Pau Enric Serra-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Pau Enric Serra"  ~ 
"Mediterranean Institute for Advanced Studies (IMEDEA, UIB-CSIC), Esporles, Spain",
TRUE ~ Affiliation1))
##Catarina Siopa----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Catarina Siopa"  ~ 
"Department of Life Sciences, Centre for Functional Ecology, University of Coimbra, Coimbra, Portugal",
TRUE ~ Affiliation1))
##Henrik G. Smith-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Henrik G. Smith"  ~ 
"Department of Biology, Lund University, Lund, Sweden",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Henrik G. Smith"  ~ 
"Centre for Environmental and Climate Science, Lund University, Lund, Sweden",
TRUE ~ Affiliation2))
##Dara Stanley------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Dara Stanley"  ~ 
"Botany Department, Trinity College Dublin, Dublin, Ireland",
TRUE ~ Affiliation1))
##Valentin Ştefan------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Valentin Ştefan"  ~ 
"Department of Community Ecology, Helmholtz Centre for Environmental Research – UFZ, Halle, Germany",
TRUE ~ Affiliation1))%>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Valentin Ştefan"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation2))

##Valentin Ştefan------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Ingolf Steffan-Dewenter"  ~ 
"Department of Animal Ecology and Tropical Biology, Biocenter, University of Würzburg, Würzburg, Germany",
TRUE ~ Affiliation1))

##Jane C. Stout------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Jane C. Stout"  ~ 
"Botany Department, Trinity College Dublin, Dublin, Ireland",
TRUE ~ Affiliation1))
##Louis Sutter------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Louis Sutter"  ~ 
"Agroecology and Environment, Agroscope, Zürich, Switzerland",
TRUE ~ Affiliation1))
##Elena Motivans Švara------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Elena Motivans Švara"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Elena Motivans Švara"  ~ 
"Department of Community Ecology, Helmholtz Centre for Environmental Research – UFZ, Halle, Germany",
TRUE ~ Affiliation2)) %>%  
mutate(Affiliation3 = case_when(Coauthor_name == "Elena Motivans Švara"  ~ 
"Martin Luther University Halle-Wittenberg, Institute of Biology, Halle, Germany",
TRUE ~ Affiliation3))
##Sebastian Świerszcz------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Sebastian Świerszcz"  ~ 
"Center for Biological Diversity Conservation, Polish Academy of Sciences, Botanical Garden, Warsaw, Poland",
TRUE ~ Affiliation1))%>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Sebastian Świerszcz"  ~ 
"Polish Academy of Sciences, The Franciszek Górski Institute of Plant Physiology, Opole, Poland",
TRUE ~ Affiliation2))
##Amibeth Thompson------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Amibeth Thompson"  ~ 
"Martin Luther University Halle-Wittenberg, Institute of Biology, Halle, Germany",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Amibeth Thompson"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation2))
##Anna Traveset------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Anna Traveset"  ~ 
"Mediterranean Institute for Advanced Studies (IMEDEA, UIB-CSIC), Esporles, Spain",
TRUE ~ Affiliation1))
##Annette Trefflich------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Annette Trefflich"  ~ 
"State Institute of Agriculture and Horticulture Saxony-Anhalt, Bernburg, Germany",
TRUE ~ Affiliation1))
##Robert Tropek-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Robert Tropek"  ~ 
"Institute of Entomology, Biology Centre, Czech Academy of Sciences, České Budějovice, Czechia",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Robert Tropek"  ~ 
"Department of Ecology, Faculty of Science, Charles University, Prague, Czechia",
TRUE ~ Affiliation2))
##Adam J. Vanbergen-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Adam J. Vanbergen"  ~ 
"Agroécologie, INRAE, Institut Agro, Université de Bourgogne, Université de Bourgogne Franche-Comté, Dijon, France",
TRUE ~ Affiliation1))
##Montserrat Vilà-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Montserrat Vilà"  ~ 
"Estación Biológica de Doñana (EBD-CSIC), Seville, Spain",
TRUE ~ Affiliation1))
##Ante Vujić-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Ante Vujić"  ~ 
"Department of Biology and Ecology, Faculty of Sciences, University of Novi Sad, Novi Sad, Serbia",
TRUE ~ Affiliation1))
##Cian White-------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Cian White"  ~ 
"Botany Department, Trinity College Dublin, Dublin, Ireland",
TRUE ~ Affiliation1))
##Jennifer B. Wickens------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Jennifer B. Wickens"  ~ 
"Centre for Agri-Environmental Research, School of Agriculture, Policy and Development, University of Reading, Reading, UK",
TRUE ~ Affiliation1))
##Victoria B. Wickens------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Victoria B. Wickens"  ~ 
"Centre for Agri-Environmental Research, School of Agriculture, Policy and Development, University of Reading, Reading, UK",
TRUE ~ Affiliation1))
##Marie Winsa------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Marie Winsa"  ~ 
"Department of Ecology, Swedish University of Agricultural Sciences, Uppsala, Sweden",
TRUE ~ Affiliation1))
##Leana Zoller------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Leana Zoller"  ~ 
"Martin Luther University Halle-Wittenberg, Institute of Biology, Halle, Germany",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Leana Zoller"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation2))
##Ignasi Bartomeus------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Ignasi Bartomeus"  ~ 
"Estación Biológica de Doñana (EBD-CSIC), Seville, Spain",
TRUE ~ Affiliation1))


##Some missing ORCIDS -check-
####Prepare numbers for affiliations-----

#TRIAL
#Add a column with row number
affiliations = affiliations %>% 
mutate(ID = seq(1:nrow(.)))

#Create tibble per affiliation plus row ID
aff1 = affiliations %>% 
select(Affiliation1, ID) %>% 
rename(Aff = Affiliation1) %>% 
mutate(Group = 1)
aff2 = affiliations %>% 
select(Affiliation2, ID) %>% 
rename(Aff = Affiliation2) %>% 
mutate(Group = 2)
aff3 = affiliations %>% 
select(Affiliation3, ID) %>% 
rename(Aff = Affiliation3)%>% 
mutate(Group = 3)

all = bind_rows(aff1, aff2, aff3) %>% 
arrange(ID) 

all1 = all %>% 
distinct(Aff, .keep_all = TRUE) %>% 
filter(!is.na(Aff)) %>% 
mutate(Aff_numbers = seq(1, nrow(.)))

#Save number affiliation for MS
#First select column of interest and paste together
Affiliation_list = all1 %>% 
select(Aff, Aff_numbers) %>% 
mutate(Aff_number_Aff = paste0("^", Aff_numbers,"^ ", Aff)) %>% 
select(Aff_number_Aff) 
write_csv(Affiliation_list, "Data/Manuscript_info/Affiliation_list.csv")

#Filter group 1
numbers_affiliation1 = all1 %>% 
filter(Group == 1) %>% 
rename(Group1 = Group) %>% 
rename(Aff_number1 = Aff_numbers) %>% 
rename(Affiliation1 = Aff) %>% 
select(Affiliation1, Aff_number1)
#Filter group 2
numbers_affiliation2 = all1 %>% 
filter(Group == 2) %>% 
rename(Group2 = Group) %>% 
rename(Aff_number2 = Aff_numbers) %>% 
rename(Affiliation2 = Aff) %>% 
select(Affiliation2, Aff_number2)
#Filter group 3
numbers_affiliation3 = all1 %>% 
filter(Group == 3) %>% 
rename(Group3 = Group) %>% 
rename(Aff_number3 = Aff_numbers) %>% 
rename(Affiliation3 = Aff) %>% 
select(Affiliation3, Aff_number3)


#Add numbers of cols 2 and 3 to 1
group2to1 = numbers_affiliation2 %>% 
rename(Affiliation1 = Affiliation2) %>% 
rename(Aff_number1 = Aff_number2)
numbers_affiliation1 = bind_rows(numbers_affiliation1, group2to1)
group3to1 = numbers_affiliation3 %>% 
rename(Affiliation1 = Affiliation3) %>% 
rename(Aff_number1 = Aff_number3)
numbers_affiliation1 = bind_rows(numbers_affiliation1, group3to1)
#Add numbers of cols 1 and 3 to 2
group1to2 = numbers_affiliation1 %>% 
rename(Affiliation2 = Affiliation1) %>% 
rename(Aff_number2 = Aff_number1)
numbers_affiliation2 = bind_rows(numbers_affiliation2, group1to2)
group3to2 = numbers_affiliation3 %>% 
rename(Affiliation2 = Affiliation3) %>% 
rename(Aff_number2 = Aff_number3)
numbers_affiliation2 = bind_rows(numbers_affiliation2, group3to2) %>% 
distinct(Affiliation2, .keep_all = TRUE)
#Add numbers of cols 1 and 3 to 2 (JUST IN CASE, no need for now)
group1to3 = numbers_affiliation1 %>% 
rename(Affiliation3 = Affiliation1) %>% 
rename(Aff_number3 = Aff_number1)
numbers_affiliation3 = bind_rows(numbers_affiliation3, group1to3)
group2to3 = numbers_affiliation2 %>% 
rename(Affiliation3 = Affiliation2) %>% 
rename(Aff_number3 = Aff_number2)
numbers_affiliation3 = bind_rows(numbers_affiliation3, group2to3) %>% 
distinct(Affiliation3, .keep_all = TRUE)


affiliations1 = affiliations 

#Left join per dataset
d1 = left_join(affiliations1, numbers_affiliation1, by = join_by(Affiliation1))
d2 = left_join(d1, numbers_affiliation2, by = join_by(Affiliation2))
d3 = left_join(d2, numbers_affiliation3, by = join_by(Affiliation3))

#Export to google sheets
#Load data to google sheets
d3_to_google_sheets = d3 %>% 
select(Study_id, Coauthor_name, Orcid, E_mail, 
       Affiliation1, Affiliation2, Affiliation3)

library(googlesheets4) #To upload to google sheets
d3_to_google_sheets = d3_to_google_sheets %>% 
mutate(ID = as.numeric(str_extract(Study_id, ".+?(?=_)"))) %>% 
relocate(ID, .before = Study_id) %>% 
arrange(ID)

#gs4_create("EuPPollNet", sheets = d3_to_google_sheets)



#Almost there! We need to add the numbers of each group to the others...
#Select cols of interest
d4 = d3 %>% 
select(Coauthor_name, Orcid, E_mail, Affiliation1, Aff_number1,  Affiliation2, Aff_number2, Affiliation3, Aff_number3)
#Paste values separated by commas
d4 = d4 %>%
rowwise() %>%
mutate(Pasted_numbers = paste(na.omit(c(Aff_number1, Aff_number2, Aff_number3)), collapse = ",")) %>%
ungroup() %>% 
mutate(Coauthor_name_aff = paste0(Coauthor_name, " ^",Pasted_numbers, "^"))

#Save authors with superscript for MS
write_csv(d4, "Data/Manuscript_info/Authorship_ordered.csv")
