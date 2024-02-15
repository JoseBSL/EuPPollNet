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
#Add rows
data = data %>% 
add_row(Study_id = NA_character_, 
        Coauthor_name = "Will Glenny", 
        Orcid = "0000-0002-2522-3775", 
        E_mail = "willglenny@gmail.com")
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
#Prepare author data for article
authors = data
#Order dataset by last word of author's name
ordered_data = authors[order(word(authors$Coauthor_name,-1)),]
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
write_csv(ordered_authors, "Data/Manuscript_info/Authorship_ordered.csv")


#Add affiliations following order of authors
#Jose B. Lanuza----
affiliations = ordered_authors %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Jose B. Lanuza"  ~ 
"Estación Biológica de Doñana (EBD-CSIC), Seville, Spain",
TRUE ~ NA_character_)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Jose B. Lanuza"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ NA_character_)) %>% 
mutate(Affiliation3 = case_when(Coauthor_name == "Jose B. Lanuza"  ~ 
"Martin Luther University Halle-Wittenberg, Institute of Biology, Halle (Saale), Germany",
TRUE ~ NA_character_))
#Tiffany Knight-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Tiffany M. Knight"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Tiffany M. Knight"  ~ 
"Martin Luther University Halle-Wittenberg, Institute of Biology, Halle (Saale), Germany",
TRUE ~ Affiliation2)) %>% 
mutate(Affiliation3 = case_when(Coauthor_name == "Tiffany M. Knight"  ~ 
"CE Department, Helmholtz Centre for Environmental Research – UFZ, Halle
(Saale), Germany",
TRUE ~ Affiliation3))
#Nerea Montes-Perez-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Nerea Montes-Perez"  ~ 
"Estación Biológica de Doñana (EBD-CSIC), Seville, Spain",
TRUE ~ Affiliation1))
#Paola Acuña-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Paola Acuña"  ~ 
"CFE, Centre for Functional Ecology and Department of Life Sciences, University of Coimbra, Portugal",
TRUE ~ Affiliation1))
#Matthias Albrecht-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Matthias Albrecht"  ~ 
"Agroscope, Zurich, Switzerland",
TRUE ~ Affiliation1))
#Maddi Artamendi----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Maddi Artamendi" ~ 
"Basque Centre for Climate Change-BC3,Leioa, Spain",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Maddi Artamendi" ~ 
"Universidad del Pais Vasco, EuskalHerriko Unibertsitatea (UPV-EHU),Leioa, Spain",
TRUE ~ Affiliation2))
#Isabelle Badenhausser----
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
#Joanne M. Bennett----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Joanne M. Bennett"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Joanne M. Bennett"  ~ 
"Martin Luther University Halle-Wittenberg, Institute of Biology, Halle (Saale), Germany",
TRUE ~ Affiliation2))
#Paolo Biella-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Paolo Biella"  ~ 
"Department of Zoology, Faculty of Science, University of South Bohemia, České Budějovice, Czech Republic",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Paolo Biella"  ~ 
"Institute of Entomology, Biology Centre of the Academy of Sciences of the Czech Republic, České Budějovice,
Czech Republic",
TRUE ~ Affiliation2)) %>% 
mutate(Affiliation3 = case_when(Coauthor_name == "Paolo Biella"  ~ 
"Department of Earth and Environment Sciences (sect. Landscape Ecology), University of Pavia, Italy",
TRUE ~ Affiliation3))
#Ricardo Bommarco----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Ricardo Bommarco"  ~ 
"Department of Ecology, Swedish University of Agricultural Sciences, Uppsala, Sweden",
TRUE ~ Affiliation1)) 
#Andree Cappellari-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Andree Cappellari"  ~ 
"Department of Agronomy, Food, Natural Resources, Animals and Environment (DAFNAE), University of Padua, Italy",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Andree Cappellari"  ~ 
"Department of Biology and Biotechnology “Charles Darwin”, Sapienza University of Rome, Italy",
TRUE ~ Affiliation2))
#Sílvia Castro-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Sílvia Castro"  ~ 
"CFE, Centre for Functional Ecology and Department of Life Sciences, University of Coimbra, Portugal",
TRUE ~ Affiliation1)) 
#Yann Clough-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Yann Clough"  ~ 
"Centre for Environmental and Climate Science, Lund University, Sweden",
TRUE ~ Affiliation1)) 
#Pau Colom-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Pau Colom"  ~ 
"Mediterranean Institute for Advanced Studies (IMEDEA, UIB-CSIC), Esporles, Spain",
TRUE ~ Affiliation1)) 
#Joana Costa-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Joana Costa"  ~ 
"CFE, Centre for Functional Ecology and Department of Life Sciences, University of Coimbra, Portugal",
TRUE ~ Affiliation1)) 
#Christophe Dominik----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Christophe Dominik"  ~ 
"CE Department, Helmholtz Centre for Environmental Research – UFZ, Halle
(Saale), Germany",
TRUE ~ Affiliation1)) 
#Yoko L. Dupont----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Yoko L. Dupont"  ~ 
"Department of Biological Sciences, University of Aarhus, Denmark",
TRUE ~ Affiliation1)) 
#Reinart Feldmann----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Reinart Feldmann"  ~ 
"Helmholtz Centre for Environmental Research – UFZ, Leipzig, Germany",
TRUE ~ Affiliation1)) 
#Victoria Ferrero----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Victoria Ferrero"  ~ 
"CFE, Centre for Functional Ecology and Department of Life Sciences, University of Coimbra, Portugal",
TRUE ~ Affiliation1))
#William Fiordaliso----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "William Fiordaliso"  ~ 
"Laboratoire de Zoologie, Université de Mons, Belgium",
TRUE ~ Affiliation1))
#Alessandro Fisogni----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Alessandro Fisogni"  ~ 
"Evo-Eco-Paleo, CNRS, Université de Lille, France",
TRUE ~ Affiliation1))
#Una Fitzpatrick----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Úna Fitzpatrick"  ~ 
"National Biodiversity Data Centre, County Waterford, Ireland",
TRUE ~ Affiliation1))
#Marta Galloni----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Marta Galloni"  ~ 
"Department of Evolutionary Experimental Biology (BES), University of Bologna, Italy",
TRUE ~ Affiliation1))
#Hugo Gaspar----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Hugo Gaspar"  ~ 
"CFE, Centre for Functional Ecology and Department of Life Sciences, University of Coimbra, Portugal",
TRUE ~ Affiliation1))
#Elena Gazzea-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Elena Gazzea"  ~ 
"Department of Agronomy, Food, Natural Resources, Animals and Environment (DAFNAE), University of Padua, Italy",
TRUE ~ Affiliation1))
#Will Glenny-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Will Glenny"  ~ 
"German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
TRUE ~ Affiliation1))
#Irina Goia-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Irina Goia"  ~ 
"Faculty of Biology and Geology, Babeș-Bolyai University, Cluj-Napoca, Romania",
TRUE ~ Affiliation1))
#Juan Pedro González-Varo-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Juan Pedro González-Varo"  ~ 
"Estación Biológica de Doñana (EBD-CSIC), Seville, Spain",
TRUE ~ Affiliation1)) %>% 
mutate(Affiliation2 = case_when(Coauthor_name == "Juan Pedro González-Varo"  ~ 
"Conservation Science Group, Department of Zoology, University of
Cambridge, UK",
TRUE ~ Affiliation2))
#Nina Hautekèete-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Nina Hautekèete"  ~ 
"Evo-Eco-Paleo, CNRS, Université de Lille, France",
TRUE ~ Affiliation1))
#Veronica Hederström-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Veronica Hederström"  ~ 
"Centre for Environmental and Climate Science, Lund University, Sweden",
TRUE ~ Affiliation1))
#Ruben Heleno----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Ruben Heleno"  ~ 
"CFE, Centre for Functional Ecology and Department of Life Sciences, University of Coimbra, Portugal",
TRUE ~ Affiliation1))
#Sandra Hervias-Parejo----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Sandra Hervias-Parejo"  ~ 
"Mediterranean Institute for Advanced Studies (IMEDEA, UIB-CSIC), Esporles, Spain",
TRUE ~ Affiliation1))
#Jonna Heuschele-----
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
#Bernhard Hoiss----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Bernhard Hoiss"  ~ 
"Department of Animal Ecology and Tropical Biology, Biocenter, University of Würzburg, Germany",
TRUE ~ Affiliation1))
#Andrea Holzschuh-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Andrea Holzschuh"  ~ 
"Department of Animal Ecology and Tropical Biology, Biocenter, University of Würzburg, Germany",
TRUE ~ Affiliation1))
#Sebastian Hopfenmüller----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Sebastian Hopfenmüller"  ~ 
"Department of Animal Ecology and Tropical Biology, Biocenter, University of Würzburg, Germany",
TRUE ~ Affiliation1))
#José M. Iriondo----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "José M. Iriondo"  ~ 
"Area of Biodiversity and Conservation, ESCET, Universidad Rey Juan Carlos, Madrid, Spain",
TRUE ~ Affiliation1))
#Birgit Jauker-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Birgit Jauker"  ~ 
"Department of Animal Ecology, Justus Liebig University Giessen, Germany",
TRUE ~ Affiliation1))
#Frank Jauker-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Frank Jauker"  ~ 
"Department of Animal Ecology, Justus Liebig University Giessen, Germany",
TRUE ~ Affiliation1))
#Jana Jersáková-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Jana Jersáková"  ~ 
"University of South Bohemia, Faculty of Science, Department of Ecosystems Biology, České Budějovice, Czech Republic",
TRUE ~ Affiliation1))
#Katharina Kallnik------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Katharina Kallnik"  ~ 
"Department of Animal Ecology and Tropical Biology, Biocenter, University of Würzburg, Germany",
TRUE ~ Affiliation1))
#Reet Karise------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Reet Karise"  ~ 
"Institute of Agricultural and Environmental Sciences, Estonian University of Life Sciences, Tartu, Estonia",
TRUE ~ Affiliation1))
#David Kleijn------
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "David Kleijn"  ~ 
"Nature Conservation and Plant Ecology Group, Wageningen University, The Netherlands",
TRUE ~ Affiliation1))
#Stefan Klotz-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Stefan Klotz"  ~ 
"CE Department, Helmholtz Centre for Environmental Research – UFZ, Halle
(Saale), Germany",
TRUE ~ Affiliation1))
#Theresia Krausl-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Theresia Krausl"  ~ 
"Centre for Environmental and Climate Science, Lund University, Sweden",
TRUE ~ Affiliation1))
#Elisabeth Kühn-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Elisabeth Kühn"  ~ 
"CE Department, Helmholtz Centre for Environmental Research – UFZ, Halle
(Saale), Germany",
TRUE ~ Affiliation1))
#Paula Dominguez Lapido-----
affiliations = affiliations %>% 
mutate(Affiliation1 = case_when(Coauthor_name == "Paula Dominguez Lapido"  ~ 
"Basque Centre for Climate Change-BC3,Leioa, Spain",
TRUE ~ Affiliation1))

s = affiliations %>% 
distinct(Affiliation1)

