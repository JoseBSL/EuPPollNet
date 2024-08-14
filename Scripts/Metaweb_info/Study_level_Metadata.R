#------------------------------------------------#
#Prepare metadata at STUDY LEVEL from Safenet
#------------------------------------------------#

#Workflow:
#1)Load libraries and data
#2)Prepare date information
#Info about starting date, finishind date,
#sampling period and days sampled
#3)Prepare taxonomic information
#Info from main orders (% and number of them)
#Info from other order (number of them)
#Total interactions

#--------------------------------------#
#1)Load libraries and data-----
#--------------------------------------#
#Load libraries
library(googlesheets4) #To upload to google sheets
library(lubridate) #To operate with dates
library(dplyr) #Handling data
library(tidyr) #Reshape data (wide format)
library(stringr)
library(readr)
#Load data 
data = readRDS("Data/3_Final_data/Interaction_data.rds")
data = data %>%
dplyr::mutate(Year = lubridate::year(Date), 
                Month = lubridate::month(Date), 
                Day = lubridate::day(Date))
#--------------------------------------#
#2)Prepare date information-----
#--------------------------------------#
#Select cols of interest
dates = data %>% 
select(Study_id, Sampling_method, Day, Month, Year) %>% 
mutate(Date = dmy(paste(Day, Month, Year, sep = "-")))
#Obtain date information
dates1 = dates %>% 
group_by(Study_id,Year,  Sampling_method) %>% 
summarise(Min_date = min(Date), 
          Max_date= max(Date), 
          Sampling_days = n_distinct(Date)) %>% 
mutate(Sampling_period = as.numeric(Max_date - Min_date)) %>% 
mutate(Study_number = as.integer(str_extract(Study_id, "[^_]+")), 
       .before=Study_id) %>% 
arrange(Study_number) %>% 
mutate(Sampling_period = case_when(!is.na(Sampling_period) ~ Sampling_period + 1,
TRUE ~ NA)) %>% 
mutate(Sampling_days = case_when(!is.na(Sampling_period) ~ Sampling_days,
TRUE ~ NA))

#--------------------------------------#
#3)Prepare taxonomic information-----
#--------------------------------------#

#Select cols of interest and prepare data for processing
taxo_info = data %>% 
select(Study_id, 
       Pollinator_accepted_name, Year, Pollinator_order) %>% 
group_by(Study_id,Year, Pollinator_order) %>% 
count(Pollinator_order) 

#Select any order that is not (Cole, Lepi,Hymeno, Dip)
main_orders = c("Hymenoptera", "Lepidoptera", "Diptera", "Coleoptera")
#Extract levels in a vector for sum non-main orders within pipes
non_main_orders = taxo_info %>% 
ungroup() %>% 
select(Pollinator_order) %>% 
distinct() %>% 
filter(!Pollinator_order %in% main_orders) %>% 
pull()
#Extract levels in a vector from all orders
all_orders = taxo_info %>% 
ungroup() %>% 
select(Pollinator_order) %>% 
distinct() %>% 
pull()

#Convert to wide format with cols of interest
#Steps:
#1st conert to wide
#2nd create a column with runing number to arrange studies
#3rd sum number of non-main orders
#4th sum number of main orders
#5th sum number of interactions across orders
#6th select cols of interest
#7th convert to percentage main order cols
taxo_info1 = taxo_info %>% 
pivot_wider(names_from = Pollinator_order,
            values_from = n) %>% #1st
mutate(Study_number = as.integer(str_extract(Study_id, "[^_]+")), 
       .before=Study_id) %>% #2nd
arrange(Study_number) %>% #2nd
mutate(Other_orders = length(non_main_orders) - #3rd
         sum(is.na(c_across(non_main_orders)), #3rd
         na.rm=TRUE)) %>% #3rd
mutate(Number_of_main_orders_recorded = (4 - #4th
         sum(is.na(Hymenoptera) + is.na(Diptera) + #4th
             is.na(Lepidoptera) + is.na(Coleoptera)))) %>% #4th
mutate(Total_interactions = sum(across(all_orders),  na.rm = TRUE)) %>% #5th
select(Study_number, Study_id, Hymenoptera, Diptera,#6th
       Lepidoptera, Coleoptera, Number_of_main_orders_recorded, #6th
       Other_orders, Total_interactions) %>%  #6th
mutate(across(main_orders, ~ formatC(.x / Total_interactions,digits = 2, format = "f"))) %>% #7th
rename(Hymenoptera_percent = Hymenoptera,
       Diptera_percent = Diptera,
       Lepidoptera_percent = Lepidoptera,
       Coleoptera_percent = Coleoptera)


#Metadata study
metadata = left_join(dates1, taxo_info1)

#Read metadata from authors and incorporate DOI column 
meta = read_csv("Data/Working_files/Metadata.csv")

doi = meta %>% 
filter(Metadata_fields == "Doi") %>% 
select(!Metadata_fields) %>% 
rename(DOI = Metadata_info) %>% 
mutate(DOI = str_replace(DOI, " and", ";"))

metadata = left_join(metadata, doi) %>% 
relocate(DOI, .before = Year)

#Add qualitative info of what orders where sampled
#Based on authors info
metadata = metadata %>% 
mutate(Taxa_recorded = NA_character_) %>%
mutate(Comment_taxa_recorded = NA_character_)

#Studies by recorded floral visitors
all_pollinators = c("1_Bartomeus", "2_Petanidou", 
        "8_Biella", "9_Heleno", 
        "10_Vanbergen", "11_Clough",
        "14_Dupont", "15_Magrach",
        "18_Bartomeus", "20_Hoiss",
        "26_Castro", "27_Castro",
        "48_Lara-Romero", "49_Hervias-Parejo",
        "50_Hervias-Parejo", "51_Petanidou",
        "52_Hadrava")

only_bees = c("3_Michez")

only_bumblebees = c("13_Karise", "22_Kallnik")

bees_and_syrphids = c("6_Marini", "7_Scheper",
                      "12_Ockinger", "16_Manincor",
                      "19_Jauker", "23_Holzschuh",
                      "24_Holzschuh", "25_Holzschuh",
                      "28_Sutter", "29_Magrach",
                      "30_Smith", "31_Roberts",
                      "38_Maurer", "39_Schweiger")

bees_syrphids_and_butterflies = c("32_ORourke", "33_Power",
                                  "34_Stanley", "35_Mullen",
                                  "36_Larkin", "37_White",
                                  "47_Benadi")

hymenoptera_diptera_lepidoptera = c("40_Knight", "41_Knight",
                                    "42_Knight", "43_Knight",
                                    "44_Knight", "45_Knight",
                                    "46_Knight", "4_Marini",
                                    "5_Marini")

metadata = metadata %>% 
mutate(Taxa_recorded = case_when(
  Study_id %in% all_pollinators ~ "Hymenoptera, Diptera, Lepidoptera and Coleoptera",
  Study_id %in% only_bees ~ "Bees",
  Study_id %in% only_bumblebees ~ "Bumbleees",
  Study_id %in% bees_and_syrphids ~ "Bees and syrphids",
  Study_id %in% bees_syrphids_and_butterflies ~ "Bees, syrphids and butterflies",
  Study_id %in% hymenoptera_diptera_lepidoptera~ "Hymenoptera, Diptera and Lepidoptera",
  Study_id == "17_Fisogni" ~ "Hymenoptera, Diptera and Coleoptera",
  Study_id == "21_Hopfenmuller" ~ "Hymenoptera",
  TRUE ~ Taxa_recorded)) %>% 
mutate(Comment_taxa_recorded = case_when(
  Study_id == "4_Marini" ~ "Apoidea, hoverflies, conopids, tachinid flies, butterflies",
  Study_id == "5_Marini" ~ "Apoidea, hoverflies, conopids, tachinid flies, butterflies",
  Study_id == "17_Fisogni" ~ "Hymenoptera, Coleoptera and Diptera (Bombyliidae and Syrphidae)",
TRUE ~ Comment_taxa_recorded))


#Read metadata from authors and incorporate Authors, mails and orcids
authors = read_csv("Data/Manuscript_info/Authorship.csv")

authors = authors %>% 
group_by(Study_id) %>% 
mutate(ID_number = row_number()) %>% 
mutate(Coauthor_name = paste0(ID_number, ") ", Coauthor_name)) %>% 
mutate(Orcid = paste0(ID_number, ") ", Orcid)) %>% 
mutate(E_mail = paste0(ID_number, ") ", E_mail)) %>% 
mutate(Orcid = str_replace(Orcid, "NA", "")) %>% 
mutate(E_mail = str_replace(E_mail, "NA", "")) %>% 
group_by(Study_id) %>%
summarise(
Coauthor_Names = paste(Coauthor_name, collapse = ", "),
Orcids = paste(Orcid, collapse = ", "),
Emails = paste(E_mail, collapse = ", ")
)

#Add authors to metadata
metadata = left_join(metadata, authors)

#Save metadata
saveRDS(metadata, "Data/3_Final_data/Metadata.rds")


#Load data to google sheets
#(ss <- gs4_create("Study_level", sheets = metadata))



