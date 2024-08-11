#DATASET NUMBER 53; Libran
#Dataset sent by  Libran

source("Scripts/Processing/Functions/Empty_templates.R") #Read empty templates to compare with

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)
library(lubridate)
library(anytime)

#Read empty templates to compare with
source("Scripts/Processing/Functions/Empty_templates.R") 
#Load function to unify structure of data
source("Scripts/Processing/Functions/Change_str.R")

#Prepare interaction data ----
data = read_csv("Data/1_Raw_Data/53_Libran/Libran.csv")

#Check against template
#compare_variables(check_interaction_data, data1)

data = data %>% 
rename(Site_id = Site,
       Plant_species = Plant,
       Pollinator_species = Visitor) %>% 
select(!c(Area, Area_2018, Connectivity_Index, Connectivity_Index_2018,
       Included, ID, Family, Genus, Gender))


#Dates are in tow formats!
#Day/month/year
#AND
#Month/day/Year

#filter 1rst oday month year
unique(data$Date)
day_month_year = data %>% 
filter(str_detect(Date, "May") | str_detect(Date, "June")) %>%
mutate(Old_date = Date) %>% 
distinct(Date, Old_date) %>% 
separate(Date, c("Day", "Month", "Year"), "/", extra = "merge") %>% 
mutate(Month = str_replace(Month, "May", "05")) %>% 
mutate(Month = str_replace(Month, "June", "06")) %>% 
mutate(Correct_date = paste0(Year, "-", Month, "-", Day)) %>% 
select(!c(Year, Month, Day))

#Now filter the opposite!
month_day_year = data %>% 
filter(!(str_detect(Date, "May") | str_detect(Date, "June"))) %>% 
mutate(Old_date = Date) %>% 
distinct(Date, Old_date) %>% 
separate(Date, c("Month", "Day", "Year"), "/", extra = "merge") %>% 
mutate(Year = if_else(Year == "17", "2017", Year)) %>% 
mutate(Year = if_else(Year == "18", "2018", Year)) %>% 
mutate(Correct_date = paste0(Year, "-", Month, "-", Day)) %>% 
select(!c(Year, Month, Day))
#Bind both!
clean_dates = bind_rows(day_month_year, month_day_year)
clean_dates1 = clean_dates %>% 
mutate(Correct_date1 = as.Date(Correct_date))


#Split date column into 3
data = data %>% 
separate(Date, c("Day", "Month", "Year"), "/", extra = "merge") %>% 
mutate(Year = if_else(Year == "17", "2017", Year)) %>% 
mutate(Year = if_else(Year == "18", "2018", Year))

#Checks
unique(data$Day)
unique(data$Month)
unique(data$Year)

#Bind columns back in the right order
data = data %>% 
mutate(Date = paste0(Year,"-", Month, "-", Day)) 

clean_date = data %>% 
distinct(Date) %>% 
mutate(Correct_date = anydate(Date))

unique(clean_date$Correct_date)




data1 = data1 %>% 
mutate(Date = str_replace(Date, "June", "06")) %>% 
mutate(Date = str_replace(Date, "May", "05")) %>% 

unique(data1$Correct_date)

#Generate date column with unique cases

#Generate date column with unique cases
clean_dates = data1 %>% 
select(Date) %>% 
distinct(Date) %>% 
mutate(Correct_date = anydate(Date))

#Bind clean dates to original dataset
data1 = left_join(data1, clean_dates)

unique(data1$Site_id)


v = data1 %>% 
distinct(Pollinator_species) %>% 
pull()

filter(columna1 == "charcater")



v = c("Bombus lapidarius", "Bombus terrestris")


data_bombus == v



data1 %>% 
group_by(Pollinator_species) %>% 
summarise(Visited_spp = n_distinct(Plant_species))
