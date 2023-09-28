#DATASET NUMBER 40; Schweiger

#Data sent by Oliver Schweiger. This dataset belongs to the VOODOO project
#https://voodoo-project.eu/about

#Read empty templates to compare with
source("Scripts/Empty_templates.R") 

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)

#Prepare interaction data ----
data <- read_csv("Data/Raw_data/40_Schweiger/Interaction_data.csv")

