#Upload files to google drive
#install packages
#remotes::install_github("claudiozandonella/trackdown",
#                        build_vignettes = TRUE, force=T)
#
#install.packages("googledrive")
#load libraries
library(googledrive)
library(trackdown)
#set google doc
drive_auth()

#upload file

#update_file
upload_file(
  file = "Manuscript/EuPPollNet_Ms.qmd", 
  gfile = "EuPPollNet_Ms"
)


library(quarto)
quarto_render("EuPPollNet_Ms.qmd", output_format = "docx")

##download file 
#download_file(
#  file = "Manuscript/Lanuza_et_al_2022.Rmd", 
#  gfile = "Reproductive_traits"
#)


