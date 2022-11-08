library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggridges)
library(giscoR)
library(sf)

#Read data 
data = read_csv("Data/Processing/Metadata.csv")
data_coord = st_read("Data/Processing/data_coord.shp") %>% 
rename(Study_id = Study_d)
authors = read_csv("Data/Processing/Authorship.csv")

# Get all countries and transform to the same CRS
cntries = st_read("Data/Processing/cntries.shp")


ui <- dashboardPage(
  dashboardHeader(title = "Safeguard networks"),
  dashboardSidebar(selectInput("v_study_id", label = "Study_id", choices = unique(data$Study_id))),
  dashboardBody(  
    tags$head(
      tags$style(
        "body{
    min-height: 611px;
    height: auto;
    max-width: 1200px;
    margin: auto;
        }"
      )
    ),
    
     h1("Dataset description"),
     tableOutput('tbl1'),
    fluidRow(column(10,  align="center",plotOutput("plot"))),
     h1("Authorship"),
    tableOutput('tbl2'),
     h1("Metadata"),
     tableOutput('tbl3')

  )
)


server <- function(input, output) { 
  

 output$plot <- renderPlot({
    ggplot() +
geom_sf(data = cntries, fill = "grey80", color = "black") +
xlim(c(2200000, 7150000)) +
ylim(c(1380000, 5500000)) +
theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
        size = 0.5), panel.background = element_rect(fill = "aliceblue"),
panel.border = element_rect(colour = "black", fill=NA, size=1)) +
geom_sf(data = data_coord %>% filter(Study_id==input$v_study_id), color="orange", size = 2.5, stroke = 0, shape = 16) +
theme(legend.position="none") + 
scale_colour_manual(name = "Study \n locations", values = colors)
  })
  
  
output$tbl1 <- renderTable(data %>%  filter(Study_id==input$v_study_id) %>% 
                          filter(Metadata_fields == "Dataset_description") %>% 
                          select(Metadata_info), colnames = FALSE,  width= "90%")                 
                                           

output$tbl2 <- renderTable(authors %>%  filter(Study_id==input$v_study_id) %>% 
                             select(!Study_id),  width= "90%")        
  
output$tbl3 <- renderTable(data %>%  filter(Study_id==input$v_study_id) %>% 
                          filter(!Metadata_fields == "Dataset_description"),  width= "90%")   

  
  }

shinyApp(ui, server)
