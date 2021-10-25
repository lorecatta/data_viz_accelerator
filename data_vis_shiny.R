######################################################
###3) Rshiny 
######################################################
library(fingertipsR)
library(DataLakeR)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
# this is was JW used in the slide packs
library("maptools") 
library("rgdal")
library("rgeos")
# mentor recommends I use this:
library(sf)
# this is from the R ExChange leaflet example
library (leaflet)
library(readxl)
library(tidyverse)

## template here https://shiny.rstudio.com/tutorial/
cvd1 <- read.csv("cvd1.csv")
cvd2 <- read.csv("cvd2.csv")
cvd1 <- cvd1 %>% ungroup()
cvd2 <- cvd2 %>% ungroup()
############ Shiny preps
# list for LA
seltlan<-cvd1 %>% 
  distinct(LAD20NM) %>% 
  select(LAD20NM) 
#seltlan$ID <- seq.int(nrow(seltlan)) 
?selectInput

target = c("Slough", "Southampton", "Woking")

small_example <- cvd2 %>%
  filter(LAD20NM %in% target)

lad_choices = list(`Slough`="Slough", `Southampton`="Southampton", `Woking`="Woking")

area<-cvd2%>%
  filter(RGN20NM=="South East") %>% 
  select(`LAD20NM`)%>%  
  distinct()%>%
  unlist() 

# add the interactive hover for value  - Check WITH LC
small_example <- small_example %>%
  mutate(text = paste0("Value: ",round(Value,2), "\n", "What else CI? sig higher/lower than av"))

write.csv(small_example,"small_example.csv")
###############################
small_example <- read.csv("small_example.csv")
###############################
ui<-fluidPage(
  
  # Copy the line below to make a select box 
  selectInput(inputId="select", label = h3("Select box"), 
              choices = lad_choices),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  plotOutput("chart")
  
)

server <- function(input, output) {
  output$chart<-renderPlot({  # call it chart as in the plot output above
    title<-"example chart for data viz"
    ggplot(small_example%>%filter(LAD20NM==input$select), 
           aes(x=WD20NM,y=Value))+
      geom_col()
  })
}

shinyApp(ui = ui, server = server)

###############################
# not sure how the above is picking the indicator for the value- need to specific indicator
# now try with the heat map
###############################
ui<-fluidPage(
  
  # Copy the line below to make a select box 
  selectInput(inputId="select", label = h3("Select box"), 
              choices = lad_choices),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  plotOutput("chart")
  
)

server <- function(input, output) {
  output$chart<-renderPlot({  # call it chart as in the plot output above
    title<-"example heat map for data viz"
    ggplot(small_example%>%filter(LAD20NM==input$select), 
           aes(WD20NM, Indicator, fill= Within_LAD_Quintile,text=text)) +  # LC- how to get the hover to work in the Rshiny
      geom_tile()
  })
}

shinyApp(ui = ui, server = server)

###############################
# now try add map with render then with
###############################
##### try map using leaflet####
Wards20 <- readOGR(dsn="https://opendata.arcgis.com/datasets/62bfaabbe3e24a359fc36b34d7fe8ac8_0.geojson") # Reads in all wards for UK
Wards20$wd20cd <- as.character(Wards20$wd20cd)
LAD20 <- readOGR(dsn="https://opendata.arcgis.com/datasets/db23041df155451b9a703494854c18c4_0.geojson") # Reads in all LAs for UK
LAD20$lad20cd <- as.character(LAD20$lad20cd)

ui<-fluidPage(
  
  # Copy the line below to make a select box 
  selectInput(inputId="select", label = h3("Select box"), 
              choices = lad_choices),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  plotOutput("chart")
  
)

server <- function(input, output) {
  output$chart<-renderPlot({  # call it chart as in the plot output above
    title<-"example heat map for data viz"
    ggplot(small_example%>%filter(LAD20NM==input$select), 
           aes(WD20NM, Indicator, fill= Within_LAD_Quintile,text=text)) +  # LC- how to get the hover to work in the Rshiny
      geom_tile()
  })
}

shinyApp(ui = ui, server = server)
# examples:
# Play action? https://stackoverflow.com/questions/56708762/how-do-i-link-a-shiny-action-button-to-a-plotly-animation-in-r
# https://shiny.rstudio.com/gallery/widget-gallery.html
# maps and zoom: https://shiny.rstudio.com/gallery/superzip-example.html
# flyto or leaflet might be it: https://gis.stackexchange.com/questions/168687/fly-to-location-in-leaflet/168688
# click on plot point https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html