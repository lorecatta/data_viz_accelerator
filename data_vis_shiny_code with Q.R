######################################################
### Rshiny - These are LKs learning notes and code- not code for collaborative coding or sharing!
######################################################
library(fingertipsR)
library(dplyr)
library(ggplot2)
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


# add the interactive hover for value  
small_example <- small_example %>%
  mutate(text = paste0("Value: ",round(Value,2), "\n", "What else CI? sig higher/lower than av"))

write.csv(small_example,"small_example.csv")
###############################
small_example <- read.csv("small_example.csv")
###############################
################################ 
# 1) heat map - works
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
# now try add map with render 
###############################
# shiny preps
small_example <- read.csv("small_example.csv")
# bring in data for map
url <- "https://opendata.arcgis.com/datasets/62bfaabbe3e24a359fc36b34d7fe8ac8_0.geojson"
Wards20 <- read_sf(url) # Reads in all wards for UK #, layer="Wards_(May_2020)_Boundaries_UK_BGC"
Wards20$wd20cd <- as.character(Wards20$wd20cd)
LAD20_url <- "https://opendata.arcgis.com/datasets/db23041df155451b9a703494854c18c4_0.geojson"
LAD20 <- read_sf(LAD20_url) # Reads in all LAs for UK #,layer="Local Authority Districts (December 2020) UK BGC")
LAD20$LAD20CD <- as.character(LAD20$LAD20CD)

Wards20ind <- merge(x=Wards20, 
                    y=small_example, 
                    by.x="wd20cd",
                    by.y="AreaCode")

lad_choices = unique(small_example$LAD20NM)
ind_choices = unique(small_example$Indicator)

bins <- c(1,2,3,4,5)
col <- colorBin("YlOrRd", domain = Wards20ind$Within_LAD_Quintile, bins=bins)

########################## https://rstudio.github.io/leaflet/shiny.html
# 2) map - works
##########################
ui<-fluidPage(
  leafletOutput("mymap"),
  p(),
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
    addProviderTiles(providers$OpenStreetMap)%>% 
      addPolygons(
        data = Wards20ind,  
        fillColor = ~col(Within_LAD_Quintile),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~wd20nm 
      )
  })
  
}

shinyApp(ui = ui, server = server)

#######################################
# 3) Chart and area works
######################################
ui<-fluidPage(
  
  # Copy the line below to make a select box 
  selectInput(inputId="select_area", label = h3("Select box"), 
              choices = lad_choices),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  plotOutput("chart")
  
)

server <- function(input, output) {
  
  areas<- reactive({
    Wards20ind%>%filter(LAD20NM==input$select_area)
  })
  
  output$chart<-renderPlot({  # call it chart as in the plot output above
    title<-"example chart for data viz"
    ggplot(areas(),
           aes(x=WD20NM,y=Value))+
      geom_col()
  })
}

shinyApp(ui = ui, server = server)

############################
# 4) indicators selection on map - works
############################
ui<-fluidPage(
  
  selectInput(inputId="select_ind", label = h3("Select box"),
              choices = ind_choices),
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  leafletOutput("mymap"),
  p(),
)

server <- function(input, output, session) {
  
  inds <- reactive({
    Wards20ind%>%filter(Indicator==input$select_ind)
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%  # what goes here? data?
      addProviderTiles(providers$OpenStreetMap)%>% 
      addTiles() %>%
      addPolygons(
        data=inds(),
        fillColor = ~col(Within_LAD_Quintile),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~wd20nm 
      )
  })
  
  ## 
  observe({
    leafletProxy("mymap", data=inds())
  })
}

shinyApp(ui = ui, server = server)
#######################################
### 5) indicator and area section (two inputs) on a simple chart - not working
######################################
ui<-fluidPage(
  
  selectInput(inputId="select_area", label = h3("Select box"), 
              choices = lad_choices),

  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  
  selectInput(inputId="select_ind", label = h3("Select box"),
              choices = ind_choices),
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  
  plotOutput("chart")
  
)

server <- function(input, output) {
  
  inds <- reactive({
    Wards20ind%>%filter(Indicator==input$select_ind)
  })
  
  areas<- reactive({
    Wards20ind%>%filter(LAD20NM==input$select_area)
  })
  
  
  output$chart<-renderPlot({
    title<-"example chart for data viz"
    ggplot(areas(), inds(), ### how select both on a chart? - LC?
           aes(x=WD20NM,y=Value))+
      geom_col()
  })
}

shinyApp(ui = ui, server = server)

  
  
# -----------------------------------------------------------------------------
#
# 6) try select area a zoom
#
# -----------------------------------------------------------------------------


# Application layout
# https://shiny.rstudio.com/articles/layout-guide.html


ui <- fluidPage(
  
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "select_ind", 
                  label = h3("Select indicator"),
                  choices = ind_choices),
      selectInput(inputId="select_area", 
                  label = h3("Select area"), 
                  choices = lad_choices)
    
    ),
  
    mainPanel(
      
      leafletOutput("mymap")
      
    )
  
  )
  
)
  
server <- function(input, output, session) {
  
  inds_areas <- reactive(
    {
      Wards20ind %>% 
        filter(Indicator == input$select_ind & LAD20NM == input$select_area)
    }
  )
  
  output$mymap <- renderLeaflet(
    {
      leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>% 
      addTiles() %>%
      # setView(lng=-0.118092, lat=51.509865, zoom = 5) %>%
      addPolygons(
        data = inds_areas(),
        fillColor = ~col(Within_LAD_Quintile),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~wd20nm)
    }
  )
}

shinyApp(ui = ui, server = server)



###############
# 7) try indicator on map and on chat at same time - not working LC?
###############
  ui<-fluidPage(
    
    selectInput(inputId="select_ind", label = h3("Select box"),
                choices = ind_choices),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value"))),
    
    selectInput(inputId="select_area", label = h3("Select box"), 
                choices = lad_choices),
    
    hr(),
    fluidRow(column(3, verbatimTextOutput("value"))),
    
    plotOutput("chart"),
    leafletOutput("mymap"),
    p(),
  )
  
  server <- function(input, output, session) {
    
    inds <- reactive({
      Wards20ind%>%filter(Indicator==input$select_ind)
    })
    
    areas<- reactive({
      Wards20ind%>%filter(LAD20NM==input$select_area)
    })
    
    output$chart<-renderPlot({
      title<-"example chart for data viz"
      ggplot(LAD20NM==areas(), Indicator==inds(), #How do I get this to work?##????
            aes(x=WD20NM,y=Value))+
        geom_col()
    })
    
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap)%>%
      addTiles() %>%
      addPolygons(
        data=inds(),
        fillColor = ~col(Within_LAD_Quintile),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~wd20nm
      )
   })

  }
  
  
  shinyApp(ui = ui, server = server)

