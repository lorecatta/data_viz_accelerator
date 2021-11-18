# Rshiny - These are LKs learning notes and code- not code for collaborative coding or sharing!

##try push again 8aff6714b243756915bd623ca9f87e03836c1d96

# Plan --------------------------------------------------------------------

# add the heat map chart to shiny - done- why is the value hover not working
# add indicator and area input to map to the shiny - done - check re zoom in 
# add indicator and area input and the chart working - today - done
# use reactive to have the areaname as selected once and apply to chart and map - today - done
# add in the boundary of the LA in bold and the wards inside - today?
# use reactive? to have the indicator added one by one (how?)on to heat map and geomap interactive
# add in the composite value
# full dataset rather than just small example

# Format chats and map (add titles, CI etc)
# add new groups of indicators, tab's across vs select on right


# Load packages -----------------------------------------------------------
#```{r}

if (!require(devtools)) install.packages("devtools")
if (!require(pacman)) install.packages("pacman") 

pacman::p_load(
  dplyr,
  ggplot2,
  shiny,
  sf,
  leaflet,
  tidyverse,
  maptools
)

#```
# Data preps --------------------------------------------------------------
small_example <- read.csv("small_example.csv")
# check dyplr this is base R -  use mutate?
small_example$IndicatorG[small_example$Indicator=="Coronary Heart Disease"]<-"CVD"
small_example$IndicatorG[small_example$Indicator=="Myocardial Infarction"]<-"CVD"
small_example$IndicatorG[small_example$Indicator=="Stroke"]<-"CVD"

url <- "https://opendata.arcgis.com/datasets/62bfaabbe3e24a359fc36b34d7fe8ac8_0.geojson"
Wards20 <- read_sf(url) # Reads in all wards for UK #, layer="Wards_(May_2020)_Boundaries_UK_BGC"
LAD20_url <- "https://opendata.arcgis.com/datasets/db23041df155451b9a703494854c18c4_0.geojson"
LAD20 <- read_sf(LAD20_url) # Reads in all LAs for UK #,layer="Local Authority Districts (December 2020) UK BGC")

Wards20$wd20cd <- as.character(Wards20$wd20cd)
LAD20$LAD20CD <- as.character(LAD20$LAD20CD)

# # ? need the wards and lad co-od in here- why only lad in the ward geo file. check about x in merge?  
# Wards20ind <- small_example %>% 
#  left_join(Wards20, by=c("AreaCode"="wd20cd")) %>% 
#  left_join(LAD20, by=c("LAD20CD"="LAD20CD"))

Wards20ind <- merge(x=Wards20,
                    y=small_example,
                    by.x="wd20cd",
                    by.y="AreaCode")

lad20_small_example <-LAD20 %>% 
  filter(
    LAD20NM=="Slough" | LAD20NM=="Southampton" | LAD20NM=="Woking"
    )

# add the interactive hover for value  and  indicator group 
small_example <- small_example %>%
  mutate(
    text = paste0("Value: ",round(Value,2), "\n", "CI? sig than av")
  ) 

lad_choices = unique(small_example$LAD20NM)
indG_choices = list(CVD="CVD")
ind_choices = unique(small_example$Indicator)

bins <- c(1,2,3,4,5)
col <- colorBin("YlOrRd", domain = Wards20ind$Within_LAD_Quintile, bins=bins)


# Application layout
# https://shiny.rstudio.com/articles/layout-guide.html

# MAP, CHART AND HEATMAP --------------------------------------------------


ui <- fluidPage(
  
  titlePanel("Data vis"),
  
  sidebarLayout(
    
    sidebarPanel(
      
     selectInput(inputId="select_area", 
                  label = h3("Select area"), 
                  choices = lad_choices), 
     selectInput(inputId="select_indG", 
                  label = h3("Select Indicator Group"), 
                  choices = indG_choices),
     selectInput(inputId = "select_ind", 
                  label = h3("Select indicator"),
                  choices = ind_choices)
      
    ),
    
    mainPanel(
      
      plotOutput("chart"),
      leafletOutput("mymap"),
      plotOutput("heatmap")
      
      
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
  
  indsG_areas <- reactive(
    {
      Wards20ind %>% 
        filter(IndicatorG == input$select_indG & LAD20NM == input$select_area)
    }
  )
  
  lad_areas <- reactive(
    {
      lad20_small_example %>% 
        filter(LAD20NM == input$select_area)
    }
  )
  output$chart<-renderPlot({
    title<-"example chart for data viz"
    ggplot(inds_areas(),
           aes(x=WD20NM,y=Value))+
      geom_col()
  })
  
  output$mymap <- renderLeaflet(
    {
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>% 
        addTiles() %>%
        # setView(lng = -1.988229, lat = 50.736129, zoom = 6) %>% 
        addPolygons(
          data = inds_areas(),
          fillColor = ~col(Within_LAD_Quintile),
          weight = 1,
          opacity = 1,
          color = "black",
          fillOpacity = 0.7,
          label = ~wd20nm) %>% 
        addPolygons(
          data = lad_areas(),
          weight = 4,
         # opacity = 1,
          color = "black",
          stroke = TRUE
        )
        
      
    }
  )
  
  
  output$heatmap<-renderPlot({  
    title<-"example heat map for data viz"
    ggplot(indsG_areas(), 
           aes(WD20NM, Indicator, fill= Within_LAD_Quintile,text=text)) +  
      geom_tile()
  })
  
}
shinyApp(ui = ui, server = server)

