# Rshiny - These are LKs learning notes and code- not code for collaborative coding or sharing!

##try push again 8aff6714b243756915bd623ca9f87e03836c1d96

# Plan --------------------------------------------------------------------

# see #1 get the sub-group indicator list to work
# see #2 add in the boundary of all the LA in bold nationally - so that user can see neighbors (maybe add select multi)
# add in the composite value
# heat map same colors as the map


# Format chats and map (add titles, CI etc)
# Sizeing issues for the names of wards and text size
# add new groups of indicators, tab's across vs select on right
# change layout with heatmap on top and group select on top of right pane, add headings
# Design with users

# use reactive and PLAY to have the indicator added one by one (how?)on to heat map and geomap interactive
# full dataset rather than just small example
# add geo levels - can I loop somehow using same var name for each level


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
#small_example <- read.csv("small_example.csv")
small_example <- read.csv("cvd2.csv")

#call UTLA20CD and NM -- to work out how to do for different levels of geography later -LK
small_example<-small_example %>%
  rename("LAD20CD"=LTLA20CD,"LAD20NM"=LTLA20NM, "Within_LAD_Quintile"=Within_LTLA_Quintile )

# check dyplr this is base R -  this code can go in data_visR

small_example$IndicatorG[small_example$Int_group==1 | small_example$Int_group==2 | small_example$Int_group==3]<-"cardiovascular disease (CVD)"

small_example$IndicatorSG[small_example$Int_group==1]<-"CVD Risk factors"
small_example$IndicatorSG[small_example$Int_group==2]<-"CVD Hospital admissions"
small_example$IndicatorSG[small_example$Int_group==3]<-"CVD Deaths"

# check dyplr this is base R - Indicator short name
small_example<-small_example %>% 
  mutate(Indicator=IndicatorName)

small_example$Indicator[small_example$Indicator=="Emergency hospital admissions for coronary heart disease, standardised admission ratio"]<-"Coronary Heart Disease"
small_example$Indicator[small_example$Indicator=="Emergency hospital admissions for Myocardial Infarction (heart attack), standardised admission ratio"]<-"Myocardial Infarction"
small_example$Indicator[small_example$Indicator=="Emergency hospital admissions for stroke, standardised admission ratio"]<-"Stroke"

url <- "https://opendata.arcgis.com/datasets/62bfaabbe3e24a359fc36b34d7fe8ac8_0.geojson"
Wards20 <- read_sf(url) # Reads in all wards for UK #, layer="Wards_(May_2020)_Boundaries_UK_BGC"
LAD20_url <- "https://opendata.arcgis.com/datasets/db23041df155451b9a703494854c18c4_0.geojson"
LAD20 <- read_sf(LAD20_url) # Reads in all LAs for UK #,layer="Local Authority Districts (December 2020) UK BGC")

Wards20$wd20cd <- as.character(Wards20$wd20cd)
LAD20$LAD20CD <- as.character(LAD20$LAD20CD)


Wards20ind <- merge(x=Wards20,
                    y=small_example,
                    by.x="wd20cd",
                    by.y="AreaCode")


# add the interactive hover for value for the heatmap - not working in shiny change way to do this
small_example <- small_example %>%
  mutate(
    text = paste0("Value: ",round(Value,2), "\n", "CI? sig than av")
  ) 

lad_choices = unique(small_example$LAD20NM)
indG_choices = unique(small_example$IndicatorG)
indSG_choices = unique(small_example$IndicatorSG)
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
     selectInput(inputId="select_indSG", 
                 label = h3("Select Indicator Sub-Group"), 
                 choices = indSG_choices),
     selectInput(inputId = "select_ind", 
                  label = h3("Select Indicator"),
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
  
  indsSG_areas <- reactive(
    {
      Wards20ind %>% 
        filter(IndicatorSG == input$select_indSG & LAD20NM == input$select_area)
    }
  )
  
  lad_areas <- reactive(
    {
      LAD20 %>% 
        filter(LAD20NM == input$select_area)
    }
  )
  output$chart<-renderPlot({
    title<-"example chart for data viz"
    ggplot(inds_areas(),
           aes(x=reorder(WD20NM,-Value),
               y=Value,
               fill = Within_LAD_Quintile))+
      labs(title=paste0(unique(small_example$IndicatorName)), # need to make this interactive
          # subtile =paste0(unique(small_example&Timeperiod)),
           y="paste the type from data",
           x="Area Name")+
      theme(axis.text.x = 
              element_text(siz=10,
                           angle=45,
                           hjust=1,
                           vjust=1))+
    geom_col()+
      geom_errorbar(aes(ymin=LowerCI95.0limit, ymax=UpperCI95.0limit), # check why not running well ? LC?
                    width=0.2,
                    position=position_dodge((0.9))
                    )
      
  })
  
  output$mymap <- renderLeaflet(
    {
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>% 
        addTiles() %>%
        addPolygons(
          data = inds_areas(),
          fillColor = ~col(Within_LAD_Quintile),
          weight = 1,
          opacity = 1,
          color = "black",
          fillOpacity = 0.7,
          label = ~wd20nm) #%>%  # this has stopped working- up date with a better lable method
 
    }
  )
  
  
  output$heatmap<-renderPlot({  
    title<-"example heat map for data viz"
    ggplot(indsSG_areas(), 
           aes(WD20NM, Indicator, 
               fill= Within_LAD_Quintile)) +  #,text=text
     labs(title=paste0(unique(small_example$IndicatorName)), # need to make this interactive indsSG_areas
          # labs(title=indsSG_areas(), # need to make this interactive indsSG_areas
           y="Indicator",
           x="Area Name")+
      theme(axis.text.x = 
              element_text(siz=10,
                           angle=45,
                           hjust=1,
                           vjust=1))+
      
      geom_tile()+
      coord_fixed()
  })
  
}
shinyApp(ui = ui, server = server)

