# data vis project
# Louise Knight
# 30/09/21

# $ git clone https://github.com/username/repo.git
# Username: your_username
# Password: your_token
# 
# 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

#####
# install packages
#####
#install.packages("plotly")
#install.packages("maptools") # will be retired in 2023- sp? https://cran.r-project.org/web/packages/sp/index.html
#install.packages("rgdal") #https://cran.r-project.org/web/packages/rgdal/index.html
#install.packages("rgeos") #https://medium.com/@snehalgawas/introduction-to-spatial-data-analysis-in-r-using-rgeos-ea69059c3b90
# install.packages("leaflet")
# install.packages("readxl")
# install.packages("tidyverse")
#install.packages("sf") # https://r-spatial.github.io/sf/index.html - this is the best one to use LC

# need ggmap ?
#library
#####
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

#library(hrbrthemes) #for the heat map??
#library("tidyverse") - do I need this? conflicts

# ## Get data from fingertips for LH
# lhins_ft<-fingertips_data(ProfileID =143, AreaTypeID = "All")
# 
# areatype <- lhins_ft %>%
#   distinct(AreaType)
# 
# lhindward <- lhins_ft%>%
#   filter(AreaType=="Ward") %>%
#   arrange(desc(Value))
# 
# lhind <- lhindward %>%
#   distinct(IndicatorID, IndicatorName)
# 
# # Indicators I am interested in
# # risk factors:
# # admissions: 93229 93232 93231
# # deaths: 93259 93257 (no MI)
# # compare: 93747
# 
# # CVD admissions - only one time period - could do ward or MSOA
# Indcvdadm<-c("93229", "93232" , "93231")
# 
# cvd<-lhins_ft %>%
#   filter(IndicatorID==Indcvdadm)
# 
# cvdareas<-cvd %>%
#   distinct(AreaType, IndicatorName)
# 
# cvdyears<-cvd %>%
#   distinct(Timeperiod, IndicatorName)
# 
# # just ward level data
# cvd <- cvd%>%
#   filter(AreaType=="Ward") %>%
#   arrange(desc(Value))
# 
# write.csv(cvd,"cvd.csv")
# write.csv(lhins_ft,"lhins_ft.csv")
# ####################################### - issue missing indicator data from FT!
# lhins_ft <- read.csv("lhins_ft.csv")
# 
# bhcft<-lhins_ft %>%  # issue 1
#   filter(AreaName=="Brunswick and Adelaide", IndicatorID=="93229" )  #
# 
# bhcftw<-lhindward %>%  # issue 1
#   filter(AreaName=="Brunswick and Adelaide", IndicatorID=="93229" )  #
########################################
cvd <- read.csv("cvd.csv")

bhcvd<-cvd %>%  # issue 1
  filter(AreaName=="Brunswick and Adelaide")  #21 wards in brighton and Hove but only 7 with data?

# #Get datalake lookup Ward to LA to regions - why are regions RGN09CD?
# ward_Lkup<-get_datalake("SELECT [WD20CD]
#                        ,[WD20NM]
#                        ,[LTLA21CD]
#                        ,[LTLA21NM]
#                        ,[UTLA21CD]
#                        ,[UTLA21NM]
#                        ,[RGN09CD]
#                        ,[RGN09NM]
#                        FROM [LookupsShared].[dbo].[vLKP_WD20]")%>%
# select(WD20CD, WD20NM, LTLA21CD, LTLA21NM)

#browser()

# wards within LA to county to region- https://geoportal.statistics.gov.uk/datasets/ons::ward-to-local-authority-district-to-county-to-region-to-country-december-2020-lookup-in-united-kingdom-v2/about
# could get from data lake, but want mentor to be able to run code. could grab from geoportal web page..
Regions <- read.csv("Ward_to_Local_Authority_District_to_County_to_Region_to_Country__December_2020__Lookup_in_United_Kingdom_V2.csv", stringsAsFactors=FALSE)

bhreg<-Regions %>% 
  filter(LAD20NM=="Brighton and Hove")  #21 wards

#join data files - left join keeps missing in both ds 
cvd1 <- cvd %>%
  left_join(Regions, by=c("AreaCode"="WD20CD"))

sum(is.na(cvd1$LAD20CD)) #
sum(is.na(cvd1$AreaName)) #

bhcvd1<-cvd1%>% 
  filter(LAD20NM=="Brighton and Hove")  #21 wards

# 14/10 keeping all regions in to test the mapping- LK
secvd1<-cvd1 %>% 
  filter(RGN20NM=="South East")

# Check number of LA
# cvd1<-cvd1 %>% 
#   filter(LAD20NM=="Brighton and Hove") 
# 
# bhltla<-cvd1 %>% 
#   distinct(AreaCode, WD20NM) # Issue only 7 wards for B and H?
###############
write.csv(cvd1,"cvd1.csv")
###############
cvd1 <- read.csv("cvd1.csv")
########################################################
# 1) Data preparation R
########################################################
cvd1<-cvd1 %>% 
  mutate(Indicator=IndicatorName)

# check dyplr this is base R
cvd1$Indicator[cvd1$Indicator=="Emergency hospital admissions for coronary heart disease, standardised admission ratio"]<-"Coronary Heart Disease"
cvd1$Indicator[cvd1$Indicator=="Emergency hospital admissions for Myocardial Infarction (heart attack), standardised admission ratio"]<-"Myocardial Infarction"
cvd1$Indicator[cvd1$Indicator=="Emergency hospital admissions for stroke, standardised admission ratio"]<-"Stroke"


# Create Ranking and Quintile Variables this splits by equal count of wards within area
# Ranking wards within LAs - need to add areas
cvd1 <- cvd1 %>%
  group_by(LAD20CD, IndicatorID) %>%
  mutate(
    Within_LAD_Rank=row_number(Value),
    Within_LAD_Reverse_Rank=row_number(desc(Within_LAD_Rank)),
    Within_LAD_Quintile=ntile(Value,5)
  )

# need to have interactive, so that as each indicator is added one by one and the score is additional? 
# Prescribed groups i.e. this is CVD admissions or could be user defined. (could add user defined weighting)

# generate a composite score and rank 
cvd1 <- cvd1 %>%
  group_by(LAD20CD) %>%
  mutate(
    Total_rank_LA_score=sum(Within_LAD_Quintile),
    Tota_rank_LA_quintile=ntile(Total_rank_LA_score,5)
  )

cvd2<-cvd1 %>%  # SE for testing the shiny
  select(RGN20NM, LAD20CD,LAD20NM, AreaCode, WD20NM,IndicatorID, IndicatorName, Indicator, Value, Within_LAD_Rank, Within_LAD_Reverse_Rank, Within_LAD_Quintile )
write.csv(cvd2,"cvd2.csv")
#########################################
# 2) test static charts and maps
#########################################
# https://r-charts.com/correlation/heat-map-ggplot2/
# https://www.datacamp.com/community/tutorials/functions-in-r-a-tutorial
cvd2 <- read.csv("cvd2.csv")
# bar chart
cvd3<-cvd2 %>% 
    filter(LAD20NM == "Brighton and Hove") 

p<-ggplot(cvd3, aes(x=WD20NM,y=Value))+
  geom_col()
p

# # function or loop for the plots?
cvd2<-cvd2 %>% 
  filter(LAD20NM == "Brighton and Hove") 

##individual for each area
area<-cvd2%>%
  filter(RGN20NM=="South East") %>% 
  select(`LAD20NM`)%>%
  distinct()%>%
  unlist() # check with JC?

indicator<-cvd2 %>% 
  select(IndicatorID) %>% 
  distinct() %>% 
  unlist()

#type(area)

for (i in area){ 
  for (j in indicator){
  p<-ggplot(cvd2%>%filter(LAD20NM==i, IndicatorID==j), 
            aes(x=WD20NM,y=Value))+
    geom_col()
  
  ggsave(filename = paste0(i,j,"test.png"),
         plot = p,
         path = "H:/2 Data Visualisation/5 R code/test graphs",
         width = 35,
         height = 20,
         units = "cm")
  }
}

warnings()
#apply - have a look at this..

# first quintiles bar- could have a stack bar with total score with different color for each indicator and indicator key
p<-ggplot(cvd3%>%filter(IndicatorID=="93229"),
          aes(x=WD20NM,y=Within_LAD_Quintile))+
  geom_col()
p

######## try a heat map chart###########################
# https://campus.datacamp.com/courses/intermediate-data-visualization-with-ggplot2/best-practices-4?ex=6
# https://www.r-graph-gallery.com/79-levelplot-with-ggplot2.html

p<-ggplot(cvd3, aes(WD20NM, IndicatorName, fill= Within_LAD_Quintile)) + 
  geom_tile()
dev.new() 
p

# how to put many plots on one panel- https://patchwork.data-imaginist.com/
# with the total score as a different color? 
# how bring one indicator in at a time? -(now or later to check LC?)

# add the interactive hover for value
cvd3 <- cvd3 %>%
  mutate(text = paste0("Value: ",round(Value,2), "\n", "What else CI? sig higher/lower than av"))

p<-ggplot(cvd3, aes(WD20NM, Indicator, fill= Within_LAD_Quintile,text=text)) + 
  geom_tile()
#scale_fill_gradient(low="white", high="blue") - check with LC?
#dev.new() - check with LC?

#rlang::last_error()
?scale_fill_gradient
?geom_tile
?plotly
?ggplot
?unlist 
?read_sf
##### try map using leaflet#### read _sf --> check if load then don't need to transform. LK
# downloaded shapefile from here: https://geoportal.statistics.gov.uk/datasets/ons::wards-may-2020-boundaries-uk-bgc/about
# is this generlised clipped- Boundaries?: This file contains the digital vector boundaries for Wards, in the United Kingdom, as at May 2020. The boundaries available are: (BGC) Generalised (20m) - clipped to the coastline (Mean High Water mark).
url <- "https://opendata.arcgis.com/datasets/62bfaabbe3e24a359fc36b34d7fe8ac8_0.geojson"

Wards20 <- read_sf(url) # Reads in all wards for UK
#, layer="Wards_(May_2020)_Boundaries_UK_BGC"
Wards20$wd20cd <- as.character(Wards20$wd20cd)

LAD20_url <- "https://opendata.arcgis.com/datasets/db23041df155451b9a703494854c18c4_0.geojson"

LAD20 <- read_sf(LAD20_url) # Reads in all LAs for UK
#,layer="Local Authority Districts (December 2020) UK BGC")
LAD20$lad20cd <- as.character(LAD20$lad20cd)

#The GeoJSON is read as a special type of dataframe called a Spatial Polygons Data Frame (SPDF).



##### Try leaflet### https://rstudio.github.io/leaflet/
#https://rpubs.com/mattdray/basic-leaflet-maps
# dhexchange examples: https://dhexchange.kahootz.com/AnalystLearningExchange/view?objectId=10583696 - R session 587 and 58 are maps.
glimpse(LAD20)

lads_eng <- subset(
  x = LAD20,  # our data
  subset = grepl(  # subset the data where the following pattern is matched
    x = LAD20@data$LAD20CD,  # in this variable in this slot of this SPDF
    pattern = "^E"  # subset anything starting with 'E'
  )
)

length(lads_eng@data$LAD20CD)  # check that the number of LADs is reduced- N=314

wd_eng <- subset(
  x = Wards20,  # our data
  subset = grepl(  # subset the data where the following pattern is matched
    x = Wards20@data$wd20cd,  # in this variable in this slot of this SPDF
    pattern = "^E"  # subset anything starting with 'E'
  )
)

length(wd_eng@data$wd20cd)  # check that the number of wds is reduced- N=7147

map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)

map  # show the map

map_lad <- map %>%
  addPolygons(
    data = lads_eng,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "black",  # line colour
    fillOpacity = ifelse(  # conditional fill opacity
      test = lads_eng@data$st_areashape > 1E+09,  # if area is over this value - will need to change to Quintile
      yes = 0.5,  # then make it half-opaque
      no = 0  # otherwise make it entirely transparent
    ),
    fillColor = "blue",
    label = ~LAD20NM  # LAD name as a hover label
  )

map_lad  # show the map
###################################
# wards- this is what we will need
###################################
#https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html
#https://rstudio.github.io/leaflet/shiny.html  # how to add map to Rshiny
###############################
small_example <- read.csv("small_example.csv")  # this is made in shiny code- just pulling in here to test map
###############################

# 1) Convert the shapefiles to a dataframe ---------- Check with LC? 
# This allows data to be attached more easily and
# is also required for gpplot to be able to plot the data
# WdShp <-Wards20
# Wds20Fort <- fortify(WdShp, region="wd20cd")
# ? fortify # going out of date use broom?

# do I need to merge or can you work across shapefile and df?
# Merge Data with fortified shapefile ---------- 
# merge not rbind as spatial data, in sp package. 
# for now just want to test the small example (3 indicators per ward). = so only matching.
?merge
Wards20ind <- merge(x=Wards20, 
                    y=small_example, # X= change to sf object once you have the sf - LK
                    by.x="wd20cd",
                    by.y="AreaCode")

# 2) set the color## 
# from JW packs
my.palette.function <- colorRampPalette(c("#fde8ee", "#98002E"))
some.colours <- my.palette.function(5)

# or use this code - from the example below? - Quantile?
# example https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html
mypalette <- colorNumeric( palette="viridis", domain=world_spdf@data$POP2005, na.color="transparent")
mypalette(c(45,43))

# m <- leaflet(world_spdf) %>% 
#   addTiles()  %>% 
#   setView( lat=10, lng=0 , zoom=2) %>%
#   addPolygons( fillColor = ~mypalette(POP2005), stroke=FALSE )
# 
# m
# 
#   m <- leaflet(world_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
#   addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", POP2005)(POP2005) )


?colorQuantile
?setView
?addPolygons

# 3) test on the map ##### need to select the indicator for the quantiles
map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)

map  # show the map
map_wd <- map %>%
  addTiles() %>% 
  #setView()or flyTo() # add flyin zoom here?
  addPolygons(
    data = wd_eng,  # ward polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "black",  # line colour
    fillColor=~mypalette(Within_LAD_Quintile), stroke=FALSE,  # can I add code for the quintile here? - LC? -
    label = ~wd20nm  #  name as a hover label
  )

map_wd  # show the map

# test this way
mypalette <- colorNumeric( palette="red", domain=Wards20@data$wd20mn, na.color="transparent")
mypalette(c(45,43))

m <- leaflet(Wards20) %>% 
  addTiles()  %>% 
#  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( fillColor = ~mypalette(Wards20@data$wd20mn), stroke=FALSE )

m

m <- leaflet(Wards20)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", POP2005)(POP2005) )







# 14/10/21 plan - updated 23/10/21
# sort out the shiny select and the chart working - sort out for full dataset rather than just small example
# test the map static - done using leaflet -the wards with quantiles next - now
# add the heat map chart to shiny - done- why is the value over not working
# add map to the shiny (check re zoom in) 
# use reactive to have the areaname as selected once and apply to chart and map
# use reactive again to have the indicator one by one (how?)on chart and map interactive
#?add in the composite value



