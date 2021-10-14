# data vis project
# Louise Knight
# 30/09/21


#install.packages("plotly")
library(fingertipsR)
library(DataLakeR)
library(dplyr)
library(ggplot2)
#library(hrbrthemes) #for the heat map
library(plotly)
library(shiny)

## will set for mentor to run
setwd("H:/2 Data Visualisation/5 R code")

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

cvd1<-cvd1 %>% 
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
# 1) Data cleaning / Setting data types - toogle between

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

# try a heat map chart 
# https://campus.datacamp.com/courses/intermediate-data-visualization-with-ggplot2/best-practices-4?ex=6
# https://www.r-graph-gallery.com/79-levelplot-with-ggplot2.html

p<-ggplot(cvd3, aes(WD20NM, IndicatorName, fill= Within_LAD_Quintile)) + 
  geom_tile()
dev.new() 
p

# how to put many plots on one panel, so I can quickly view them? -  check with LC?
# with the total score as a different color? how bring one indicator in at a time? -(now or later to check LC?)

# add the interactive hover for value
cvd3 <- cvd3 %>%
  mutate(text = paste0("Value: ",round(Value,2), "\n", "What else CI?"))

p<-ggplot(cvd3, aes(WD20NM, Indicator, fill= Within_LAD_Quintile,text=text)) + 
  geom_tile()+
#scale_fill_gradient(low="white", high="blue") - check with LC?
#dev.new() - check with LC?
ggplotly(p,tooltip="text")
 

?scale_fill_gradient
?geom_tile
?plotly
rlang::last_error()
?ggplot
?unlist 

# try map static

# 14/10/21 plan
# test the map static
# add the heat map chart to shiny
# add map to the shiny (check re zoom in)
# use  reactive to have the areaname as selected once and apply to chart and map
# use reactive again to have the indicator on chart and map interactive

STOP

########## Rshiny template here https://shiny.rstudio.com/tutorial/
cvd2 <- read.csv("cvd2.csv")

################## Shiny preps
# list for LA
seltlan<-cvd1 %>% 
  distinct(LAD20NM) %>% 
  select(LAD20NM)  

cvd1 <- cvd1 %>% ungroup()

target = c("Slough", "Southampton", "Woking")

small_example <- cvd1 %>%
  filter(LAD20NM %in% target)

lad_choices = list(`Slough`="Slough", `Southampton`="Southampton", `Woking`="Woking")

area<-cvd2%>%
  filter(RGN20NM=="South East") %>% 
  select(`LAD20NM`)%>%
  distinct()%>%
  unlist() # check with JC?

?selectInput

seltlan$ID <- seq.int(nrow(seltlan)) 

###############################
ui<-fluidPage(
  
  # Copy the line below to make a select box - LK: how do look-up list 
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



# examples:
# Play action? https://stackoverflow.com/questions/56708762/how-do-i-link-a-shiny-action-button-to-a-plotly-animation-in-r
# https://shiny.rstudio.com/gallery/widget-gallery.html
# maps and zoom: https://shiny.rstudio.com/gallery/superzip-example.html
# flyto or leaflet might be it: https://gis.stackexchange.com/questions/168687/fly-to-location-in-leaflet/168688
# click on plot point https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html