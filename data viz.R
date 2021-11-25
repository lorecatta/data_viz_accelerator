# data vis project
# Louise Knight
# 30/09/21


# Load Packages -----------------------------------------------------------

if (!require(devtools)) install.packages("devtools")
if (!require(pacman)) install.packages("pacman") 

pacman::p_load(
  fingertipsR,
  DataLakeR,
  dplyr,
  ggplot2,
  tidyverse,
  plotly
)

detach("package:plotly", unload = TRUE)

# Get data from fingertips for LH -----------------------------------------

lhins_ft<-fingertips_data(ProfileID =143, AreaTypeID = "All")

areatype <- lhins_ft %>%
  distinct(AreaType)

lhindward <- lhins_ft%>%
  filter(AreaType=="Ward") %>%
  arrange(desc(Value))

lhind <- lhindward %>%
  distinct(IndicatorID, IndicatorName)


# Write data --------------------------------------------------------------
write.csv(lhins_ft,"lhins_ft_2.csv")
lhins_ft_2 <- read.csv("lhins_ft_2.csv")

# just ward level data
lhindward <- lhins_ft_2 %>%
  filter(AreaType=="Ward") %>%
  arrange(desc(Value))

# Subset here if want selection of inds in cvd ----------------------------
cvd <-lhindward 
#Remove missing data?
miss_data <- cvd %>%
  filter(is.na(Value))

cvdareas<-cvd %>%
  distinct(AreaType, IndicatorName)

cvdyears<-cvd %>%
  distinct(Timeperiod, IndicatorName)

cvdinds<-cvd %>%
  distinct(IndicatorID, IndicatorName)


write.csv(cvd,"cvd.csv")
# Read --------------------------------------------------------------------

cvd <- read.csv("cvd.csv")

# bhcvd<-cvd %>%  # issue 1
#   filter(AreaName=="Brunswick and Adelaide")  #21 wards in brighton and Hove but only 7 with data?

#Get datalake lookup Ward to LA to regions - why are regions RGN09CD? Do we want LA 20 or 21?
ward_Lkup<-get_datalake("SELECT [WD20CD]
                       ,[WD20NM]
                       ,[LTLA21CD]
                       ,[LTLA21NM]
                       ,[UTLA21CD]
                       ,[UTLA21NM]
                       ,[LTLA20CD]
                       ,[LTLA20NM]
                       ,[UTLA20CD]
                       ,[UTLA20NM]
                       ,[RGN09CD]
                       ,[RGN09NM]
                       FROM [LookupsShared].[dbo].[vLKP_WD20]") #%>%

#join data files - left join keeps missing in both ds 
cvd1 <- cvd %>%
  left_join(ward_Lkup, by=c("AreaCode"="WD20CD"))

sum(is.na(cvd1$UTLA20CD)) #
sum(is.na(cvd1$AreaName)) #

bhcvd1<-cvd1%>% 
 distinct( AreaName, .keep_all = TRUE) %>% #21 wards 19?
  filter(UTLA20NM=="Brighton and Hove") 

##LK check if the map boundary is utLA or lt? 
# 14/10 keeping all regions in to test the mapping- LK
secvd1<-cvd1 %>% 
  filter(RGN09NM=="South East")

# Write and read cvd1 --------------------------------------------------------------
write.csv(cvd1,"cvd1.csv")
# Data preparation R ------------------------------------------------------
cvd1 <- read.csv("cvd1.csv")

# Indicators I am interested in for first example of CVD
Ind_cvd_rf<-c("93105", "93106" , "93489" , "93488" , "93107", "93108")
Ind_cvd_adm<-c("93229", "93232" , "93231")
Ind_cvd_dea<-c("93259", "93257" )

Ind_sef_IoD<-c("93747", "93268" )
Ind_sef_pop<-c("93746", "93745", "93082", "93744", "93084", "93226")

cvd1<-cvd1 %>% 
  mutate(
    Int_group=NA
  )
cvd1$Int_group[cvd1$IndicatorID=="93105"| cvd1$IndicatorID=="93106" | cvd1$IndicatorID=="93489"| cvd1$IndicatorID== "93488" | cvd1$IndicatorID== "93107" | cvd1$IndicatorID=="93108"]<-1 #CVD Risk Factors"
cvd1$Int_group[cvd1$IndicatorID=="93229"| cvd1$IndicatorID=="93232" | cvd1$IndicatorID=="93231"]<-2 #"CVD Hospital Admissions"
cvd1$Int_group[cvd1$IndicatorID=="93259"| cvd1$IndicatorID=="93257" ]<-3 #"CVD Deaths"

# Rank and within area quintiles ------------------------------------------

## re code this as loops!
cvd1 <- cvd1 %>%
  group_by(LTLA20CD, IndicatorID) %>%
  mutate(
    Within_LTLA_Rank=row_number(Value),
    Within_LTLA_Reverse_Rank=row_number(desc(Within_LTLA_Rank)),
    Within_LTLA_Quintile=ntile(Value,5),
  )

cvd1 <- cvd1 %>%
  group_by(AreaName, Int_group) %>%
  mutate(
    Total_wardIG_score=sum(Within_LTLA_Quintile, na.rm = T)
  )

cvd1 <- cvd1 %>%
  group_by(LTLA20CD, Int_group) %>%
  mutate(
Total_LTLAIG_quintile=ntile(Total_wardIG_score,5)
)


# the records for the composite in for LTLA
cvdLTLAT <- cvd1 %>%
  distinct (AreaName, Int_group,.keep_all=T ) %>% 
  select(LTLA20CD, LTLA20NM, AreaName, WD20NM, AreaCode, Int_group, Total_wardIG_score, Total_LTLAIG_quintile) %>% 
  mutate(IndicatorName="Composite Indicator group quintile", Within_LTLA_Quintile=Total_LTLAIG_quintile) # named the same col 

cvd1<-cvd1 %>% 
  bind_rows(cvd1,cvdLTLAT)

#
cvd1 <- cvd1 %>%
  group_by(UTLA20CD, IndicatorID) %>%
  mutate(
    Within_UTLA_Rank=row_number(Value),
    Within_UTLA_Reverse_Rank=row_number(desc(Within_UTLA_Rank)),
    Within_UTLA_Quintile=ntile(Value,5),
  )

#
cvd1 <- cvd1 %>%
  group_by(RGN09CD, IndicatorID) %>%
  mutate(
    Within_RGN_Rank=row_number(Value),
    Within_RGN_Reverse_Rank=row_number(desc(Within_RGN_Rank)),
    Within_RGN_Quintile=ntile(Value,5),
  )


# Read and Write cvd2 --------------------------------------------------------------
write.csv(cvd1,"cvd2a.csv")
cvd2a <- read.csv("cvd2a.csv")

cvd2<-cvd2a %>%  # SE for testing the shiny
  select(RGN09NM, UTLA20CD,UTLA20NM, LTLA20CD,LTLA20NM, AreaCode, WD20NM,IndicatorID, IndicatorName, Int_group, 
         Value,Timeperiod, LowerCI95.0limit, UpperCI95.0limit,
         Within_LTLA_Rank, Within_LTLA_Reverse_Rank, Within_LTLA_Quintile, 
         Total_wardIG_score, Total_LTLAIG_quintile,
         Within_UTLA_Rank, Within_UTLA_Reverse_Rank, Within_UTLA_Quintile, 
         Within_RGN_Rank, Within_RGN_Reverse_Rank, Within_RGN_Quintile 
         )


write.csv(cvd2,"cvd2.csv")


