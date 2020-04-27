library(tidyverse)
library(sf)
library(readxl)
library(openxlsx)
library(tmap)
survey_2 <- read_excel("surveys/survey_2.xlsx")
survey_1 <- read_excel("surveys/survey_1.xlsx")
revised_all_survey <- read_excel("surveys/revised_all_survey.xlsx")
###Survey 1 Map
survey_1_as_sf<-survey_1 %>%
  mutate(x=as.numeric(X))%>%
  mutate(y=as.numeric(Y))%>%
  st_as_sf(coords=c("x","y"),crs=4326,remove=FALSE)
survey_1_as_sf$condition<-factor(survey_1_as_sf$condition, levels = c("Standard","Substandard","Dilapidated"))
tmap_mode("view")
tm_shape(survey_1_as_sf)+
  tm_dots(col="condition",palette=c(Standard='grey', Substandard='red', Dilapidated='red'))+
  tm_dots(size=.5)+
  tm_facets("landlord_classification")

###Survey 2 Map
survey_2_as_sf<-survey_2%>%
  mutate(x=as.numeric(X))%>%
  mutate(y=as.numeric(Y))%>%
  st_as_sf(coords=c("x","y"),crs=4326,remove=FALSE)
survey_2_as_sf$condition<-factor(survey_2_as_sf$condition, levels = c("well maintained","sound","minor repairs needed",
                                                                      "moderate rehabilitation needed","substantial rehabilitation needed","dilapidated"))
tmap_mode("view")
tm_shape(survey_2_as_sf)+
  tm_dots(col="condition",palette=c("well maintained"='grey', "sound"='grey', "minor repairs needed"='grey',
                                    "moderate rehabilitation needed"= 'red', "substantial rehabilitation needed"='red',"dilapidated"='red'))+
  tm_dots(size=.1)+
  tm_facets("landlord_classification")
