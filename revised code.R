library(tidyverse)
library(readxl)
library(openxlsx)
library(tidyr)
library(sf)
library(viridis)
library(tmap)
#Load variable lists
s2_varlist<-read_excel("data/s1_2_crosswalk.xlsx",sheet="s2_vars")
s1_varlist<-read_excel("data/s1_2_crosswalk.xlsx",sheet="s1_vars")
record_times<-read_csv("data/record_times_all.csv") %>%
  group_by(parcel_num,primary_city) %>%
  summarise(record_id=first(record_id),
            date=first(date))
###Gainesville
gainesville_issues_info1<-read_excel("data/Gainesville issues and info.xlsx")%>%
  select(primary_city,parcel_num,address,homeexempt,ownname,ownaddress,owncity,condition,ownstate)
gainesville_issues_info1$condition <- replace(as.character(gainesville_issues_info1$condition), gainesville_issues_info1$condition == "Fair","Substandard")
gainesville_issues_info1$condition  <- replace(as.character(gainesville_issues_info1$condition), gainesville_issues_info1$condition== "Good","Standard")
gainesville_issues_info1$condition  <- replace(as.character(gainesville_issues_info1$condition), gainesville_issues_info1$condition == "Poor","Dilapidated")
gainesville_latlong<-read_csv("data/gville_points.csv") %>%
  select(PARCEL_NUM,long,lat) %>%
  rename(parcel_num=PARCEL_NUM)
gainesville_issues_info<-gainesville_issues_info1%>%
  mutate("primary_city"="gainesville") %>%
  left_join(gainesville_latlong)%>%
  rename(X=long,Y=lat)
gainesville_property<-read_excel("data/gainesville property.xlsx")%>%
  select(REALKEY,YR_BUILT,NO_BEDRMS,FULLBATHS,HEATEDAREA)%>%
  mutate(YR_BUILT=2020-YR_BUILT)%>%
  rename(age_building=YR_BUILT)
gainesville_value<-read_excel("data/gainesville value.xlsx")%>%
  select(REALKEY,PARCEL_NO,CURR_VAL)%>%
  rename(parcel_num=PARCEL_NO)
gainesville_landlord<-gainesville_issues_info%>%
  left_join(gainesville_value)%>%
  left_join(gainesville_property)%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0))
gainesville_citymatch<-gainesville_landlord%>%
  left_join(gainesville_issues_info)%>% left_join(gainesville_value)%>%
  left_join(gainesville_property)%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) 
gainesville_complete<-gainesville_landlord%>%
  left_join(gainesville_issues_info)%>% left_join(gainesville_value)%>%
  left_join(gainesville_property)%>%
  left_join(gainesville_citymatch)%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"outtown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"intown_landlord"))
###Millen
millen_info_with_parcel_number <- read_excel("data/millen info with parcel number.xlsx")%>%
  rename(parcel_num=Parcel_No,realkey=Realkey)%>%
  select(parcel_num,realkey,Ownkey)
millen_issues <- read_csv("data/millen_surveydata_rev_2017_06_27.csv")%>%
  rename(parcel_num=Parcel_ID,condition=Classify,X=long,Y=lat)%>%
  select(parcel_num,condition,X,Y,address)
millen_owner <- read_excel("data/millen_owner.xlsx")%>%
  select(Ownkey,ownname,ownaddress,owncity,ownstate)
millen_home_exempt <- read_csv("data/millen home exempt.csv")%>%
  select("PARCEL_NO","HOMEEXEMPT")%>%
  rename(parcel_num=PARCEL_NO,homeexempt=HOMEEXEMPT)
millen_issues_info<-millen_issues%>%
  left_join(millen_info_with_parcel_number)%>%
  left_join(millen_home_exempt)%>%
  left_join(millen_owner)
millen_property<-read_excel("data/millen property.xlsx")%>%
  select(realkey,yr_built,no_bedrms,fullbaths,heatedarea)%>%
  mutate(yr_built=2020-yr_built)%>%
  rename(age_building=yr_built)
millen_value<-read_excel("data/millen value.xlsx")%>%
  select(realkey,parcel_no,curr_val)%>%
  rename(parcel_num=parcel_no)
millen_landlords<-millen_issues_info%>%
  left_join(millen_property)%>%
  left_join(millen_value)%>%
  mutate("primary_city"="millen")%>%
  rename(CURR_VAL=curr_val,NO_BEDRMS=no_bedrms,HEATEDAREA=heatedarea,FULLBATHS=fullbaths,REALKEY=realkey)%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0)) 
millen_citymatch<-millen_landlords%>%
  left_join(millen_issues_info)%>%
  left_join(millen_property)%>%
  left_join(millen_value)%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) 
millen_complete<-millen_landlords%>%
  left_join(millen_issues_info)%>% left_join(millen_property)%>%
  left_join(millen_value)%>%
  left_join(millen_citymatch)%>%
  mutate("primary_city"="millen")%>%
  rename(CURR_VAL=curr_val,NO_BEDRMS=no_bedrms,HEATEDAREA=heatedarea,FULLBATHS=fullbaths,REALKEY=realkey)%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"outtown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"intown_landlord"))

  
#Monroe
monroe_info <- read_excel("data/monroe info.xlsx")%>%
  select(Parcel_No,address,Realkey,homeexempt,ownname,ownaddress,owncity,ownstate,curr_val)%>%
  rename(parcel_num=Parcel_No,REALKEY=Realkey,CURR_VAL=curr_val)
monroe_issues <- read_csv("data/Monroedata_2017_08_25_combined.csv") %>%
  rename(parcel_num=Parcel_ID,condition=Classify,X=long,Y=lat)%>%
  select(X,Y,parcel_num,condition,parcel_num)
monroe_issues_info<-monroe_issues%>%
  left_join(monroe_info)
monroe_property<- read_excel("data/monroe property.xlsx")%>%
  select(REALKEY,YR_BUILT,NO_BEDRMS,FULLBATHS,HEATEDAREA)%>%
  mutate(YR_BUILT=2020-YR_BUILT)%>%
  rename(age_building=YR_BUILT)
monroe_landlord<-monroe_issues_info%>%
  left_join(monroe_property)%>%
  mutate("primary_city"="monroe")%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0)) 
monroe_citymatch<-monroe_landlord%>%
  left_join(monroe_issues_info)%>%
  left_join(monroe_property)%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) 
monroe_complete<-monroe_landlord%>% 
  left_join(monroe_issues_info)%>%
  left_join(monroe_info)%>%
  left_join(monroe_property)%>%
  left_join(monroe_citymatch)%>%
  mutate("primary_city"="monroe")%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"outtown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"intown_landlord"))
  
###Survey 1 COmplete
survey_1<-bind_rows(gainesville_complete,millen_complete,monroe_complete)%>%
  filter(!is.na(condition))%>%
  filter(condition!="NA")%>%
  mutate("survey"="one")%>%
  filter(!is.na(ownname))%>%
  filter(ownname!="NA")
write.xlsx(survey_1,"revised_survey_1.xlsx")
###Survey 1 Map
survey_1_as_sf<-survey_1 %>%
  mutate(x=as.numeric(X))%>%
  mutate(y=as.numeric(Y))%>%
  st_as_sf(coords=c("x","y"),crs=4326,remove=FALSE)
survey_1_as_sf$condition<-factor(survey_1_as_sf$condition, levels = c("Standard","Substandard","Dilapidated"))
tmap_mode("view")
tm_shape(survey_1_as_sf)+
  tm_dots(col="condition",palette=c(Standard='grey', Substandard='red', Dilapidated='red'))+
  tm_dots(size=.1)
### Commerce
commerce_issues_info <- read_csv("data/commerce_parcel_points.csv")%>%
  mutate(primary_city="Commerce")%>%
  select(primary_city,parcel_num,address,realkey,homeexempt,name1,ownaddress,owncity,condition,X,Y)%>%
  rename(ownname=name1,REALKEY=realkey)
commerce_property<-read_excel("data/commerce property .xlsx")%>%
  select(REALKEY,YR_BUILT,NO_BEDRMS,FULLBATHS,HEATEDAREA)%>%
  mutate(YR_BUILT=2020-YR_BUILT)%>%
  rename(age_building=YR_BUILT)
commerce_value<- read_excel("data/Commerce value.xlsx")%>%
  select(REALKEY,PARCEL_NO,CURR_VAL,OWNKEY)%>%
  rename(parcel_num=PARCEL_NO,ownkey=OWNKEY)
commerce_state_info <- read_excel("data/commerce owner.xlsx")%>%
  select(ownkey,ownstate)
commerce_landlord<-commerce_issues_info%>%
  left_join(commerce_property)%>%
  left_join(commerce_value)%>%
  left_join(commerce_state_info)%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  rename(OWNKEY=ownkey)%>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0)) 
commerce_citymatch<-commerce_landlord%>%
  left_join(commerce_issues_info)%>%
  left_join(commerce_property)%>% left_join(commerce_value)%>%
  left_join(commerce_state_info)%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) 
commerce_complete<-commerce_landlord%>%
  left_join(commerce_issues_info)%>%
  left_join(commerce_property)%>%
  left_join(commerce_value)%>%
  left_join(commerce_state_info)%>%
  left_join(commerce_citymatch)%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  rename(OWNKEY=ownkey)%>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"outtown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"intown_landlord"))
###Hartwell
hartwell_issues_info <- read_excel("data/hartwell_issues_owner_correct.xlsx")%>%
  mutate(primary_city="Hartwell")%>%
  select(primary_city,parcel_num,address,realkey,homeexempt,ownname,ownaddress,owncity,condition,lat,long,ownstate)%>%
  rename(X=long,Y=lat,REALKEY=realkey)%>%
  mutate(REALKEY=as.character(REALKEY))
hartwell_property<-read_excel("data/hartwell property.xlsx")%>%
  select(REALKEY,YR_BUILT,NO_BEDRMS,FULLBATHS,HEATEDAREA)%>%
  mutate(YR_BUILT=2020-YR_BUILT)%>%
  rename(age_building=YR_BUILT)%>%
  mutate(REALKEY=as.character(REALKEY))
hartwell_value<- read_excel("data/hartwell value.xlsx")%>%
  select(REALKEY,PARCEL_NO,CURR_VAL)%>%
  rename(parcel_num=PARCEL_NO)%>%
  mutate(REALKEY=as.character(REALKEY))
hartwell_landlord<-hartwell_issues_info%>%
  left_join(hartwell_property)%>%
  left_join(hartwell_value)%>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0))
hartwell_citymatch<-hartwell_landlord%>%
  left_join(hartwell_issues_info)%>%
  left_join(hartwell_property)%>%
  left_join(hartwell_value)%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch)
hartwell_complete<-hartwell_landlord%>% 
  left_join(hartwell_issues_info)%>%
  left_join(hartwell_property)%>%
  left_join(hartwell_value)%>%
  left_join(hartwell_citymatch)%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"outtown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"intown_landlord"))
#Warrenton
warrenton_issues <- read_excel("data/warrenton issues.xlsx")%>%
  select(condition,X,Y,Parcel_No,homestead)%>%
  rename(homeexempt=homestead,parcel_num=Parcel_No)
warrenton_issues_information<- warrenton_issues%>%
  left_join(warrenton_information1)
warrenton_property <- read_excel("data/warrenton property.xlsx")%>%
  select(REALKEY,YR_BUILT,NO_BEDRMS,FULLBATHS,HEATEDAREA)%>%
  mutate(YR_BUILT=2020-YR_BUILT)%>%
  rename(age_building=YR_BUILT)%>%
  mutate(REALKEY=as.character(REALKEY))
warrenton_value <- read_excel("data/warrenton value.xlsx")%>%
  select(REALKEY,PARCEL_NO,CURR_VAL,OWNKEY)%>%
  rename(parcel_num=PARCEL_NO)%>%
  mutate(REALKEY=as.character(REALKEY))
warrenton_owner<-read_excel("data/warrenton owner.xlsx")%>%
  select(OWNKEY,LASTNAME,ADDRESS1,CITY,STATE)%>%
  rename(ownname=LASTNAME,ownaddress=ADDRESS1,owncity=CITY,ownstate=STATE)
warrenton_owner_value<-(warrenton_owner)%>%
   left_join(warrenton_value)
warrenton_owner_value_issues<-warrenton_issues%>%
  left_join(warrenton_owner_value)
warrenton_landlord<-warrenton_owner_value_issues%>%
  left_join(warrenton_property)%>%
  mutate("primary_city"="warrenton")%>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0)) 
warrenton_citymatch<-warrenton_landlord%>%
  left_join(warrenton_owner_value_issues)%>%
  left_join(warrenton_property)%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch)
warrenton_complete<-warrenton_landlord%>%
  left_join(warrenton_owner_value_issues)%>%
  left_join(warrenton_property)%>%
  left_join(warrenton_citymatch)%>%
  mutate("primary_city"="warrenton")%>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"outtown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"intown_landlord"))
#Cochran
cochran_home_exempt <- read_csv("data/cochran home exempt.csv")%>%
  select("PARCEL_NO","HOMEEXEMPT")%>%
  rename(parcel_num=PARCEL_NO)
Cochran_info <- read_excel("data/Cochran info.xlsx")%>%
  select(ownkey,ownname,ownaddress,owncity,ownstate)
Cochran_Issues <- read_excel("data/Cochran Issues.xlsx")%>%
  select(latitude...12,longitude...13,address,ownkey,condition,parcel_num)%>%
  rename(Y=latitude...12,X=longitude...13)
cochran_info_issues<-Cochran_Issues%>%
  left_join(Cochran_info)%>%
  left_join(cochran_home_exempt)
cochran_property<- read_excel("data/cochran property.xlsx")%>%
  select(realkey,yr_built,no_bedrms,fullbaths,heatedarea)%>%
  mutate(yr_built=2020-yr_built)%>%
  rename(age_building=yr_built)
cochran_value<-read_excel("data/cochran value.xlsx")%>%
  select(realkey,parcel_no,curr_val,ownkey)%>%
  rename(parcel_num=parcel_no)
cochran_info_issues_value<-cochran_info_issues%>%
  left_join(cochran_value)%>%
  rename(OWNKEY=ownkey)
cochran_landlord<-cochran_info_issues_value%>%
  left_join(cochran_property)%>%
  mutate("primary_city"="cochran")%>%
  rename(CURR_VAL=curr_val,NO_BEDRMS=no_bedrms,HEATEDAREA=heatedarea,FULLBATHS=fullbaths,REALKEY=realkey)%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0)) 
cochran_citymatch<-cochran_landlord%>%
  left_join(cochran_info_issues_value)%>%
  left_join(cochran_property)%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch)
cochran_complete<-cochran_landlord%>%
  left_join(cochran_info_issues_value)%>%
  left_join(cochran_property)%>%
  left_join(cochran_citymatch)%>%
  mutate("primary_city"="cochran")%>%
  rename(CURR_VAL=curr_val,NO_BEDRMS=no_bedrms,HEATEDAREA=heatedarea,FULLBATHS=fullbaths,REALKEY=realkey,homeexempt=HOMEEXEMPT)%>%
  mutate(REALKEY=as.character(REALKEY))%>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"outtown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"intown_landlord"))

###Survey 2 Combined
survey_2<-bind_rows(hartwell_complete,commerce_complete,warrenton_complete,cochran_complete)%>%
  filter(!is.na(condition))%>%
  filter(condition!="NA")%>%
  mutate("survey"="two")%>%
  filter(!is.na(ownname))%>%
  filter(ownname!="NA")
write.xlsx(survey_2,"revised_survey_2.xlsx")
###Survey 2 Map
survey_2_as_sf<-survey_2 %>%
  mutate(x=as.numeric(X))%>%
  mutate(y=as.numeric(Y))%>%
  st_as_sf(coords=c("x","y"),crs=4326,remove=FALSE)
survey_2_as_sf$condition<-factor(survey_2_as_sf$condition, levels = c("well maintained","sound","minor repairs needed",
                                                                      "moderate rehabilitation needed","substantial rehabilitation needed","dilapidated"))
tmap_mode("view")
tm_shape(survey_2_as_sf)+
  tm_dots(col="condition",palette=c("well maintained"='grey', "sound"='grey', "minor repairs needed"='grey',
                                    "moderate rehabilitation needed"= 'red', "substantial rehabilitation needed"='red',"dilapidated"='red'))+
  tm_dots(size=.1)

###All Survey
all_survey<-bind_rows(survey_1,survey_2)
write.xlsx(all_survey,"revised_all_survey.xlsx")

