library(tidyverse)
install.packages("openxlsx") 
library(openxlsx)
library(sf)
library(viridis)
library(tmap)
s1data<-read_csv("s1_data.csv")
s1data<-s1data%>%
  left_join(gainesville_latlong)
s1data_landlord<-s1data %>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0))
###Millen 
s1data1<-s1data %>%
  left_join(s1data_landlord) %>%
  mutate(primcity_lower=tolower(primary_city),
         owncity_lower=tolower(owncity),
         citymatch=if_else(primcity_lower==owncity_lower,1,0)) %>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"intown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"outtown_landlord")) %>%
  filter(primary_city=="millen")

chisq.test(s1data1$classify,s1data1$landlord)
###p-value < 2.2e-16
s1data1_tbl_millen<-s1data1 %>%
  count(primary_city,landlord,classify,name="count") %>%
  group_by(landlord) %>%
  mutate(total=sum(count)) %>%
  mutate(pct=round(count/total*100,1)) %>%
  select(primary_city,landlord,classify,pct) %>%
  spread(classify,pct)

write_csv(s1data1,"s1data_test.csv")
write.xlsx(s1data1_tbl_millen,"s1data1_tbl_millen.xlsx")
millen_as_sf<-s1data %>%filter(primary_city=="millen")%>%
  mutate(x=as.numeric(x))%>%
  mutate(y=as.numeric(y))%>%
  st_as_sf(coords=c("x","y"),crs=4326,remove=FALSE)
tmap_mode("view")
tm_shape(millen_as_sf)+
  tm_dots(col="classify")
###Monroe
s1data1<-s1data %>%
  left_join(s1data_landlord) %>%
  mutate(primcity_lower=tolower(primary_city),
         owncity_lower=tolower(owncity),
         citymatch=if_else(primcity_lower==owncity_lower,1,0)) %>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"intown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"outtown_landlord")) %>%
  filter(primary_city=="monroe")

chisq.test(s1data1$classify,s1data1$landlord)
###p-value < 2.14e-11
s1data1_tbl_monroe<-s1data1 %>%
  count(primary_city,landlord,classify,name="count") %>%
  group_by(landlord) %>%
  mutate(total=sum(count)) %>%
  mutate(pct=round(count/total*100,1)) %>%
  select(primary_city,landlord,classify,pct) %>%
  spread(classify,pct)

write.xlsx(s1data1_tbl_monroe,"s1data1_tbl_monroe.xlsx")
monroe_as_sf<-s1data  %>%filter(primary_city=="monroe")%>%
  mutate(x=as.numeric(x))%>%
  mutate(y=as.numeric(y))%>%
  st_as_sf(coords=c("x","y"),crs=4326,remove=FALSE)
tmap_mode("view")
tm_shape(monroe_as_sf)+
  tm_dots(col="classify")
###Gainesville
s1data1<-s1data %>%
  left_join(s1data_landlord) %>%
  mutate(primcity_lower=tolower(primary_city),
         owncity_lower=tolower(owncity),
         citymatch=if_else(primcity_lower==owncity_lower,1,0)) %>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"intown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"outtown_landlord")) %>%
  filter(primary_city=="gainesville")

chisq.test(s1data1$classify,s1data1$landlord)
###p-value 0.001725
s1data1_tbl_gaineseville<-s1data1 %>%
  count(primary_city,landlord,classify,name="count") %>%
  group_by(landlord) %>%
  mutate(total=sum(count)) %>%
  mutate(pct=round(count/total*100,1)) %>%
  select(primary_city,landlord,classify,pct) %>%
  spread(classify,pct)

write.xlsx(s1data1_tbl_gaineseville,"s1data1_tbl_gainesville.xlsx")
gainesville_as_sf<-s1data %>%filter(primary_city=="gainesville")%>%
  mutate(x=as.numeric(x))%>%
  mutate(y=as.numeric(y))%>%
  st_as_sf(coords=c("long","lat"),crs=4326,remove=FALSE)
tmap_mode("view")
tm_shape(gainesville_as_sf)+
  tm_dots(col="classify")
###Hartwell
s2data_landlord<-s2_data %>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0))
s2data2<-s2_data %>%
  left_join(s2data_landlord) %>%
  mutate(primcity_lower=tolower(primary_city),
         owncity_lower=tolower(owncity),
         citymatch=if_else(primcity_lower==owncity_lower,1,0)) %>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"intown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"outtown_landlord")) %>%
  filter(primary_city=="hartwell")

chisq.test(s2data2$condition,s2data2$landlord)
###p-value 0.0002786
s2data2_tbl_hartwell<-s2data2 %>%
  count(primary_city,landlord,condition,name="count") %>%
  group_by(landlord) %>%
  mutate(total=sum(count)) %>%
  mutate(pct=round(count/total*100,1)) %>%
  select(primary_city,landlord,condition,pct) %>%
  spread(condition,pct)

write.xlsx(s2data2_tbl_hartwell,"s2data1_tbl_hartwell.xlsx")
hartwell_as_sf<-s2_data %>%filter(primary_city=="hartwell")%>%
  filter(condition!="NA")%>%
  mutate(x=as.numeric(x))%>%
  mutate(y=as.numeric(y))%>%
  st_as_sf(coords=c("x","y"),crs=4326,remove=FALSE)
tmap_mode("view")
tm_shape(hartwell_as_sf)+
  tm_dots(col="condition")
###Cochran
s2data_landlord<-s2_data %>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0))
s2data2<-s2_data %>%
  left_join(s2data_landlord) %>%
  mutate(primcity_lower=tolower(primary_city),
         owncity_lower=tolower(owncity),
         citymatch=if_else(primcity_lower==owncity_lower,1,0)) %>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"intown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"outtown_landlord")) %>%
  filter(primary_city=="cochran")

chisq.test(s2data2$condition,s2data2$landlord)
###p-value 1.143e-09
s2data2_tbl_cochran<-s2data2 %>%
  count(primary_city,landlord,condition,name="count") %>%
  group_by(landlord) %>%
  mutate(total=sum(count)) %>%
  mutate(pct=round(count/total*100,1)) %>%
  select(primary_city,landlord,condition,pct) %>%
  spread(condition,pct)

write.xlsx(s2data2_tbl_cochran,"s2data1_tbl_cochran.xlsx")
cochran_as_sf<-s2_data %>%filter(primary_city=="cochran")%>%
  filter(condition!="NA")%>%
  mutate(x=as.numeric(x))%>%
  mutate(y=as.numeric(y))%>%
  st_as_sf(coords=c("x","y"),crs=4326,remove=FALSE)
tmap_mode("view")
tm_shape(cochran_as_sf)+
  tm_dots(col="condition")
###Commerce
s2data_landlord<-s2_data %>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0))
s2data2<-s2_data %>%
  left_join(s2data_landlord) %>%
  mutate(primcity_lower=tolower(primary_city),
         owncity_lower=tolower(owncity),
         citymatch=if_else(primcity_lower==owncity_lower,1,0)) %>%
  mutate(landlord=case_when(homeexempt!="S0"~"owner",
                            homeexempt=="S0" & multiprop==0~"singleowner",
                            homeexempt=="S0" & citymatch==1 & multiprop==1~"intown_landlord",
                            homeexempt=="S0" & citymatch==0 & multiprop==1~"outtown_landlord")) %>%
  filter(primary_city=="Commerce")

chisq.test(s2data2$condition,s2data2$landlord)
###p-value 3.497e-05
s2data2_tbl_commerce<-s2data2 %>%
  count(primary_city,landlord,condition,name="count") %>%
  group_by(landlord) %>%
  mutate(total=sum(count)) %>%
  mutate(pct=round(count/total*100,1)) %>%
  select(primary_city,landlord,condition,pct) %>%
  spread(condition,pct)

write.xlsx(s2data2_tbl_commerce,"s2data1_tbl_commerce.xlsx")
commerce_as_sf<-s2data2 %>%filter(primary_city=="Commerce")%>%
  filter(condition!="NA")%>%
  mutate(x=as.numeric(x))%>%
  mutate(y=as.numeric(y))%>%
  st_as_sf(coords=c("x","y"),crs=4326,remove=FALSE)
tmap_mode("view")
tm_shape(commerce_as_sf)+
  tm_dots(col="condition")+
  tm_dots(size=.1)
