library(tidyverse)
library(readxl)
###Read in Data
commerce_issues <- read.csv("commerce issues.csv")
commerce_owner <- read.csv("commerce owner.csv")
commerce_proprty <- read.csv("commerce proprty.csv")
Gainesville_issues_and_info <- read.csv("Gainesville issues and info.csv")
hartwell_issues_and_owner_correct_version <- read.csv("hartwell issues and owner correct version.csv")
millen_info_with_parcel_number <- read.csv("millen info with parcel number.csv")
millen_issues <- read.csv("millen issues.csv")
millen_owner <- read.csv("millen_owner.csv")
monroe_info_full <- read.csv("monroe info.csv")
monroe_issues <- read.csv("monroe issues.csv")
warrenton_information_full <- read.csv("warrenton information.csv")
warrenton_issues <- read.csv("warrenton issues.csv")
Cochran_Issues <- read.csv("Cochran Issues.csv")
Cochran_info_full <- read.csv("Cochran info.csv")
### Combine Commerce Data
commerce_owner_property_full<-commerce_owner%>%
  left_join(commerce_proprty)
commerce_owner_property<-commerce_owner_property_full%>%
  select(ownname,ownaddress,owncity,ownstate,ownzip,Parcel_No,HOMEEXEMPT)
commerce_issues_info<-commerce_issues%>%
  left_join(commerce_owner_property)
commerce_info_issues<-commerce_issues_info%>%
  mutate("primary_city"="Commerce")
commerce_info_issues <- commerce_info_issues %>%  
  mutate(realkey = as.integer(realkey))
### Combine Gainesville data
Gainesville_issues_and_info <- read_excel("Gainesville issues and info.xlsx")
gainesville_info_issues<-Gainesville_issues_and_info%>%
  mutate("primary_city"="gainesville")
###Combine hartwell
hartwell_info_issues<-hartwell_issues_and_owner_correct_version
hartwell_info_issues<-hartwell_info_issues%>%
  mutate("primary_city"="hartwell")
###Combine millen
millen_info_full<-millen_info_with_parcel_number%>%
  left_join(millen_owner)
millen_info<-millen_info_full%>%
  select(Parcel_No,ownname,ownaddress,owncity,ownstate)
millen_info_issues<-millen_issues%>%
  left_join(millen_info)
millen_info_issues<-millen_info_issues%>%
  mutate("primary_city"="millen")
###Combine monroe
monroe_info<-monroe_info_full%>%
  select(ownname,ownaddress,owncity,ownstate,ownzip,homeexempt,Parcel_No)
monroe_info_issues<-monroe_issues%>%
  left_join(monroe_info)
monroe_info_issues<-monroe_info_issues%>%
  mutate("primary_city"="monroe")
###Combine cochran
cochran_info<-Cochran_info_full%>%
  select(ownkey,ownname,ownaddress,owncity,ownstate,ownzip)
cochran_info_issues<-Cochran_Issues%>%
  left_join(cochran_info)
cochran_info_issues<-cochran_info_issues%>%
  mutate("primary_city"="cochran")
cochran_info_issues <- cochran_info_issues %>%  
  mutate(realkey = as.character(realkey))
###Combine Warrenton
warrenton_spacer<-substr(warrenton_issues[3,]$Parcel_No,4,7)
warrenton2<-warrenton_issues %>%
  mutate(parcel_new=gsub(warrenton_spacer," ",Parcel_No))
warrenton_information <- warrenton_information_full %>%  
  mutate(ownaddress = as.logical(ownaddress))
warrenton_info_issues_incom<-warrenton_issues%>%
  left_join(warrenton_information)%>%
  mutate("primary_city"="warrenton")
warrenton_info_issues <- warrenton_info_issues_incom %>%  
  mutate(homestead = as.numeric(homestead))



###eliminate all data from property and owner, except own address,name, city, state, homestead variable, eliminate old curo projects
#change variable names to match. seperate one for each survey, finish the crosswalk file

 ### merge all files together

###merge all data


full_bind<-bind_rows(millen_info_issues, gainesville_info_issues,
                          hartwell_info_issues,cochran_info_issues,
                          monroe_info_issues,warrenton_info_issues, commerce_info_issues)
full_data_combine<-merge(gainesville_info_issues,millen_info_issues,hartwell_info_issues,cochran_info_issues,millen_info_issues,warrenton_info_issues,by=c("primary_city"))
