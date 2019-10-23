library(tidyverse)
library(readxl)
###Read in Data
commerce_issues <- read_excel("commerce issues.xlsx")
commerce_owner <- read_excel("commerce owner.xlsx")
commerce_proprty <- read_excel("commerce proprty.xlsx")
Gainesville_issues_and_info <- read_excel("Gainesville issues and info.xlsx")
hartwell_issues_and_owner_correct_version <- read_excel("hartwell issues and owner correct version.xlsx")
millen_info_with_parcel_number <- read_excel("millen info with parcel number.xlsx")
millen_issues <- read_excel("millen issues.xlsx")
millen_owner <- read_excel("millen_owner.xlsx")
monroe_info <- read_excel("monroe info.xlsx")
monroe_issues <- read_excel("monroe issues.xlsx")
warrenton_information <- read_excel("warrenton information.xlsx")
warrenton_issues <- read_excel("warrenton issues.xlsx")
Cochran_Issues <- read_excel("Cochran Issues.xlsx")
Cochran_info <- read_excel("Cochran info.xlsx")
### Combine Commerce Data
commerce_owner_property<-commerce_owner%>%
  left_join(commerce_proprty)
commerce_issues_info<-commerce_issues%>%
  left_join(commerce_owner_property)
commerce_info_issues<-commerce_issues_info%>%
  mutate("primary_city"="Commerce")
### Combine Gainesville data
Gainesville_issues_and_info <- read_excel("Gainesville issues and info.xlsx")
gainesville_info_issues<-Gainesville_issues_and_info%>%
  mutate("primary_city"="gainesville")
###Combine hartwell
hartwell_info_issues<-hartwell_issues_and_owner_correct_version
hartwell_info_issues<-hartwell_info_issues%>%
  mutate("primary_city"="hartwell")
###Combine millen
millen_info<-millen_info_with_parcel_number%>%
  left_join(millen_owner)
millen_info_issues<-millen_issues%>%
  left_join(millen_info)
millen_info_issues<-millen_info_issues%>%
  mutate("primary_city"="millen")
###Combine monroe
monroe_info_issues<-monroe_issues%>%
  left_join(monroe_info)
monroe_info_issues<-monroe_info_issues%>%
  mutate("primary_city"="monroe")
###Combine cochran
cochran_info_issues<-Cochran_Issues%>%
  left_join(Cochran_info)
cochran_info_issues<-cochran_info_issues%>%
  mutate("primary_city"="cochran")
###Combine Warrenton, need help on fizxing excel
warrenton_info_issues<-warrenton_issues%>%
  left_join(warrenton_information)

###eliminate all data from property and owner, except own address,name, city, state, homestead variable, eliminate old curo projects
#change variable names to match. seperate one for each survey, finish the crosswalk file

 ### merge all files together
full_data_combine<-merge(gainesville_info_issues,millen_info_issues,hartwell_info_issues,cochran_info_issues,millen_info_issues,by=c("primary_city"))
