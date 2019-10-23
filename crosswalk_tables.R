library(tidyverse)

#Create a variable table for each community
monroe_vars<-as.tibble(names(monroe_info_issues)) %>%
  mutate(monroe_pres=1)
gaines_vars<-as.tibble(names(gainesville_info_issues)) %>%
  mutate(gaines_pres=1)
hartwell_vars<-as.tibble(names(hartwell_info_issues)) %>%
  mutate(hartwell_pres=1)
commerce_vars<-as.tibble(names(commerce_info_issues)) %>%
  mutate(commerce_pres=1)
warrenton_vars<-as.tibble(names(warrenton_info_issues)) %>%
  mutate(warrenton_pres=1)
cochran_vars<-as.tibble(names(cochran_info_issues)) %>%
  mutate(cochran_pres=1)
millen_vars<-as.tibble(names(millen_info_issues)) %>%
  mutate(millen_pres=1)
###error shows up when I try to full join the vars tables
all_vars<-full_join(monroe_vars,gaines_vars,hartwell_vars,commerce_vars,warrenton_vars,cochran_vars,millen_vars)
write_csv(all_vars,"data/var_crosswalk.csv")

#warrenton parcels
warrenton_spacer<-substr(warrenton_issues[3,]$Parcel_No,4,7)
warrenton2<-warrenton_issues %>%
  mutate(parcel_new=gsub(warrenton_spacer," ",Parcel_No))
head(warrenton2$parcel_new)
