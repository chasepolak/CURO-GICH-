library(tidyverse)

#Create a variable table for each community
commerce_vars<-as.tibble(names(commerce_info_issues)) %>%
  mutate(commerce_pres=1)
monroe_vars<-as.tibble(names(monroe_info_issues)) %>%
  mutate(monroe_pres=1)
gaines_vars<-as.tibble(names(gainesville_info_issues)) %>%
  mutate(gaines_pres=1)

all_vars<-full_join(gaines_vars,monroe_vars)
write_csv(all_vars,"data/var_crosswalk.csv")


#warrenton parcels
warrenton_spacer<-substr(warrenton_issues[3,]$Parcel_No,4,7)
warrenton2<-warrenton_issues %>%
  mutate(parcel_new=gsub(warrenton_spacer," ",Parcel_No))
head(warrenton2$parcel_new)
