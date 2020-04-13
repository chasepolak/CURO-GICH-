library(tidyverse)
library(lubridate)

#Script to combine times and add to the survey data
cochran_times<-read_csv("data/Cochran/cochran_times.csv") %>%
  rename(record_id=fulcrum_id) %>%
  mutate(date=as_date(created_at)) %>%
  select(-created_at) %>%
  mutate(primary_city="cochran")
commerce_times<-read_csv("data/Commerce/commerce_times.csv") %>%
  rename(record_id=X_uuid) %>%
  mutate(primary_city="Commerce")
hartwell_times<-read_csv("data/Hartwell_git/hartwell_times.csv")%>%
  rename(record_id=X_uuid)%>%
  mutate(primary_city="hartwell")
millen_times<-read_csv("data/Millen/millen_times.csv") %>%
  rename(record_id=X_uuid,
         date=Date,
         parcel_num=Parcel_ID) %>%
  mutate(primary_city="millen")
monroe_times<-read_csv("data/Monroe/monroe_times.csv")  %>%
  rename(record_id=X_uuid,
         date=Date) %>%
  mutate(primary_city="monroe")
warrenton_times<-read_csv("data/Warrenton/warrenton_times.csv") %>%
  rename(record_id=fulcrum_id) %>%
  mutate(date=as_date(created_at)) %>%
  select(-created_at) %>%
  mutate(primary_city="warrenton")
warrenton_spacer<-substr(warrenton_times[3,]$parcel_num,4,7)
warrenton_times<-warrenton_times %>%
  rename(parcelno_old=parcel_num) %>%
  mutate(parcel_num=gsub(warrenton_spacer," ",parcelno_old)) 
warrenton_times$parcelno_old<-NULL

all_records<-bind_rows(cochran_times,commerce_times,hartwell_times,millen_times,
                       monroe_times,
                       warrenton_times)
write_csv(all_records,"data/record_times_all.csv")
