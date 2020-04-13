#Issue_analysis

library(tidyverse)
library(readxl)

#Load data
s1_data<-read_csv("s1_data.csv")
s2_data<-read_csv("s2_data.csv")

s1_cw<-read_excel("data/s1_2_crosswalk.xlsx",sheet="s1_crosswalk")

#Transform s1 variables to match survey 2
s1_data_cw<-s1_data %>%
  gather(c(min_chimn:maj_winddo,yard_grcov:lit_yard),key="s1_var",value="value") %>%
  left_join(s1_cw) %>%
  filter(is.na(s2_var)==FALSE) %>%
  select(-s1_var) %>%
  group_by(s2_var,parcel_num,record_id) %>%
  mutate(value=max(value)) %>%
  distinct() %>%
  spread(s2_var,value) %>%
  mutate(ownzip=as.character(ownzip)) 

survdata<-bind_rows(s2_data,s1_data_cw)

#Overall_rates
survey_table_all<-survdata %>%
  gather(found_compreplace:yard_weeds,key=issue,value=value) %>%
  group_by(issue) %>%
  summarise(issue_count=sum(value,na.rm=TRUE),
            records=n()) %>%
  mutate(issue_pct=if_else(is.na(issue_count),-99,
                           round(issue_count/records*100,1))) %>%
  select(issue,issue_pct) %>%
  filter(issue!="yard_satis" & issue_pct>2) %>%
  mutate(primary_city="All")

#Create rates by city
survey_table<-survdata %>%
  gather(found_compreplace:yard_weeds,key=issue,value=value) %>%
  group_by(primary_city,issue,survey) %>%
  summarise(issue_count=sum(value,na.rm=TRUE),
            records=n()) %>%
  mutate(issue_pct=if_else(is.na(issue_count),-99,
                           round(issue_count/records*100,1))) %>%
  select(primary_city,issue,survey,issue_pct) %>%
  filter(issue %in% survey_table_all$issue)

survey_table_combine<-bind_rows(survey_table_all,survey_table) %>%
  filter(primary_city!="gainesville")

ggplot(survey_table_combine,aes(x=issue,y=issue_pct,color=primary_city))+
  geom_point() +
  ylim(0,30)

ggplot(survey_table_combine,aes(x=primary_city,y=issue_pct,fill=primary_city))+
  geom_bar(stat="identity") +
  coord_flip()+
  facet_wrap(~issue)

survey_table_wide<-survey_table_combine %>%
  spread(primary_city,issue_pct)
write_csv(survey_table_wide,"data/survey_table.csv")
 
#ratings
s1_class<-s1_data %>%
  count(primary_city,classify) %>%
  group_by(primary_city) %>%
  mutate(records=sum(n),
         pct=round(n/records*100,1)) %>%
  select(-n) %>%
  spread(classify,pct)
write_csv(s1_class,"s1_classifications.csv")

s2_class<-s2_data %>%
  count(primary_city,condition) %>%
  group_by(primary_city) %>%
  mutate(records=sum(n),
         pct=round(n/records*100,1)) %>%
  select(-n) %>%
  spread(condition,pct)
write_csv(s2_class,"s2_classifications.csv")
