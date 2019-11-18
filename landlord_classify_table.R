library(tidyverse)
s1data<-read_csv("s1_data.csv")

s1data_landlord<-s1data %>%
  count(primary_city,ownname,name="count") %>%
  mutate(multiprop=if_else(count>1,1,0))
 
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

s1data1_tbl<-s1data1 %>%
  count(primary_city,landlord,classify,name="count") %>%
  group_by(landlord) %>%
  mutate(total=sum(count)) %>%
  mutate(pct=round(count/total*100,1)) %>%
  select(primary_city,landlord,classify,pct) %>%
  spread(classify,pct)

write_csv(s1data1,"s1data_test.csv")
