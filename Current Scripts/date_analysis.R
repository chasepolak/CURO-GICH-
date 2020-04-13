#Date analysis
library(tidyverse)
library(lubridate)
library(readxl)
library(ggbeeswarm)

pseudonym<-read_excel("data/Community table.xlsx") %>%
  mutate(primary_city=tolower(City)) %>%
  select(primary_city,Pseudonym)

allsurvey_data<-read_csv("allsurvey_data.csv") %>%
  mutate(primary_city=tolower(primary_city)) %>%
  left_join(pseudonym)

dates<-allsurvey_data %>%
  filter(primary_city!="gainesville") %>%
  group_by(primary_city) %>%
  select(Pseudonym,date) %>%
  filter(is.na(date)==FALSE) %>%
  mutate(start_date=min(date),
         end_date=max(date),
         date_num=interval(start_date,date)/days(1))

ggplot(dates,aes(x=date_num))+
  geom_histogram()+
  facet_wrap(~Pseudonym,scales="free") +
  theme_minimal() +
  xlab("Number of days after starting") +
  ylab("Number of properties assessed")


ggplot(dates,aes(x=Pseudonym,y=date_num))+
  geom_quasirandom(width=0.2)+
  theme_minimal() +
  ylab("") +
  xlab("Number of days after starting")

#Completion graph
dates1<-dates %>%
  group_by(Pseudonym) %>%
  arrange(date) %>%
  mutate(dummy=1,
         cumtotal=cumsum(dummy),
         cumpct=cumtotal/max(cumtotal)*100)

cbPalette <- c("#66c2a5", "#ff7f00", "#8da0cb", "#e41a1c", "#a6d854", "#ffd92f")

ggplot(dates1,aes(x=date_num,y=cumpct,color=Pseudonym))+
  geom_line()+
  theme_minimal() +
  scale_color_manual(values=cbPalette)+
  xlab("") +
  ylab("Number of days after starting")+
  labs(colour="Community")
