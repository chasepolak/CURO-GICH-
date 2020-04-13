
na_homex<-allsurvey_data %>%
  filter(is.na(homeexempt)==TRUE) %>%
  count(primary_city)

owncity<-allsurvey_data %>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) %>%
  right_join(allsurvey_data) %>%
  filter(is.na(owncity)==FALSE)

owncity_tbl<-owncity %>%
  group_by(primary_city) %>%
  summarise(owncount=sum(citymatch),
            propcount=n(),
            pct=owncount/propcount*100) 

owncity_filter<-owncity %>%
  filter(citymatch==1)

warrenton_cities<-owncity_filter %>%
  filter(primary_city=="warrenton") %>%
  count(owncity)

monroe_owner<-owncity_filter %>%
  filter(primary_city=="monroe") %>%
  mutate(ownname_start=tolower(substr(ownname,1,9))) %>%
  count(ownname_start)

all_owner<-owncity_filter %>%
  mutate(ownname_start=tolower(substr(ownname,1,12))) %>%
  count(ownname_start,primary_city) %>%
  spread(primary_city,n)

all_city<-owncity_filter %>%
  mutate(ownname_start=tolower(substr(ownname,1,12))) %>%
  count(owncity,primary_city) %>%
  spread(primary_city,n)
###scoring conditions survey 1
s1_data$classify <- replace(as.character(s1_data$classify), s1_data$classify == "Substandard", "2")
s1_data$classify <- replace(as.character(s1_data$classify), s1_data$classify == "Standard", "1")
s1_data$classify <- replace(as.character(s1_data$classify), s1_data$classify == "Dilapidated", "3")
s1_data<-s1_data%>%
  filter(is.na(classify)==FALSE)%>%
  mutate(classify=as.numeric(classify))
s1_data$gen_cond <- replace(as.character(s1_data$gen_cond), s1_data$gen_cond == "Good", "1")
s1_data$gen_cond <- replace(as.character(s1_data$gen_cond), s1_data$gen_cond == "good", "1")
s1_data$gen_cond <- replace(as.character(s1_data$gen_cond), s1_data$gen_cond== "fair", "2")
s1_data$gen_cond <- replace(as.character(s1_data$gen_cond), s1_data$gen_cond== "Fair", "2")
s1_data$gen_cond <- replace(as.character(s1_data$gen_cond), s1_data$gen_cond == "Poor", "3")
s1_data$gen_cond <- replace(as.character(s1_data$gen_cond), s1_data$gen_cond == "poor", "3")
s1_data$gen_cond <- replace(as.character(s1_data$gen_cond), s1_data$gen_cond == "0", "1")
s1_data<-s1_data%>%
  filter(is.na(gen_cond)==FALSE)%>%
  mutate(gen_cond=as.numeric(gen_cond))
prop_count<-s1_data%>%
  group_by(ownname,primary_city,owncity,ownstate)%>%
  summarise(prop_count=n())
s1_data<-s1_data%>%
  left_join(prop_count)

###scoring condistions survey 2
s2_data$condition <- replace(as.character(s2_data$condition), s2_data$condition == "well maintained", "1")
s2_data$condition <- replace(as.character(s2_data$condition), s2_data$condition == "substantial rehabilitation needed", "5")
s2_data$condition <- replace(as.character(s2_data$condition), s2_data$condition == "sound", "2")
s2_data$condition <- replace(as.character(s2_data$condition), s2_data$condition == "moderate rehabilitation needed", "4")
s2_data$condition <- replace(as.character(s2_data$condition), s2_data$condition == "minor repairs needed", "3")
s2_data$condition <- replace(as.character(s2_data$condition), s2_data$condition == "dilapidated", "6")

###Look at common home exempt households
home_exempt_s1<-s1_data%>%
  filter(homeexempt!="S0")%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) %>%
  right_join(home_exempt_s1) %>%
  filter(is.na(owncity)==FALSE) 
non_home_exempts1<-s1_data%>%
  filter(homeexempt=="S0")%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) %>%
  right_join(non_home_exempts1) %>%
  filter(is.na(owncity)==FALSE) 
home_exempt_s2<-s2_data%>%
  filter(homeexempt!="S0")%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) %>%
  right_join(non_home_exempt_s2) %>%
  filter(is.na(owncity)==FALSE) %>%
  filter(primary_city!="warrenton")
non_home_exempt_s2<-s2_data%>%
  filter(homeexempt=="S0")%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) %>%
  right_join(non_home_exempt_s2) %>%
  filter(is.na(owncity)==FALSE) %>%
  filter(primary_city!="warrenton")
###top landowners survey 1
top_own_homeexempt_s1<-home_exempt_s1%>%
  mutate(owner_name=tolower(substr(ownname,1,12)))%>%
  group_by(owner_name,primary_city,owncity,ownstate)%>%
  summarise(all_count=n())
top_own_non_homeexempt_s1<-non_home_exempts1%>%
  mutate(owner_name=tolower(substr(ownname,1,12)))%>%
  group_by(owner_name,primary_city,owncity,ownstate)%>%
  summarise(all_count=n())
###top landowners survey 2
top_own_homeexempt_s2<-home_exempt_s2%>%
  mutate(owner_name=tolower(substr(ownname,1,12)))%>%
  mutate(own_warrenton=tolower(substr(ownaddress1,1,12)))%>%
  mutate(own_commerce=tolower(substr(ownname1,1,12)))%>%
  group_by(owner_name,primary_city,owncity,ownstate,own_warrenton,own_commerce)%>%
  summarise(all_count=n())
top_own_non_homeexempt_s2<-non_home_exempt_s2%>%
  mutate(own_commerce=tolower(substr(ownname1,1,12)))%>%
  mutate(owner_name=tolower(substr(ownname,1,12)))%>%
  mutate(own_warrenton=tolower(substr(ownaddress1,1,12)))%>%
  group_by(owner_name,primary_city,owncity,ownstate,own_warrenton,own_commerce)%>%
  summarise(all_count=n())
### S1 Analysis
###analysis of mean conditions for home exempt vs non home exempt classify
mean_classify_exempt_s1<-home_exempt_s1%>%
  mutate(mean_classify=mean(classify))%>%
  select(mean_classify)
###1.309742
mean_classify_non_exempts1<-non_home_exempts1%>%
  mutate(classify=as.numeric(classify))%>%
  mutate(mean_classify=mean(classify))%>%
###1.544932
###analysis of mean conditions for home exempt vs non home exempt gen_cond
mean_condition_exempcond<-home_exempt_s1%>%
  mutate(mean_condition=mean(gen_cond))
###1.340514
mean_condition_non_exemptcond<-non_home_exempts1%>%
  mutate(mean_condition=mean(gen_cond))
###1.608168
###combine number of property owned with survey data, whether in state or not, and whether in city or not
prop_exempt_s1<-home_exempt_s1%>%
  group_by(ownname,primary_city,owncity,ownstate)%>%
  summarise(prop_count=n())
prop_non_exempt_s1<-non_home_exempts1%>%
  group_by(ownname,primary_city,owncity,ownstate)%>%
  summarise(prop_count=n())
comb_home_exempt<-prop_exempt_s1%>%
  left_join(home_exempt_s1)
comb_non_exempt<-prop_non_exempt_s1%>%
  left_join(non_home_exempts1)
filter_top_prop_ne<-comb_non_exempt%>%
  filter(prop_count>1)
analaysis_top_ne<-filter_top_prop_ne%>%
  mutate(classify=as.numeric(classify))%>%
  mutate("mean_condition"=mean(classify))
mean(analaysis_top_ne$classify)

###city and state matching data
owncity<-s1_data %>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) %>%
  right_join(s1_data) %>%
  filter(is.na(owncity)==FALSE)

own_state<-s1_data%>%
  count(ownstate) %>%
  filter(is.na(ownstate)==FALSE) %>%
  mutate(state_match=if_else(ownstate=="GA",0,1)) %>%
  select(ownstate,state_match) %>%
  right_join(s1_data) %>%
  filter(is.na(ownstate)==FALSE)
### city and state matching analysis
analysis_state_here<-own_state%>%
  filter(state_match=='0')%>%
  mutate(classify=as.numeric(classify))%>%
  mutate("mean_condition"=mean(classify))
###1.472163
analysis_state_away<-own_state%>%
  filter(state_match=='1')%>%
  mutate(classify=as.numeric(classify))%>%
  mutate("mean_condition"=mean(classify))
###1.510101
analysis_city_here<-owncity%>%
  filter(citymatch=='0')%>%
  mutate(classify=as.numeric(classify))%>%
  mutate("mean_condition"=mean(classify))
###1.466921
analysis_city_away<-owncity%>%
  filter(citymatch=='1')%>%
  mutate(classify=as.numeric(classify))%>%
  mutate("mean_condition"=mean(classify))
###1.502532

###city and state matching analysis gen_cond
analysis_state_here_cond<-own_state%>%
  filter(state_match=='0')%>%
  mutate(gen_cond=as.numeric(gen_cond))%>%
  mutate("mean_condition"=mean(gen_cond))
###1.515472
analysis_state_away_cond<-own_state%>%
  filter(state_match=='1')%>%
  mutate(classify=as.numeric(gen_cond))%>%
  mutate("mean_condition"=mean(gen_cond))
###1.675676
analysis_city_here_cond<-owncity%>%
  filter(citymatch=='0')%>%
  mutate(classify=as.numeric(gen_cond))%>%
  mutate("mean_condition"=mean(gen_cond))
###1.518792
analysis_city_away_cond<-owncity%>%
  filter(citymatch=='1')%>%
  mutate(classify=as.numeric(gen_cond))%>%
  mutate("mean_condition"=mean(gen_cond))
###1.541005
###Analysis top landowners
filter_top_prop_ne<-comb_non_exempt%>%
  filter(prop_count>1)
analaysis_top_ne<-filter_top_prop_ne%>%
  mutate(classify=as.numeric(classify))%>%
  mutate("mean_condition"=mean(classify))
mean(analaysis_top_ne$classify)
###Statistical Tests
shapiro.test(s1_data$classify)
wilcox.test(home_exempt_s1$classify, non_home_exempts1$classify)
shapiro.test(s1_data$gen_cond)
wilcox.test(home_exempt_s1$gen_cond, non_home_exempts1$gen_cond)
### S2 Analysis
library(tidyr) 
###analysis of mean conditions for home exempt vs non home exempt classify 
mean_condition_exempts2<-s2_data%>%
  filter(homeexempt!="S0")%>% 
  filter(primary_city!="warrenton")%>%
  mutate(condition=as.numeric(condition))%>%
  mutate(mean_condition=mean(condition))
###1.866667
non_exempts2_<-non_home_exempt_s2%>%
  filter(homeexempt=="S0")%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) %>%
  right_join(non_home_exempt_s2) %>%
  filter(is.na(owncity)==FALSE) %>%
  filter(primary_city!="warrenton")%>%
  mutate(condition=as.numeric(condition))%>%
  mutate(mean_condition=mean(condition))
###2.521968
###city & State data s2
mean_owncity_s2<-s2_data%>%
  filter(citymatch!="1")%>% 
  filter(primary_city!="warrenton")%>%
  mutate(condition=as.numeric(condition))%>%
  mutate(mean_condition=mean(condition))
###2.101338
mean_away_city_s2<-s2_data%>%
  filter(citymatch!="1")%>% 
  filter(primary_city!="warrenton")%>%
  mutate(condition=as.numeric(condition))%>%
  mutate(mean_condition=mean(condition))
###2.914365 
mean_ownstate_GA_s2<-s2_data%>%
  filter(ownstate=="GA")%>%
  filter(primary_city!="warrenton")%>%
  mutate(condition=as.numeric(condition))%>%
  mutate(mean_condition=mean(condition))
###2.242007
mean_away_state_s2<-s2_data%>%
  filter(ownstate!="GA")%>%
  filter(primary_city!="warrenton")%>%
  mutate(condition=as.numeric(condition))%>%
  mutate(mean_condition=mean(condition))
###2.743243