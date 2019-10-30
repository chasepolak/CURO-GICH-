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
