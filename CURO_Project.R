library(tidyverse)
library(readxl)

#Load variable lists
s2_varlist<-read_excel("data/s1_2_crosswalk.xlsx",sheet="s2_vars")
s1_varlist<-read_excel("data/s1_2_crosswalk.xlsx",sheet="s1_vars")

###Read in Data
record_times<-read_csv("data/record_times_all.csv") %>%
  group_by(parcel_num,primary_city) %>%
  summarise(record_id=first(record_id),
            date=first(date))
commerce_issues <- read_csv("data/commerce_parcel_points.csv")
commerce_owner <- read_excel("data/commerce owner.xlsx")
commerce_proprty <- read_excel("data/commerce proprty.xlsx")
Gainesville_issues_and_info <- read_excel("data/Gainesville issues and info.xlsx")
hartwell_issues_and_owner_correct_version <- read_excel("data/hartwell issues and owner correct version.xlsx")
millen_info_with_parcel_number <- read_excel("data/millen info with parcel number.xlsx")
millen_issues <- read_csv("data/millen_surveydata_rev_2017_06_27.csv")
millen_owner <- read_excel("data/millen_owner.xlsx")
millen_home_exempt <- read_csv("data/millen home exempt.csv")%>%
  select("PARCEL_NO","HOMEEXEMPT")%>%
  rename(Parcel_No=PARCEL_NO)
monroe_info <- read_excel("data/monroe info.xlsx")
monroe_issues <- read_csv("data/Monroedata_2017_08_25_combined.csv") %>%
  rename(Parcel_No=Parcel_ID)

warrenton_information <- read_csv("data/warrenton_information1.csv")
warrenton_issues1 <- read_csv("data/parcel_points_issue_warrenton.csv") %>%
  mutate(dummy=1) 
warrenton_issues2<-warrenton_issues1 %>%
  select(fulcrum_id:photo4) %>%
  distinct
warrenton_issues<-warrenton_issues1 %>%
  select(fulcrum_id,value,dummy) %>%
  distinct() %>%
  spread(value,dummy,fill=0) %>%
  right_join(warrenton_issues2)

cochran_issues1 <- read_csv("data/parcel_points_issue_cochran.csv") %>%
  mutate(dummy=1) 
cochran_issues2<-cochran_issues1 %>%
  select(fulcrum_id:photo4) %>%
  distinct
cochran_issues<-cochran_issues1 %>%
  select(fulcrum_id,value,dummy) %>%
  distinct() %>%
  spread(value,dummy,fill=0) %>%
  right_join(cochran_issues2)
Cochran_info1 <- read_excel("data/Cochran info.xlsx")
Cochran_info<-read_csv("data/cochran_parcelpoints_2018_07_13.csv") %>%
  select(Ownkey,Parcel_No) %>%
  distinct() %>%
  rename(ownkey=Ownkey) %>%
  right_join(Cochran_info1)
cochran_home_exempt <- read_csv("data/cochran home exempt.csv")%>%
  select("PARCEL_NO","HOMEEXEMPT")%>%
  rename(Parcel_No=PARCEL_NO)
### Combine Commerce Data
commerce_owner_property<-commerce_owner%>%
  left_join(commerce_proprty)
commerce_issues_info<-commerce_issues%>%
  left_join(commerce_owner_property)
commerce_info_issues<-commerce_issues_info%>%
  mutate("primary_city"="Commerce",
         realkey=as.character(realkey))
### Combine Gainesville data
Gainesville_issues_and_info <- read_excel("data/Gainesville issues and info.xlsx") %>%
  mutate(classify=case_when(min_total>3|maj_total>1~"Dilapidated",
                            min_total>1|maj_total>0~"Substandard",
                            min_total<2|maj_total<1~"Standard"))

gainesville_latlong<-read_csv("data/gville_points.csv") %>%
  select(PARCEL_NUM,long,lat) %>%
  rename(parcel_num=PARCEL_NUM)
gainesville_info_issues<-Gainesville_issues_and_info%>%
  mutate("primary_city"="gainesville") %>%
  left_join(gainesville_latlong)
###Combine hartwell
hartwell_info_issues_new<-hartwell_issues_and_owner_correct_version
hartwell_info_issues<-hartwell_info_issues_new%>%
  mutate("primary_city"="hartwell")%>%
  rename(condition_1=condition)%>%
  rename(condition=condition__1)
  
###Combine millen
millen_info_issues<-millen_issues %>%
  rename(Parcel_No=Parcel_ID) %>%
  left_join(millen_info_with_parcel_number) %>%
  left_join(millen_owner)%>%
  #left_join(millen_info) %>%
  mutate("primary_city"="millen")%>%
  left_join(millen_home_exempt)
millen_home_exempt_fixed<-millen_home_exempt%>%
  rename(parcel_num=Parcel_No)%>%
  rename(homeexempt=HOMEEXEMPT)
###Combine monroe
monroe_info_issues<-monroe_issues%>%
  left_join(monroe_info,by="Parcel_No") %>%
  mutate("primary_city"="monroe",
         address=if_else(address.x=="No",address.y,address.x)) %>%
  select(-address.x,-address.y)
###Combine cochran
cochran_info_issues<-cochran_issues%>%
  rename(Parcel_No=parcel_num) %>%
  left_join(Cochran_info) %>%
  mutate("primary_city"="cochran")%>%
  left_join(cochran_home_exempt)
cochran_exempt_fixed<-cochran_home_exempt%>%
  rename(parcel_num=Parcel_No)%>%
  rename(homeexempt=HOMEEXEMPT)
###Combine Warrenton, need help on fizxing excel
warrenton_spacer<-substr(warrenton_issues[3,]$parcel_num,4,7)
warrenton_issues<-warrenton_issues %>%
  rename(parcelno_old=parcel_num) %>%
  mutate(Parcel_No=gsub(warrenton_spacer," ",parcelno_old))
#head(warrenton_issues$Parcel_No)
warrenton_information <- warrenton_information %>%
  select(-Parcel_No) %>%
  rename(Parcel_No=Parcel_Num)
warrenton_info_issues<-warrenton_issues%>%
  left_join(warrenton_information) %>%
  mutate(primary_city="warrenton")

#Create variable cross tab
names_list<-function(df,var){
  df1<-as_tibble(names(df)) %>%
    mutate(varname=1) 
  names(df1)<-c("value",var)
  df1
}

millen_names<-names_list(millen_info_issues,"millen")
gainesville_names<-names_list(gainesville_info_issues,"gville") 
hartwell_names<-names_list(hartwell_info_issues,"hartwell")
cochran_names<-names_list(cochran_info_issues,"cochran")
commerce_names<-names_list(commerce_info_issues,"commerce")
monroe_names<-names_list(monroe_info_issues,"monroe")
warrenton_names<-names_list(warrenton_info_issues,"warrenton")

var_s1<-millen_names %>%
  full_join(gainesville_names) %>%
  full_join(monroe_names)

var_s2<-hartwell_names %>%
  full_join(cochran_names) %>%
  full_join(commerce_names) %>%
  full_join(warrenton_names)

# write_csv(var_s1,"data/var_s1.csv")
# write_csv(var_s2,"data/var_s22.csv")

##Create Survey 1 data
s1_cw<-read_csv("data/var_s1.csv")

millen_info_issues$spreadid<-1:nrow(millen_info_issues)
millen_data_cw<-millen_info_issues %>%
  gather(-spreadid,key="var",value="value") %>%
  right_join(s1_cw %>% filter(millen==1)) %>%
  select(-var,-millen:-monroe) %>%
  spread(universal,value)%>%
  left_join(millen_home_exempt_fixed)

gainesville_info_issues$spreadid<-1:nrow(gainesville_info_issues)
gainesville_data_cw<-gainesville_info_issues %>%
  gather(-spreadid,key="var",value="value") %>%
  right_join(s1_cw %>% filter(gville==1)) %>%
  select(-var,-millen:-monroe) %>%
  spread(universal,value) %>%
  select(-spreadid)

monroe_info_issues$spreadid<-1:nrow(monroe_info_issues)
monroe_data_cw<-monroe_info_issues %>%
  select(-x,-y) %>%
  gather(-spreadid,key="var",value="value") %>%
  right_join(s1_cw %>% filter(monroe==1)) %>%
  select(-var,-millen:-monroe) %>%
  filter(is.na(spreadid)==FALSE) %>%
  filter(is.na(value)==FALSE) %>%
  spread(universal,value) 

s1_data<-bind_rows(monroe_data_cw,millen_data_cw,gainesville_data_cw) %>%
  mutate(survey="v1") %>%
  left_join(record_times) %>%
  mutate(prop_add=case_when(add_rev=="No"~address,
                            is.na(add_rev)==TRUE~address,
                            add_rev=="Unknown"~address,
                            TRUE~add_rev)) %>%
  select(s1_varlist$s1_vars)

#Create numeric dummies for s1 data
s1_data_num<-s1_data %>% 
  gather(c(min_chimn:min_rot,min_unfinw:maj_siding,maj_winddo,
           yard_grcov:lit_yard),key=issue,value=value) %>%
  mutate(value1=if_else(value=="Yes",1,0)) %>%
  select(-value) %>%
  distinct() %>%
  spread(issue,value1) %>%
  select(s1_varlist$s1_vars)

write_csv(s1_data_num,"s1_data.csv")


#Version 2 data
s2_cw<-read_csv("data/var_s2.csv")

hartwell_info_issues$spreadid<-1:nrow(hartwell_info_issues)
hartwell_data_cw<-hartwell_info_issues %>%
  gather(-spreadid,key="var",value="value") %>%
  right_join(s2_cw %>% filter(hartwell==1)) %>%
  select(-var,-hartwell:-warrenton) %>%
  spread(universal,value) %>%
  select(-spreadid)

cochran_info_issues$spreadid<-1:nrow(cochran_info_issues)
cochran_data_cw<-cochran_info_issues %>%
  gather(-spreadid,key="var",value="value") %>%
  right_join(s2_cw %>% filter(cochran==1)) %>%
  select(-var,-hartwell:-warrenton) %>%
  spread(universal,value) %>%
  select(-spreadid)%>%
  left_join(cochran_exempt_fixed)

commerce_info_issues$spreadid<-1:nrow(commerce_info_issues)
commerce_data_cw<-commerce_info_issues %>%
  gather(-spreadid,key="var",value="value") %>%
  right_join(s2_cw %>% filter(commerce==1)) %>%
  select(-var,-hartwell:-warrenton) %>%
  spread(universal,value) %>%
  select(-spreadid)

warrenton_info_issues$spreadid<-1:nrow(warrenton_info_issues)
warrenton_data_cw<-warrenton_info_issues %>%
  gather(-spreadid,key="var",value="value") %>%
  right_join(s2_cw %>% filter(warrenton==1)) %>%
  select(-var,-hartwell:-warrenton) %>%
  spread(universal,value) %>%
  select(-spreadid)



#Combine s2 data
s2_data<-bind_rows(hartwell_data_cw,cochran_data_cw,commerce_data_cw,warrenton_data_cw) %>%
  filter(condition!="NA")%>%
  count(primary_city,owncity) %>%
  filter(is.na(owncity)==FALSE) %>%
  mutate(owncity1=tolower(owncity),
         primcity1=tolower(primary_city),
         citymatch=if_else(owncity1==primcity1,0,1)) %>%
  select(primary_city,owncity,citymatch) %>%
  right_join(s2_data) %>%
  filter(is.na(owncity)==FALSE)
###this part not working when added
  mutate(survey="v2") %>%
  left_join(record_times) %>%
  mutate(prop_add=if_else(is.na(address_add)==TRUE,prop_add,address_add)) %>%
  select(s2_varlist$s2_vars) 



#transform issue dummies to 0/1
s2_data<-s2_data %>%
  gather(found_compreplace:roof_shingles,key=issue,value=value) %>%
  mutate(value1=if_else(value>0,1,0)) %>%
  select(-value) %>%
  spread(issue,value1) %>%
  gather(genprop_forsale:yard_weeds,key=issue,value=value) %>%
  mutate(value1=if_else(value==TRUE,1,0)) %>%
  select(-value) %>%
  spread(issue,value1) %>%
  select(s2_varlist$s2_vars) 
write_csv(s2_data,"s2_data.csv")

allsurvey_data<-bind_rows(s1_data_num %>% mutate(ownzip=as.character(ownzip)),
                          s2_data %>% mutate(x=as.character(x),y=as.character(y))) 
write_csv(allsurvey_data,"allsurvey_data.csv")

