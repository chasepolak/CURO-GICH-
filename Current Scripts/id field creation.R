survey_add<-survey_1 %>%
  select(ownaddress,owncity) %>%
  distinct() %>%
  mutate(row=row_number()) %>%
  mutate(c_own=paste("C",str_pad(row,4,pad="0"),sep=""))

survey_1 <-survey_1 %>%
  left_join(survey_add)
survey_add_2<-survey_2 %>%
  select(ownaddress,owncity) %>%
  distinct() %>%
  mutate(row=row_number()) %>%
  mutate(c_own=paste("C",str_pad(row,4,pad="0"),sep=""))
survey_2 <-survey_2 %>%
  left_join(survey_add_2)
survey_add_all<-revised_all_survey %>%
  select(ownaddress,owncity) %>%
  distinct() %>%
  mutate(row=row_number()) %>%
  mutate(c_own=paste("C",str_pad(row,4,pad="0"),sep=""))
survey_add_all <-revised_all_survey %>%
  left_join(survey_add_all)
install.packages("stdlib")
library("stdlib")
String1 = revised_all_survey$ownname
String2 = "LLC"
String1.contains(String2);

write.xlsx(revised_all_survey,"surveys/revised_all_survey.xlsx")
write.xlsx(survey_1,"surveys/survey_1.xlsx")
write.xlsx(survey_2,"surveys/survey_2.xlsx")
