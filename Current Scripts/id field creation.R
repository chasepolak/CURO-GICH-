survey_2 <- read_excel("surveys/survey_2.xlsx")
survey_1 <- read_excel("surveys/survey_1.xlsx")
revised_all_survey <- read_excel("surveys/revised_all_survey.xlsx")
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
library(stdlib)
install.packages("stringr")
library("stringr")
String1 = revised_all_survey$ownname
String2 = "LLC"
String1.contains(String2);
###flagging LLC data for revised_all_survey

revised_all_survey<- cbind(LLC.df,revised_all_survey)

LLC_data<-str_detect(survey_1$ownname, pattern = fixed("LLC"))
str(LLC)
survey_1$ownname[LLC]
LLC.df<-data.frame(LLC)
###flagging LLC data for survey_1
LLC<-str_detect(survey_1$ownname, pattern = fixed("LLC"))
str(LLC)
survey_1$ownname[LLC]
LLC.df<-data.frame(LLC)

survey_1<- cbind(LLC.df,survey_1)
###flagging LLC data for survey_2

LLC<-str_detect(survey_2$ownname, pattern = fixed("LLC"))
str(LLC)
survey_2$ownname[LLC]
LLC.df<-data.frame(LLC)
survey_2<- cbind(LLC.df,survey_2)

write.xlsx(revised_all_survey,"surveys/revised_all_survey.xlsx")
write.xlsx(survey_1,"surveys/survey_1.xlsx")
write.xlsx(survey_2,"surveys/survey_2.xlsx")
