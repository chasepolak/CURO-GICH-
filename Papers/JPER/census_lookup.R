library(tidycensus)
library(tidyverse)

v17 <- load_variables(2017, "acs5", cache = TRUE)

cities_name<-c("Warrenton city, Georgia", "Hartwell city, Georgia",
"Commerce city, Georgia", "Cochran city, Georgia",
"Monroe city, Georgia", "Millen city, Georgia",
"Gainesville city, Georgia")

vars<- read_csv("data/tidycensus_vars.csv")

cities_data <- get_acs(geography = "place", state="GA", variables = vars$variable, year = 2017)

cities_filter<-cities_data %>%
  filter(NAME %in% cities_name) %>%
  left_join(vars) %>%
  select(-moe)

cities_filter_med<-cities_filter %>%
  filter(norm=="-999")

cities_norm<-cities_filter %>%
  filter(norm=="-99") %>%
  select(-norm) %>%
  rename(norm=variable,
         est_norm=estimate,
         name_norm=NAME) %>%
  select(norm,name_norm,est_norm,GEOID)

cities_pct<-cities_filter %>%
  filter(norm != "-999" & norm !="-99") %>%
  left_join(cities_norm) %>%
  rename(est_raw=estimate) %>%
  mutate(estimate=round(est_raw/est_norm*100,0)) %>%
  select(-est_raw,-est_norm) %>%
  rename(NAME=name_norm)

totpop<-cities_norm %>%
  filter(norm=="B01001_001") %>%
  rename(estimate=est_norm,
         NAME=name_norm) 

cities_final<-bind_rows(cities_filter_med,cities_pct,totpop) %>%
  select(-norm,-variable) %>%
  spread(label,estimate)


##Georgia
state_data <- get_acs(geography = "state", state="GA", variables = vars$variable, year = 2017)

state_filter<-state_data %>%
  #filter(NAME %in% state_name) %>%
  left_join(vars) %>%
  select(-moe)

state_filter_med<-state_filter %>%
  filter(norm=="-999")

state_norm<-state_filter %>%
  filter(norm=="-99") %>%
  select(-norm) %>%
  rename(norm=variable,
         est_norm=estimate,
         name_norm=NAME) %>%
  select(norm,name_norm,est_norm,GEOID)

state_pct<-state_filter %>%
  filter(norm != "-999" & norm !="-99") %>%
  left_join(state_norm) %>%
  rename(est_raw=estimate) %>%
  mutate(estimate=round(est_raw/est_norm*100,0)) %>%
  select(-est_raw,-est_norm)

totpop<-state_norm %>%
  filter(norm=="B01001_001") %>%
  rename(estimate=est_norm) 

state_final<-bind_rows(state_filter_med,state_pct,totpop) %>%
  select(-norm,-variable) %>%
  spread(label,estimate)

