##County map of rural landlords
library(tidyverse)
library(sf)
library(tidycensus)
library(tmap)

v17 <- load_variables(2017, "acs5", cache = TRUE)
vars_sel<-c("B25032_001","B25032_013")
cty_rent<-get_acs(geography="county",state="GA",var=vars_sel,geometry=TRUE)
cty_rate<-cty_rent %>%
  select(GEOID,variable,estimate) %>%
  spread(variable,estimate) %>%
  mutate(cty_rate=B25032_013/B25032_001*100)

communitypoints<-read_csv("Community table.csv") %>%
  filter(City!="Rockmart") %>%
  st_as_sf(coords=c("X1","Y1"),crs=4326)

tm_shape(cty_rate)+
  tm_polygons("cty_rate",style="jenks",title="% Renter occupied") +
  tm_layout(legend.outside=TRUE)+
tm_shape(communitypoints)+
  tm_bubbles(size=0.2,col="black")+
  tm_text("Pseudonym",shadow=TRUE,auto.placement = TRUE,
          bg.color="white",bg.alpha=0.6)
