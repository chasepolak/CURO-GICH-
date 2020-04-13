library(tidyverse)
library(sf)
bbox<-st_as_sfc(st_bbox(millen_as_sf,crs=4326))
samplegrid<-st_make_grid(bbox,cellsize=0.004,square=FALSE) %>%
  st_sf() %>%
  rownames_to_column(var="gridid")
plot(samplegrid)

library(tmap)
tm_shape(samplegrid)+
  tm_borders()+
tm_shape(millen_as_sf)+
  tm_dots()

point_grid<-millen_as_sf %>%
  st_join(samplegrid,join=st_within)

point_tally<-point_grid %>%
  st_set_geometry(NULL) %>%
  group_by(gridid,landlord) %>%
  summarise(count=n()) %>%
  spread(landlord,count,fill=0)
grid_count<-samplegrid %>%
  left_join(point_tally)
