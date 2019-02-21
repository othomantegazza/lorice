library(leaflet)
library(mapview)

m <- leaflet() %>% 
  addTiles() %>% 
  leaflet::addPolygons(abg_fields$geometry[[1]])

abg_fields$geometry %>% class() 

mapview(abg_fields)
