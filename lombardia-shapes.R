library(tidyverse)
library(sf)
library(mapview)

lom <- sf::st_read("data/Regione_10000_CT10_polygon")
lom_2 <- sf::st_read("data/Regione_10000_CT10_polygon/Regione_10000_CT10_polygon.shp")

mapview(lom)

lom_3 <- sf::st_read("data/Ambiti_amministrativi_comunali_polygon/Ambiti_amministrativi_comunali_polygon.shp")

mapview(lom_3)

# lom_4 <- sf::st_read("data/Ambiti_amministrativi_comunali_polygon/Ambiti_amministrativi_comunali_polygon.dbf")

lom_5 <- sf::st_read("data/Limiti_2016_ED50/Com2016_ED50")

lom_5 %>% 
  filter(COD_REG == 3) %>% 
  mapview()
