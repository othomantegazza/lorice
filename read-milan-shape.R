# library(maptools)
library(sf)
library(tidyverse)
library(ggmap)
library(leaflet)
library(mapview)

area <- sf::st_read("data/Particelle_agricole_milano.shp") 

# plot(area2)

# save(area, file = "data/parts-milano.Rdata")

geom1 <- area %>%
  filter(nome_comun == "ABBIATEGRASSO") %>%
  pull(geometry)

geom1[[2]] %>% plot()

abg_fields <- 
  area %>%
  filter(nome_comun == "ABBIATEGRASSO") #%>% 
  # ggplot() + 
  # geom_sf()

# abg_fields %>%
#   ggplot() +
#   geom_sf() +
#   coord_sf()
  
# openstreetmaps
abg_path <- "data/abg_map.Rdata" 
if(!file.exists(abg_path)) {
  # portland <- get_map("Portland, Oregon",
  abg_map <- get_map(location = c(8.84, 45.31,
                                   8.97, 45.43),
                      zoom = 13,
                      # source = "stamen",
                      maptype = "toner-lite")
  save(abg_map, file = abg_path)
} else {
  load(abg_path)
}

# This does not work
# Do I have to use sp objects?
ggmap(abg_map) +
  geom_sf(data = abg_fields)



# with leaflet and mapview ------------------------------------------------

mapview(abg_fields)

# area$nome_comun %>% unique()

area %>% 
  filter(nome_comun == "MILANO") %>% 
  mapview()

area %>% 
  filter(nome_comun == "SETTALA") %>% 
  mapview()
