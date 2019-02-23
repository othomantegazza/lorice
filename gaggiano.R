library(tidyverse)
library(sf)
library(leaflet)


# get data ----------------------------------------------------------------

# lombardia dati actually has an API that you can use to 
# download data systematically, but I did't have time
# to learn how to use it

milan_shapes_path <- "data/milan-shapes.Rdata"

if(!file.exists(milan_shapes_path)) {
  
  temp <- tempfile()
  temp2 <- tempfile()
  
  paste0("https://www.dati.lombardia.it/api/views/",
         "68w7-gci9/files/18177450-9392-409c-a7e8-b2d1fa399383",
         "?filename=particelle_agricole_milano.zip") %>% 
    download.file(destfile = temp)
  
  temp <- temp %>% unzip(exdir = temp2)
  milan_shapes <- temp2 %>% sf::st_read()
  
  save(milan_shapes, file = milan_shapes_path)
  rm(temp, temp2)
  
} else {
  
  load(milan_shapes_path)

}

# Tidy data ---------------------------------------------------------------

gaggiano <- milan_shapes %>%
  filter(nome_comun == "GAGGIANO") %>% 
  mutate(geometry = geometry %>% st_transform("+init=epsg:4326"))



# launch a leaflet app -------------------------------------------------------------

leaflet() %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.Terrain) %>%
  addPolygons(data = gaggiano$geometry,
              # some style, feel free to modify it
              stroke = TRUE, weight = 5,
              fillColor =  "#00BB29", #"#A0CF9A", #"#46AEF9",
              color = "#D9F6FF", #"#95BBC6", #"#2D408F",
              fillOpacity = .7)

# Plot --------------------------------------------------------------------

# this is an option to produce maps in ggplot2
# but making screenshots of leaflet just seemed easier

# library(rosm)
# library(ggspatial)
# 
# ggplot() +
#   annotation_map_tile(type = "stamenbw",
#                       zoom = 13) +
#   layer_spatial(data = gaggiano) 
