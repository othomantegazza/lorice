library(tidyverse)
library(sf)
library(leaflet)
library(mapview)

# Produzione Riso ---------------------------------------------------------

links <- c(Bergamo = "https://www.dati.lombardia.it/api/views/sp2m-8pyv/rows.csv?accessType=DOWNLOAD",
           Milano = "https://www.dati.lombardia.it/api/views/8t7w-8tqw/rows.csv?accessType=DOWNLOAD",
           Pavia = "https://www.dati.lombardia.it/api/views/7n6i-5dyc/rows.csv?accessType=DOWNLOAD",
           Lecco = "https://www.dati.lombardia.it/api/views/gf5m-a45v/rows.csv?accessType=DOWNLOAD",
           Brescia = "https://www.dati.lombardia.it/api/views/usvu-2evr/rows.csv?accessType=DOWNLOAD",
           Mantova = "https://www.dati.lombardia.it/api/views/dk23-emij/rows.csv?accessType=DOWNLOAD",
           Varese = "https://www.dati.lombardia.it/api/views/nkmt-xhn4/rows.csv?accessType=DOWNLOAD",
           Lodi = "https://www.dati.lombardia.it/api/views/hvwv-fgj3/rows.csv?accessType=DOWNLOAD",
           Sondrio = "https://www.dati.lombardia.it/api/views/dnyh-ygvh/rows.csv?accessType=DOWNLOAD",
           Como = "https://www.dati.lombardia.it/api/views/v4dh-ebfc/rows.csv?accessType=DOWNLOAD",
           Cremona = "https://www.dati.lombardia.it/api/views/4kuj-9vhh/rows.csv?accessType=DOWNLOAD",
           Monza = "https://www.dati.lombardia.it/api/views/mche-usbq/rows.csv?accessType=DOWNLOAD")

lom_agri_path <- "data/lom_agri.Rdata"
if(!file.exists(lom_agri_path)) {
lom_agri <-
  links %>% 
  map(read_csv)

save(lom_agri, file = lom_agri_path)
} else {
  load(lom_agri_path)
}

# mil <- read_csv("data/Particelle_agricole_Provincia_di_Milano_dati_generali.csv")
# 
# read_csv("https://www.dati.lombardia.it/api/views/dnyh-ygvh/rows.csv?accessType=DOWNLOAD")
# 
# mil_riso <- 
#   mil %>% 
#   filter(str_detect(UTILIZZO, pattern = "^RISO")) %>% # pull(UTILIZZO) %>% unique()
#   group_by(COMUNE) %>% 
#   summarize(sup_used = sum(SUP_UTILIZZATA))

# Come sono codificate le produzioni di riso?
# riso <- 
#   lom_agri %>%
#   map(~filter(., UTILIZZO %>% 
#                 str_detect("^RISO")) %>% 
#         pull(UTILIZZO) %>% 
#         unique()) 

# riso

lom_riso <- 
  lom_agri %>% 
  reduce(rbind) %>% 
  filter(str_detect(UTILIZZO, pattern = "^RISO")) %>% # pull(UTILIZZO) %>% unique()
  group_by(COMUNE) %>%
  summarize(sup_used = sum(SUP_UTILIZZATA))

lom_riso_catastale <- 
  lom_agri %>% 
  reduce(rbind) %>% 
  filter(str_detect(UTILIZZO, pattern = "^RISO")) %>% # pull(UTILIZZO) %>% unique()
  group_by(COMUNE) %>%
  summarize(sup_used = sum(SUP_CATASTALE))
  
# This object is heavy!
rm(lom_agri)

# shapefile lombardia -----------------------------------------------------

# lom_istat_base <- 
#   sf::st_read("data/Limiti_2016_ED50/Com2016_ED50")

lom_istat <- 
  sf::st_read("data/Limiti_2016_WGS84/Com2016_WGS84") %>% 
  filter(COD_REG == 3) %>% 
  mutate(COMUNE = COMUNE %>%
           as.character() %>% 
           toupper(),
         geometry = geometry %>% st_transform("+init=epsg:4326"))

lom_istat_simple <- 
  sf::st_read("data/Limiti_2016_WGS84_g/Com2016_WGS84_g") %>% 
  filter(COD_REG == 3) %>% 
  mutate(COMUNE = COMUNE %>%
           as.character() %>% 
           toupper(),
         geometry = geometry %>% st_transform("+init=epsg:4326"))
# merge and plot ----------------------------------------------------------

# mil_riso_shape <- 
#   mil_riso %>% 
#   full_join(lom_istat, by = "COMUNE") %>% 
#   mutate(geometry = geometry %>% st_transform("+init=epsg:4326"),
#          rice_dens = sup_used/SHAPE_Area) %>% 
#   as.data.frame() 

lom_riso_shapes <- 
  lom_riso %>% 
  # full_join(lom_istat, by = "COMUNE") %>%
  full_join(lom_istat_simple, by = "COMUNE") %>%
  mutate(rice_dens = sup_used/SHAPE_Area) %>%
  filter(!geometry %>% map_lgl(is.null)) %>% 
  as.data.frame()
  

pal <- colorNumeric("viridis", NULL)

# leaflet() %>% 
#   addTiles() %>% 
#   addPolygons(data = mil_riso_shape$geometry,
#               fillColor = pal(mil_riso_shape$rice_dens),
#               stroke = FALSE,
#               fillOpacity = .7)


leaflet() %>% 
  # addTiles() %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = lom_riso_shapes$geometry,
              fillColor = pal(lom_riso_shapes$rice_dens),
              stroke = TRUE, weight = 1, color = "#2D408F",
              fillOpacity = .7)


# lom_riso_catastale_shapes <- 
#   lom_riso_catastale %>% 
#   # full_join(lom_istat, by = "COMUNE") %>%
#   full_join(lom_istat_simple, by = "COMUNE") %>%
#   mutate(rice_dens = sup_used/SHAPE_Area) %>%
#   filter(!geometry %>% map_lgl(is.null)) %>% 
#   as.data.frame()

# leaflet() %>% 
#   # addTiles() %>%
#   # addProviderTiles(providers$CartoDB.Positron) %>%
#   addProviderTiles(providers$Stamen.Toner) %>%
#   addPolygons(data = lom_riso_catastale_shapes$geometry,
#               fillColor = pal(lom_riso_catastale_shapes$rice_dens),
#               stroke = TRUE, weight = 1, color = "#2D408F",
#               fillOpacity = .7)

# Exploring rice production in Northern Italy with #opendata from Regione Lombardia and the @LeafletJS package in #rstats. Blog post soon 🌾☀️:) https://www.dati.lombardia.it