library(tidyverse)
library(sf)
library(leaflet)

# MAKE A DIRECTORY NAMED DATA to STORE DATA!!!!!

# Rice Production - Download files ---------------------------------------

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

# Merge production datasets ----------------------------------------------------------

# this dataset stores the surface dedicated to rice production
# by municipality
lom_rice <- 
  lom_agri %>% 
  reduce(rbind) %>% 
  filter(str_detect(UTILIZZO, pattern = "^RISO")) %>% # pull(UTILIZZO) %>% unique()
  group_by(COMUNE) %>%
  summarize(sup_used = sum(SUP_UTILIZZATA))

# This object is heavy!
rm(lom_agri)


# Get Shapefile for municipalities ----------------------------------------

link_istat <- paste0("http://www.istat.it/storage/cartografia/",
                     "confini_amministrativi/archivio-confini/",
                     "generalizzati/2016/Limiti_2016_WGS84_g.zip")


municip_path <- "data/istat_municipalities"

if(!file.exists(municip_path)) {
  
  temp <- tempfile()

  link_istat %>% 
    download.file(destfile = temp)
  
  temp %>% unzip(exdir = municip_path)
  rm(temp, temp2)
  
}

lombardy_shapes <- 
  paste0(municip_path,
         "/Limiti_2016_WGS84_g/Com2016_WGS84_g/",
         "Com2016_WGS84_g.shp") %>% 
  sf::st_read() %>% 
  # the regional code for Lombardia is 3
  filter(COD_REG == 3) %>% 
  # Need municipality names in upper case
  # for the joining
  mutate(COMUNE = COMUNE %>%
           as.character() %>% 
           toupper(),
         # This is for plot compatibility with openstreetmap
         geometry = geometry %>% st_transform("+init=epsg:4326"))


# Join shapefile and data -------------------------------------------------

lom_rice_shapes <- 
  lom_rice %>% 
  # full_join(lom_istat, by = "COMUNE") %>%
  full_join(lombardy_shapes, by = "COMUNE") %>%
  # In map, better plot densities
  mutate(rice_dens = sup_used/SHAPE_Area) %>%
  # exclude municipalities that did't match
  # if any
  filter(!geometry %>% map_lgl(is.null)) %>% 
  # estimate area in km2 from shape
  # the area in the dataset have no units
  # I'm using this column to guess that they are
  # in square meters
  mutate(shape_area_km2 = geometry %>%
           sf::st_area() %>% 
           units::set_units(value = km^2)) %>% 
  as.data.frame()
         

# Leaflet app -------------------------------------------------------------

# pal <- colorNumeric("viridis", NULL)

pal <-
  scico::scico(n = 100,
               palette = "tokyo",
               direction = -1) %>%
  colorNumeric(NULL,
               na.color = "#7A82A6")#"#E0E0D2")

leaflet() %>% 
  # addTiles() %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = lom_rice_shapes$geometry,
              fillColor = pal(lom_rice_shapes$rice_dens),
              stroke = TRUE,
              weight = 1,
              color = "#2D408F",
              fillOpacity = .8)



