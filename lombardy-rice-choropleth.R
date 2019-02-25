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
  rm(temp)
  
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

save(lom_rice_shapes, file = "data/lom_rice_shapes.Rdata")

# Leaflet app -------------------------------------------------------------

# Tokyo Palette, It has green colors,
# looks good for agricolture ;)
pal <-
  scico::scico(n = 100,
               palette = "tokyo",
               direction = -1) %>%
  colorNumeric(NULL,
               na.color = "#7A82A6")#"#E0E0D2")

# Useful additiona infos in label:
# municipality and area
labels <- 
  paste0("<strong>", lom_rice_shapes$COMUNE, "</strong><br/>",
         "rice cultivated area:<br>",
         # Transform m2 to km2
         lom_rice_shapes$sup_used %>%
           `/`(., 10^6) %>%
           round(2), #%>%
           # NA to 0 for the label
           # {case_when(rlang::are_na(.) ~ 0,
           #            TRUE ~ .)},
           # Lable
         " Km<sup>2</sup>") %>%
  lapply(htmltools::HTML)

attribution <-
  paste0('Map tiles by <a href="http://stamen.com">Stamen Design</a>, ',
         '<a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>',
         ' &mdash; Map data &copy; ',
         '<a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
         ' &mdash; Data by ',
         '<a href="https://www.dati.lombardia.it/">Regione Lombardia</a>, ',
         '<a href="https://www.dati.gov.it/content/italian-open-data-license-v20">IODL v2.0</a>')

stamen_options <- 
  tileOptions(variant = "toner",
              subdomains = "abcd",
              ext = "png",
              maxZoom = 20)
m_lom <- 
  leaflet() %>% 
  setView(lat = 45.6, ln = 9.7, zoom = 9) %>% 
  addTiles(urlTemplate = "//stamen-tiles-{s}.a.ssl.fastly.net/{variant}/{z}/{x}/{y}.{ext}",
           attribution = attribution,
           options = stamen_options) %>%
  addPolygons(data = lom_rice_shapes$geometry,
              fillColor = pal(lom_rice_shapes$rice_dens),
              stroke = TRUE,
              weight = 1,
              color = "#2D408F",
              fillOpacity = .8,
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal",
                             padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal,
            values = lom_rice_shapes$rice_dens,
            labFormat = labelFormat(prefix = "", suffix = "%",
                                    between = ", ",
                                    transform = function(x) {100 * x}),
            title = "Land dedicated<br>to Rice\nProduction",
            position = "bottomright")

m_lom
# Use this in css rules to specify one column
# In the legend of leaflet (addLegend)
# div.info.legend.leaflet-control br {clear: both;}


