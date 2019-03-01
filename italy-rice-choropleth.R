library(tidyverse)
library(sf)
library(leaflet)


# read shapes -------------------------------------------------------------

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

shapes_istat <- 
  paste0(municip_path,
         "/Limiti_2016_WGS84_g/Com2016_WGS84_g/",
         "Com2016_WGS84_g.shp") %>% 
  sf::st_read() %>% 
  mutate(COMUNE = COMUNE %>%
           as.character() %>% 
           toupper(),
         geometry = geometry %>% st_transform("+init=epsg:4326"))


# load rice production ----------------------------------------------------

# I downloaded these data manually
# from https://www.enterisi.it/
load("data/enterisi-riceprod.Rdata")

rice_prod <- 
  rice_prod %>% 
  group_by(COMUNE) %>% 
  summarise(acri = sum(acri))


# merge ------------------------------------------------------------------

rice_shapes <- 
  rice_prod %>% 
  # small fixes to join ISTAT shapefile
  mutate(COMUNE = COMUNE %>% str_replace("´", "'")) %>%
  mutate(COMUNE = COMUNE %>% 
           str_replace("E'", "È") %>% 
           str_replace("A'", "À") %>% 
           str_replace("O'", "Ò")) %>% 
  left_join(shapes_istat) %>% 
  filter(!geometry %>% map_lgl(is.null)) %>% 
  # Turn Acres into square meters
  mutate(m2_surface = acri %>% 
           units::set_units(value = acres) %>% 
           units::set_units(value = m^2)) %>% 
  # because also the shape area is in square meters
  mutate(SHAPE_Area = SHAPE_Area %>% 
           units::set_units(value = m^2)) %>% 
  mutate(rice_dens = m2_surface/SHAPE_Area) %>% 
  # most of municipalities don't grow rice
  filter(units::drop_units(rice_dens)> 0)

save(rice_shapes, file = "data/rice-ente-shapes.Rdata")

# visualize ---------------------------------------------------------------

# Tokyo palettes with green shades
pal <- scico::scico(n = 100,
                    palette = "tokyo",
                    direction = -1) %>% colorNumeric(NULL)

# add an html label
labels <- 
  paste0("<strong>", rice_shapes$COMUNE, "</strong><br/>",
         "rice cultivated area:<br>",
         # Transform m2 to km2
         rice_shapes$m2_surface %>%
           `/`(., 10^6) %>%
           round(2) %>%
           # NA to 0 for the label
           {case_when(rlang::are_na(.) ~ paste(NA_character_, "(none?)"),
                      TRUE ~ paste(., " Km<sup>2</sup>"))}) %>%
  lapply(htmltools::HTML)

# cite sources
attribution <-
  paste0('Map tiles by <a href="http://stamen.com">Stamen Design</a>, ',
         '<a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>',
         ' &mdash; Map data &copy; ',
         '<a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
         ' &mdash; Data &copy; ',
         '<a href="https://www.enterisi.it/servizi/notizie/notizie_homepage.aspx">',
         'Ente Nazionale Risi</a>')


stamen_options <- 
  tileOptions(variant = "toner",
              subdomains = "abcd",
              ext = "png",
              maxZoom = 20)

percent_label <- 
  labelOptions(style = list("font-weight" = "normal",
                            padding = "3px 8px"),
               textsize = "15px",
               direction = "auto")

m <- 
  leaflet() %>% 
  setView(lat = 45.30, ln = 8.60, zoom = 10) %>% 
  addTiles(urlTemplate = "//stamen-tiles-{s}.a.ssl.fastly.net/{variant}/{z}/{x}/{y}.{ext}",
           attribution = attribution,
           options = stamen_options) %>%
  addPolygons(data = rice_shapes$geometry,
              fillColor = pal(rice_shapes$rice_dens %>% units::drop_units()),
              stroke = TRUE, weight = 2,
              color = "black",
              fillOpacity = .8,
              highlight = highlightOptions(weight = 5),
              label = labels,
              labelOptions = percent_label) %>% 
  addLegend(pal = pal,
            values = rice_shapes$rice_dens %>% units::drop_units(),
            labFormat = labelFormat(prefix = "", suffix = "%",
                                    between = ", ",
                                    transform = function(x) {100 * x}),
            title = "Land dedicated<br>to Rice\nProduction",
            position = "bottomright")

m
# rice_shapes %>% arrange(desc(rice_dens)) %>% head() %>% View()

