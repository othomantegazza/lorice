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
  mutate(COMUNE = COMUNE %>% str_replace("´", "'")) %>%
  mutate(COMUNE = COMUNE %>% 
           str_replace("E'", "È") %>% 
           str_replace("A'", "À") %>% 
           str_replace("O'", "Ò")) %>% 
  left_join(shapes_istat) %>% 
  filter(!geometry %>% map_lgl(is.null)) %>% 
  mutate(rice_dens = acri/SHAPE_Area) %>% 
  filter(rice_dens > 0)

# missing
# rice_prod_missing <- 
#   rice_prod %>% 
#   filter(!COMUNE %in% shapes_istat$COMUNE)
# 
# apostrofo <- 
#   rice_prod_missing %>% 
#   mutate(COMUNE = COMUNE %>% str_replace("´", "'")) %>% 
#   left_join(shapes_istat) %>% 
#   filter(!geometry %>% map_lgl(is.null))
# 
# rice_prod_missing <- 
#   rice_prod %>% 
#   mutate(COMUNE = COMUNE %>% str_replace("´", "'")) %>% 
#   filter(!COMUNE %in% shapes_istat$COMUNE)
# 
# accento <- 
#   rice_prod_missing %>% 
#   mutate(COMUNE = COMUNE %>% 
#            str_replace("E'", "È") %>% 
#            str_replace("A'", "À") %>% 
#            str_replace("O'", "Ò")) %>% 
#   left_join(shapes_istat) %>% 
#   filter(geometry %>% map_lgl(is.null))

# rice_shapes <- 
#   rice_shapes %>% 
#   bind_rows(apostrofo) %>% 
#   bind_rows(accento)

save(rice_shapes, file = "data/rice-ente-shapes.Rdata")

# visualize ---------------------------------------------------------------

# Tokyo palettes with green shades
pal <- scico::scico(n = 100,
                    palette = "tokyo",
                    direction = -1) %>% colorNumeric(NULL)

# add an html label
labels <- sprintf(
  "<strong>%s</strong><br/>%g ha",
  rice_shapes$COMUNE, rice_shapes$acri
) %>% lapply(htmltools::HTML)


m <- 
  leaflet() %>% 
  setView(lat = 45.30, ln = 8.60, zoom = 10) %>% 
  # addTiles() %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  # addProviderTiles(providers$Stamen.Toner) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = rice_shapes$geometry,
              fillColor = pal(rice_shapes$rice_dens),
              stroke = TRUE, weight = 2,
              # color = "#2D408F",
              color = "black",
              fillOpacity = .8,
              highlight = highlightOptions(
                weight = 5),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) 

# rice_shapes %>% arrange(desc(rice_dens)) %>% head() %>% View()

