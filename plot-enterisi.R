library(tidyverse)
library(sf)
library(leaflet)


# read shapes -------------------------------------------------------------

shapes_istat <- 
  sf::st_read("data/Limiti_2016_WGS84/Com2016_WGS84") %>% 
  mutate(COMUNE = COMUNE %>%
           as.character() %>% 
           toupper(),
         geometry = geometry %>% st_transform("+init=epsg:4326"))


# load rice production ----------------------------------------------------

load("data/enterisi-riceprod.Rdata")

rice_prod <- 
  rice_prod %>% 
  group_by(COMUNE) %>% 
  summarise(acri = sum(acri))


# merge ------------------------------------------------------------------

rice_shapes <- 
  rice_prod %>% 
  left_join(shapes_istat) %>% 
  filter(!geometry %>% map_lgl(is.null)) %>% 
  mutate(rice_dens = acri/SHAPE_Area) %>% 
  filter(rice_dens > 0)


# visualize ---------------------------------------------------------------

# pal <- colorNumeric("viridis", NULL)
pal <- scico::scico(n = 100,
                    palette = "tokyo",
                    direction = -1) %>% colorNumeric(NULL)

# add an html panel
labels <- sprintf(
  "<strong>%s</strong><br/>%g ha",
  rice_shapes$COMUNE, rice_shapes$acri
) %>% lapply(htmltools::HTML)

leaflet() %>% 
  # addTiles() %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
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

