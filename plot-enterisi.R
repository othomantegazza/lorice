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

# pal <- colorNumeric("viridis", NULL)
pal <- scico::scico(n = 100,
                    palette = "tokyo",
                    direction = -1) %>% colorNumeric(NULL)

# add an html panel
labels <- sprintf(
  "<strong>%s</strong><br/>%g ha",
  rice_shapes$COMUNE, rice_shapes$acri
) %>% lapply(htmltools::HTML)

# color palette breaks
# vals <- rice_shapes$rice_dens %>% quantile(probs = seq(0, 1, 0.2))
vals <- rice_shapes$rice_dens %>% range() %>% {seq(from = .[1], to = .[2], length.out = 5)}
breaks <- vals %>% scales::scientific(2)

leaflet() %>% 
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
                direction = "auto")) %>% 
  addLegend(pal = pal, 
            values =  rice_shapes$rice_dens,
            # bins = vals, labels = breaks,
            labFormat = labelFormat(digits = 6),
            opacity = 0.7, title = NULL,
            position = "bottomright")

# rice_shapes %>% arrange(desc(rice_dens)) %>% head() %>% View()

