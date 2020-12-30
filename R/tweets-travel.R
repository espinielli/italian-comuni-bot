# plot the path of the centroids in the order the comune's are tweeted

# as per Robert Kosara's "The US ZIPScribble Map"
# https://eagereyes.org/zipscribble-maps/united-states
# and in Observable
# https://observablehq.com/@mbostock/zipscribble
# https://observablehq.com/@uwdata/cartographic-visualization

# TODO:
# 1. better legend, i.e. name the region
# 2. diverging color codes (?)
# 3. should be plot ZIP codes rather than comuni's centroids?

library(tidyverse)
library(sf)

make_centroid <- function(com) {
  com %>%
  sf::st_transform(23032) %>%
  sf::st_centroid() %>%
  sf::st_transform(4326) %>%
  sf::st_coordinates() %>%
  as_tibble() %>%
  `names<-`(c("lon", "lat"))
}

istat_year <- 2016
comuni <- "data/Limiti0101{yyyy}/Com0101{yyyy}/Com0101{yyyy}_WGS84.shp" %>%
  str_glue(yyyy = istat_year) %>%
  sf::st_read() %>%
  sf::st_transform(crs = 4326) %>%
  mutate_at(vars(starts_with("COD_"), "PRO_COM"), as_factor) %>%
  mutate(layout = st_geometry(.)) %>%
  st_centroid() %>%
  mutate(centroid = st_geometry(.)) %>%
  mutate(geometry = layout)

# from https://github.com/r-spatial/sf/issues/1179
st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) st_as_sfc(st_bbox(y))
  do.call("c", lapply(x, f))
}
comuni$bbox = st_bbox_by_feature(comuni)

# italy <- ne_countries(country = 'italy', scale = 'medium', returnclass = 'sf')
italy <- sf::st_read(here::here("data", "italy.geojson"))
regions <- sf::st_read(here::here("data", "regions2016.geojson"))


c <- comuni %>%
  mutate(geometry = centroid) %>%
  group_by(COD_REG) %>%
  arrange(PRO_COM) %>%
  summarize(m = 1) %>%
  st_cast("LINESTRING")

g <- ggplot() +
  geom_sf(data = italy, fill = "white", colour = "gray77") +
  geom_sf(data = regions, size = 0.1) +
  geom_sf(data = c, aes(colour = COD_REG), size = 0.15, alpha = 0.7) +
  theme_void()
g
