# plot the path of the centroids in the order the comune's are tweeted

# TODO: finish it

library(tidyverse)
library(sf)
library(ggmap)
library(raster)
library(rgdal)

source("map-utils.R")

coms <- readRDS("data/coms.rds")
c <- coms %>%
  mutate(centroid = split(., 1:nrow(.)) %>% purrr::map(st_centroid))
