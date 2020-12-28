library(dplyr)
library(stringr)
library(purrr)
library(sf)

download_istat <- FALSE
istat_year <- "2020"

if (download_istat) {
  # download the relevant shapefile from ISTAT https://www.istat.it/it/archivio/124086
  #    http://www.istat.it/storage/cartografia/confini_amministrativi/archivio-confini/non_generalizzati/2016/Limiti_2016_WGS84.zip
  # reviewed in 20201225:
  # https://www.istat.it/it/archivio/222527
  # https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/Limiti01012020.zip

  istat_base <- "http://www.istat.it/"
  istat_storage <- "storage/cartografia/confini_amministrativi/"
  istat_type <- "non_generalizzati/"
  istat_year <- "2016"
  istat_filename <- str_glue("Limiti0101{yyyy}.zip", yyyy = istat_year)
  istat_destination <- "data/" %>% str_c(istat_filename)

  istat_file <- str_c(istat_base, istat_storage, istat_type) %>%
    # str_glue("{yyyy}/", yyyy = istat_year) %>%
    str_c(istat_filename)
  download.file(istat_file, istat_destination)
  unzip(istat_destination, exdir = "data")
}

# "data/Limiti_2016_WGS84/Reg2016_WGS84/Reg_2016_WGS84.shp" %>%
#   sf::st_read() %>%
#   sf::st_transform(crs = 4326) %>%
#   dplyr::select(-starts_with("SHAPE")) %>%
#   st_write("data/regions2016.geojson")


regioni <- "data/Limiti0101{yyyy}/Reg0101{yyyy}/Reg0101{yyyy}_WGS84.shp" %>%
  str_glue(yyyy = istat_year) %>%
  sf::st_read() %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::mutate(bb = split(., 1:nrow(.)) %>% purrr::map(st_bbox)) %>%
  dplyr::select(COD_REG, DEN_REG) %>%
  st_write(str_glue("data/regions{yyyy}.geojson", yyyy = istat_year)) %>%
  sf::st_set_geometry(NULL)

province <- "data/Limiti0101{yyyy}/ProvCM0101{yyyy}/ProvCM0101{yyyy}_WGS84.shp" %>%
  str_glue(yyyy = istat_year) %>%
  st_read() %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::mutate(bb = split(., 1:nrow(.)) %>% purrr::map(st_bbox)) %>%
  dplyr::select(COD_REG, COD_PROV, DEN_PCM, DEN_PROV, SIGLA) %>%
  sf::st_set_geometry(NULL)

comuni <- "data/Limiti0101{yyyy}/Com0101{yyyy}/Com0101{yyyy}_WGS84.shp" %>%
  str_glue(yyyy = istat_year) %>%
  sf::st_read() %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::mutate(bb = split(., 1:nrow(.)) %>% purrr::map(st_bbox)) %>%
  dplyr::left_join(regioni) %>%
  dplyr::left_join(province) %>%
  dplyr::select(-starts_with("SHAPE")) %>%
  dplyr::arrange(PRO_COM)

saveRDS(coms, "data/coms.rds")
