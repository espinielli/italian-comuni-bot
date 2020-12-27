library(dplyr)
library(stringr)
library(purrr)
library(sf)

download_istat <- FALSE

if (download_istat) {
  # download the relevant shapefile from ISTAT https://www.istat.it/it/archivio/124086
  #    http://www.istat.it/storage/cartografia/confini_amministrativi/archivio-confini/non_generalizzati/2016/Limiti_2016_WGS84.zip
  # reviewed in 20201225:
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
  unzip(istat_destination, exdir = "data/cucu/")
}

regioni <- sf::st_read("data/Limiti_2016_WGS84/Reg2016_WGS84/Reg_2016_WGS84.shp") %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::mutate(bb = split(., 1:nrow(.)) %>% purrr::map(st_bbox))

rr <- regioni %>%
  dplyr::select(COD_REG, REGIONE) %>%
  sf::st_set_geometry(NULL)

province <- st_read("data/Limiti_2016_WGS84/CMProv2016_WGS84/CMprov2016_WGS84.shp") %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::mutate(bb = split(., 1:nrow(.)) %>% purrr::map(st_bbox))

pp <- province %>%
  dplyr::select(COD_REG, COD_PRO, DEN_CMPRO, PROVINCIA, SIGLA) %>%
  sf::st_set_geometry(NULL)

comuni <- sf::st_read("data/Limiti_2016_WGS84/Com2016_WGS84/Com2016_WGS84.shp") %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::mutate(bb = split(., 1:nrow(.)) %>% purrr::map(st_bbox))

coms <- comuni %>%
  dplyr::left_join(rr) %>%
  dplyr::left_join(pp) %>%
  dplyr::select(-FLAG_CM, -NOME_TED, -starts_with("SHAPE")) %>%
  dplyr::arrange(PRO_COM)

saveRDS(coms, "data/coms.rds")
