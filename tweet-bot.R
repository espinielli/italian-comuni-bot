has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}

if (!has_internet()) stop("No internet connection!")

suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(readr))
suppressMessages(library(stringr))
suppressMessages(library(sf))
suppressMessages(library(ggmap))
suppressMessages(library(raster))
suppressMessages(library(rgdal))
suppressMessages(library(rtweet))
suppressMessages(library(patchwork))
suppressMessages(library(rnaturalearth))

authorize_apps <- function() {
  # Consumer Key (API Key)
  italiancomuni_bot_api_key = Sys.getenv("ITALIANCOMUNI_BOT_API_KEY")
  # Consumer Secret (API Secret)
  italiancomuni_bot_api_key_secret = Sys.getenv("ITALIANCOMUNI_BOT_API_KEY_SECRET")
  # Access Token
  italiancomuni_bot_access_token = Sys.getenv("ITALIANCOMUNI_BOT_ACCESS_TOKEN")
  # Access Token Secret
  italiancomuni_bot_access_token_secret = Sys.getenv("ITALIANCOMUNI_BOT_ACCESS_TOKEN_SECRET")

  twitter_token <- create_token(
    app = "italiancomuni",
    consumer_key = italiancomuni_bot_api_key,
    consumer_secret = italiancomuni_bot_api_key_secret,
    access_token = italiancomuni_bot_access_token,
    access_secret = italiancomuni_bot_access_token_secret
  )

  # Google MAPS
  register_google(key = Sys.getenv("ITALIANCOMUNI_BOT_GOOGLE_MAPS_API_KEY"))
}
next_comune <- function(lc) {
  coms <- readRDS("data/coms.rds")
  lc <- nrow(coms)
  l <- read_file("last-tweeted.txt") %>% as.integer()
  n <- (l + 1) %% lc

  msg <- ifelse((n %% 19) == 0,
                "Done in #rstats using #ggplot2, #rspatial, #ggmap and #rtweet.",
                "")
  credits <- ifelse((n %% 67) == 0,
                    "Sources @istat_it, @istat_en, @googlemaps.",
                    "")

  # select the next Comune to deal with
  com <- coms %>%
    dplyr::filter(row_number() == n) %>%
    dplyr::slice(1) %>%
    mutate(idx = n, msg = msg, credits = credits)
}
calc_zoom_fix <- function (lon, lat, data, adjust = 0, f = 0.05) {
  # Fixed to ggmap:calc_zoom
  # see https://github.com/dkahle/ggmap/pull/141
  if (!missing(adjust))
    stopifnot(is.integer(adjust))
  if (missing(data)) {
    if (missing(lat)) {
      bbox <- lon
      errorString <- "if specifying a bounding box, the format should match that of make_bbox."
      if (length(bbox) != 4)
        stop(errorString, call. = FALSE)
      if (!all(names(bbox) == c("left", "bottom", "right",
                                "top")))
        stop(errorString, call. = FALSE)
      lon_range <- bbox[c("left", "right")]
      lat_range <- bbox[c("bottom", "top")]
    }
    else {
      if (length(lon) != 2 || length(lat) != 2 || !is.numeric(lon) ||
          !is.numeric(lat))
        stop("if specifying ranges, they both must be of length 2 and numeric.")
      lon_range <- sort(lon)
      lat_range <- sort(lat)
    }
  }
  else {
    lon <- data[, deparse(substitute(lon))]
    lat <- data[, deparse(substitute(lat))]
    bbox <- ggmap::make_bbox(lon, lat, f = f)
    lon_range <- bbox[c("left", "right")]
    lat_range <- bbox[c("bottom", "top")]
  }
  lonlength <- diff(lon_range)
  latlength <- diff(lat_range)
  zoomlon <- ceiling(log2(360 * 2/lonlength))
  zoomlat <- ceiling(log2(180 * 2/latlength))
  # FIXED: use min() instead of max() in order to include the whole bbox
  zoom <- min(zoomlon, zoomlat)
  zoom + adjust
}
ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb')
  .extent <- raster::extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster::raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  raster::stack(red,green,blue)
}
generate_cropped_map <- function(com) {
  com.sp <- as_Spatial(com)

  bb <- com$bb[[1]]
  # centroid
  centroid <- com %>%
    sf::st_transform(23032) %>%
    sf::st_centroid() %>%
    sf::st_transform(4326) %>%
    sf::st_coordinates() %>%
    as_tibble() %>%
    `names<-`(c("lon", "lat"))

  mbb <- make_bbox(
    lon = c(bb["xmin"], bb["xmax"]),
    lat = c(bb["ymin"], bb["ymax"]),
    f = 0.1
  )

  z <- calc_zoom_fix(mbb)

  m <- ggmap::get_map(location = centroid, zoom = z, maptype = "satellite")
  terrain <- ggmap::ggmap(m)
  com.sp.df <- com.sp %>% fortify()

  # crop to boundary
  m.rast <- ggmap_rast(map = m)
  # double it: sometimes it fails with:
  #    Error in (function (x)  : attempt to apply non-function
  com.only <- raster::mask(m.rast, com.sp)
  com.only <- raster::mask(m.rast, com.sp)
  cc <- c(mbb["left"] + (mbb["right"] - mbb["left"]) / 2,
          mbb["bottom"] + (mbb["top"] - mbb["bottom"]) / 2) %>%
    `names<-`(NULL) %>%
    round(2)

  # prep raster as a data frame for printing with ggplot
  com.df <- data.frame(rasterToPoints(com.only))

  # p.logo <- ggplot(com.df) +
  #   geom_point(aes(x = x, y = y, col = rgb(layer.1 / 255, layer.2 / 255, layer.3 / 255))) +
  #   scale_color_identity() +
  #   theme_void()
  # ggsave("verona.jpg", plot = p.logo, scale = 0.7)

  centroid <- centroid %>%
    as.numeric() %>%
    round(2)

  p <- ggplot2::ggplot(com.df) +
    ggplot2::geom_point(aes(x = x, y = y,
                            col = rgb(layer.1 / 255, layer.2 / 255, layer.3 / 255))) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_continuous(breaks = c(centroid[1]), labels = function(x) sprintf("%.2f", x)) +
    ggplot2::scale_y_continuous(breaks = c(centroid[2]), labels = function(x) sprintf("%.2f", x)) +
    ggplot2::theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.ticks.length = unit(-0.4, "cm"),
                   axis.text.x = element_text(margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
                   axis.text.y = element_text(margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
                   plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5))

  pr <- p +
    labs(x = "Longitude (WGS84)",
         y = "Latitude",
         caption = "Sources: ISTAT (comuni, 2016), Google Maps (satellite)") +
    ggtitle(
      label = str_glue("{comune} ({id})",
                       comune = com$COMUNE,
                       id = str_pad(com$PRO_COM, 6, pad = "0")),
      subtitle = str_c(com$DEN_CMPRO, com$REGIONE, sep = ", "))
  pr
}
generate_media <- function(com, filename = "comune_raster.jpg") {
  p1 <- generate_cropped_map(com)
  italy <- ne_countries(country = 'italy', scale = 'medium', returnclass = 'sf')
  bbox_comune <- com$bb %>%
    unlist %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_set_crs(4326)

  p2 <- ggplot() +
    geom_sf(data = italy, fill = "white", colour = "gray77") +
    geom_sf(data = bbox_comune, fill = NA, color = "red", size = 1.5) +
    theme_void()
  p <- p1 +
    inset_element(p2,
                  left  = 0, bottom = 0,
                  right = 0.3, top = .3,
                  align_to = 'full')

  ggsave(filename, plot = p)
}

tweet_comune <- function(com) {
  fn <- "comune_raster.jpg"
  generate_media(com, filename = fn)
  # Twitter bot
  sts <- stringr::str_glue(
    "{comune} ({id}), {provreg}.\n\n{msg}\n{credits}",
    comune = com$COMUNE,
    id = stringr::str_pad(com$PRO_COM, 6, pad = "0"),
    provreg = stringr::str_c(com$DEN_CMPRO, com$REGIONE, sep = ", "),
    msg = com$msg,
    credits = com$credits)
  rtweet::post_tweet(status = sts,
                     media = normalizePath(fn))
}


authorize_apps()
com <- next_comune()
tweet_comune(com)
about <- str_c("Tweeted about ", com$COMUNE, "; idx=", n)
print(about)
writeLines(text = as.character(n) , "last-tweeted.txt")
