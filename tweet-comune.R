
generate_google_map <- function(com, filename = "comune.jpg") {
  com.sp <- as(com, "Spatial")

  bb <- com$bb[[1]]
  # centroid
  centroid <- com %>%
    st_transform(23032) %>%
    st_centroid() %>%
    st_transform(4326) %>%
    st_coordinates() %>%
    as_tibble() %>%
    `names<-`(c("lon", "lat"))

  mbb <- make_bbox(
    lon = c(bb["xmin"], bb["xmax"]),
    lat = c(bb["ymin"], bb["ymax"]),
    f = 0.1
  )

  z <- calc_zoom(mbb)

  m <- get_map(location = centroid, zoom = z, maptype = "satellite")
  terrain <- ggmap(m)
  com.sp.df <- com.sp %>% fortify

  # ggmap approach without cropping
  # inspired by
  # https://ryanpeek.github.io/2017-11-21-mapping-with-sf-part-3/
  pg <- terrain +
    geom_polygon(
      data = com.sp.df,
      aes(x = long, y = lat),
      fill=NA,
      color="yellow",
      lwd = 0.4, alpha=0.5) +
    labs(x = "Longitude (WGS84)",
         y = "Latitude",
         caption = "Sources: ISTAT (comuni, 2016), Google Maps (satellite)") +
    ggtitle(
      label = str_glue("{comune} ({id})",
                       comune = com$COMUNE,
                       id = str_pad(com$PRO_COM, 6, pad = "0")),
      subtitle = str_c(com$DEN_CMPRO, com$REGIONE, sep = ", "))

  ggsave("comune_ggmap.jpg", plot = pg)
}

generate_cropped_map <- function(com, filename = "comune_raster.jpg") {
  com.sp <- as(com, "Spatial")

  bb <- com$bb[[1]]
  # centroid
  centroid <- com %>%
    sf::st_transform(23032) %>%
    sf::st_centroid() %>%
    sf::st_transform(4326) %>%
    sf::st_coordinates() %>%
    as_tibble() %>%
    `names<-`(c("lon", "lat"))

  mbb <- ggmap::make_bbox(
    lon = c(bb["xmin"], bb["xmax"]),
    lat = c(bb["ymin"], bb["ymax"]),
    f = 0.1
  )

  z <- calc_zoom(mbb)

  m <- ggmap::get_map(location = centroid, zoom = z, maptype = "satellite")
  terrain <- ggmap::ggmap(m)
  com.sp.df <- com.sp %>% fortify

  # crop to boundary
  m.rast <- ggmap_rast(map = m)
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
         caption = "Sources: ISTAT (comuni), Google Maps (satellite)") +
    ggtitle(
      label = str_glue("{comune} ({id})",
                       comune = com$COMUNE,
                       id = str_pad(com$PRO_COM, 6, pad = "0")),
      subtitle = str_c(com$DEN_CMPRO, com$REGIONE, sep = ", "))

  ggsave(filename, plot = pr)
}

tweet_authorize <- function() {
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
}

tweet_comune <- function(com, n, msg = "", credits = "") {
  generate_cropped_map(com)

  # Twitter bot
  sts <- stringr::str_glue("{comune} ({id}), {provreg}.\n\n{msg}\n{credits}",
                  comune = com$COMUNE,
                  id = stringr::str_pad(com$PRO_COM, 6, pad = "0"),
                  provreg = stringr::str_c(com$DEN_CMPRO, com$REGIONE, sep = ", "),
                  txt = msg,
                  credits = credits)
  rtweet::post_tweet(status = sts,
             media = normalizePath("comune_raster.jpg"))
}
