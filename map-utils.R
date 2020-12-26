# Fixed to ggmap:calc_zoom
# see https://github.com/dkahle/ggmap/pull/141
calc_zoom <- function (lon, lat, data, adjust = 0, f = 0.05)
{
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

# from https://gis.stackexchange.com/a/155495/76173
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
