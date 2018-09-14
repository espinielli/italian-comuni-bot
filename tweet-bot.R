has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}

if (!has_internet()) stop("No internet connection!")


library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(sf)
library(ggmap)
library(raster)
library(rgdal)
library(rtweet)


source("map-utils.R")
source("tweet-comune.R")


coms <- readRDS("data/coms.rds")
lc <- nrow(coms)

tweet_authorize()

# repeat every hour
l <- read_file("last-tweeted.txt") %>% as.integer()
n <- (l + 1) %% lc

# select the next comune to deal with
com <- coms %>%
  dplyr::filter(row_number() == n) %>%
  dplyr::slice(1)


msg <- ifelse((n %% 19) == 0,
              "Done in #rstats using #ggplot2, #rspatial, #ggmap and #rtweet.",
              "")
credits <- ifelse((n %% 67) == 0,
                  "Sources @istat_it, @istat_en, @googlemaps.",
                  "")
tweet_comune(com, n, msg = msg, credits = credits)
about <- str_c("Tweeted about ", com$COMUNE, "; idx=", n)
print(about)
writeLines(text = as.character(n) , "last-tweeted.txt")
