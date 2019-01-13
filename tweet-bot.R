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


source("map-utils.R")
source("tweet-comune.R")


coms <- readRDS("data/coms.rds")
lc <- nrow(coms)

tweet_authorize()
register_google(key = Sys.getenv("google_maps_api_key"))


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
