# italian-comuni-bot

Work in progress.

A Twitter bot written in R, see https://twitter.com/italiancomuni for actual tweets.

It tweets a map of an Italian comune together with its name, province, and region.

![a tweeted map](comune_rater.jpg "a comune")


Inspired by
* [@everytract](https://twitter.com/everytract)
* [@GVAcartografic](https://twitter.com/GVAcartografic)'s [#Secciócensal](https://twitter.com/hashtag/Secci%C3%B3censal?src=hash)

GIS data from ISTAT (Italian National Institute of Statistics),
satellite maps from [Google Maps](https://www.google.com/maps).


## ToDo's

* automation: AWS Lambda would be nice but cron will probably do for now
* plot "traveller's map" of centroid of all communi as they will be published.
