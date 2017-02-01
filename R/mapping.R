
library(data.table)
d <- fread(
    "data/UStweets.dtbl",
    colClasses = c(created_at = "POSIXt",
                   status_id = "character",
                   text = "character",
                   lat = "numeric",
                   long = "numeric"))
## fix date
devtools::load_all("/Users/mwk/r/rtweet", export_all = TRUE)
d$created_at <- format_date(d$created_at)
nrow(d)


## color regions

##rt <- stream_tweets(c(-180, -90, 180, 90), timeout = 60 * 15)

wc <- which(d$long <= -110 & d$long >= -125)
mt <- which(d$long <= -95 & d$long >= -110)
mw <- which(d$long <= -80 & d$long >= -95)
ec <- which(d$long <= -65 & d$long >= -80)
d$color <- NA_character_
d$color[wc] <- "#FF0000"
d$color[mt] <- "#FF0099"
d$color[mw] <- "#9900FF"
?map
d$color[ec] <- "#0000FF"
d <- d[!is.na(d$color), ]
cols <- c(rep("#ff0000", 5) "#ff0099", "#"))
source("R/animoop.R")
set.seed(12345)
setwd("/Users/mwk/COLLOQUIUM")

png("images/worlddensity.png", 7, 4.5, "in", res = 127.5)
par(mar = c(0, 0, 0, 0))
map("usa", fill = TRUE, col = "#fcfcfc", border = "#fafafa", lwd = .25)
with(d[sample(1:10000, 2500),], points(long, lat, col = "#00000005", pch = 20, cex = .75))
points(d$long, d$lat, col = "#00000003", pch = 20, cex = .15)
dev.off()
animoop

animoop(d, rng = 2500,
        land = "#ffffff",
        ocean = "#eaeaea",
        border = "#333333",
        cex = .3,
        alpha = "33",
        lwd = .15,
        n = 50,
        cols = d$color)
system(
    "convert .imgs/*.png -delay 5 -loop 0 images/usatweets_3color.gif")

str(list(sample(wc, 1000),
         sample(mt, 1000),
         sample(mw, 1000),
         sample(ec, 1000)))
## convert *.png -delay 3 -loop 0 binom.gif
## convert *.png -delay 5 -loop 0 map_ustweets.gif
## convert -delay 200 -loop 0 *.png anim.gif

