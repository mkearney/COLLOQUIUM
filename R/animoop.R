animoop <- function(x, n = 50,
                    rng = c(1000, 50000),
                    cols = c("#FF0000", "#0000FF"),
                    cex = c(1, .25),
                    alpha = c("01", "22"),
                    pch = 16,
                    dir = ".imgs",
                    lwd = .5,
                    border = "#11111a",
                    land = "#000000",
                    ocean = "#11111a",
                    w = 6.5,
                    h = 4.25,
                    ...) {
    require(maps)
    ##data(worldMapEnv)
    if (!dir.exists(dir)) {
        dir.create(dir)
    }
    ## run loop
    for (i in seq_len(n)) {
        png(paste0(dir, "/img", i, ".png"),
            w, h, "in", res = 127.5)
        ## plot points
        geomap(x,
               rng = rng,
               cols = cols,
               alpha = alpha,
               cex = cex,
               lwd = lwd,
               land = land,
               border = border,
               ocean = ocean)
        dev.off()
    }
}

mymap <- function(land = "#000000",
                  lwd = .1,
                  border = "#E0E0E0",
                  ocean = "#000013") {
    map("state",
        fill = TRUE,
        col = land,
        lwd = lwd,
        border = border,
        bg  = ocean)
}

mypoints <- function(x, rng,
                     cols = "#ffffff",
                     alpha = "66",
                     cex = .25, pch = 16, ...) {
    if (missing(rng)) {
        rng <- seq_len(nrow(x))
    } else if (is.list(rng)) {
        rng <- unlist(rng)
    } else if (length(rng) == 1) {
        rng <- sample(seq_len(nrow(x)), rng)
    }
    if (any(grepl("color", names(x)))) {
        colors <- x$color[rng] %>%
            as.character()
    } else if (length(cols) > 1) {
        colors <- sample(cols, length(rng), replace = TRUE)
    } else {
        colors <- cols
    }
    points(x$long[rng] + runif(length(rng), -.1, .001),
           x$lat[rng] + runif(length(rng), -.1, .001),
           col = paste0(colors, alpha),
           cex = cex, pch = pch, ...)
}

geomap <- function(x, rng, cols, alpha,
                   cex, n = 1, ...) {
    op <- par(no.readonly = TRUE)
    par(mar = c(0, 0, 0, 0))
    on.exit(par(op))
    mymap(...)
    ##n <- max(vapply(list(cols, alpha, rng, cex), length,
    ##                double(1)), na.rm = TRUE)
    ##if (n > 1) {
    ##    if (length(rng) != n) rng <- rep(rng, n)
    ##    if (length(alpha) != n) alpha <- rep(alpha, n)
    ##    if (length(cex) != n) cex <- rep(cex, n)
    ##    if (length(cols) != n) cols <- rep(cols, n)
    ##}
    for (i in seq_len(n)) {
        mypoints(x, rng, cols, alpha, cex)
        mypoints(x, rng, cols, alpha, cex)
    }
}
