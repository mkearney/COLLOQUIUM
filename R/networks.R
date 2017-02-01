library(igraph)
## users and elites data
d <- readr::read_rds("/Users/mwk/mizzou/wrangled/sample.rds")
d <- d[, c(1, 2, 3, 4, 5, 8:10, 13, 16)]
nope <- vapply(d$friends, length, double(1)) < 2
d <- d[!nope, ]
##d <- subset(d, date == as.Date("2016-06-13") &
##            group %in% c("d", "r"))
d <- subset(d, date == as.Date("2016-11-15") &
            group %in% c("d", "r"))

## read elites data
e <- readr::read_rds("/Users/mwk/mizzou/wrangled/elites.tbl.v2.rds")
##e <- subset(e, date == as.Date("2016-06-13") &
##               g %in% c("d", "r"))
e <- subset(e, date == as.Date("2016-11-15") &
               g %in% c("d", "r"))
e <- subset(e, int > 50)

## to from matrix
x <- read.csv("data/networks.csv", stringsAsFactors = FALSE)
x[, 1] <- as.character(x[, 1])
x[, 2] <- as.character(x[, 2])
x <- as.matrix(x)
##keep <- table(x[, 1]) %>%
##    sort(decreasing = TRUE) %>%
##    names() %>%
##    .[500:1500]
##keep <- c(
##    sample(keep[keep %in% d$user_id[d$group == "d"]], 50),
##    sample(keep[keep %in% d$user_id[d$group == "r"]], 50))
keep <- readRDS("data/keep.rds")
x <- x[x[, 1] %in% keep, ]

g <- graph_from_edgelist(x[sample(seq_len(nrow(x)),
                                  nrow(x)), 1:2])


## color matching
dmatch <- data.frame(
    'userid' = c(d$user_id, e$user_id),
    'group' = c(d$group, e$g),
    'screen_name' = c(d$user_id, e$screen_name),
    stringsAsFactors = FALSE)
dmatch$col <- "#2244ffaa"
dmatch$col[dmatch$group == "r"] <- "#dd3333aa"
dmatch$border <- "#002266cc"
dmatch$border[dmatch$group == "r"] <- "#661111cc"

cols <- V(g)$name %>%
       match(dmatch$userid) %>%
       dmatch$col[.]
bords <- V(g)$name %>%
       match(dmatch$userid) %>%
       dmatch$border[.]
## network plot




for (i in seq_len(50)) {
png(paste0("docs/images/networkT1", i, ".png"),
    6, 5, "in", res = 127.5)
##set.seed(123456)
##set.seed(23456)
##set.seed(86100827)
set.seed(100 + i)
par(mar = c(1,0,1.45,0), family = "Avenir Next Condensed",
    adj = .325,  cex.main = 1.4)
igraph_options(plot.layout=layout_nicely)
plot(g, vertex.color = cols,
     main = "Twitter network cluster around partisanship",
     edge.width = .4,
     ylim = c(-1, 1.1),
     xlim = c(-1, 1),
     edge.color = "#00000066",
     edge.arrow.size = .075,
     edge.label = NA,
     vertex.size = 6, ##5,
     vertex.label = NA,
     vertex.frame.color = bords)
mtext("Networks of R (red) and D (blue) partisans (N = 1000) on Jun. 13, 2016",
      side = 3, adj = .305, line = -1.1, cex = .95)
dev.off()
}





################ time 2



.getmatrow <- function(x, usr, group) {
    x <- e$user_id[e$user_id %in% x]
    matrix(
        c(rep(usr, length(x)),
          x,
          rep(group, length(x))),
        ncol = 3)
}
getmatrow <- function(x, usr, group) {
    x <- mapply(.getmatrow, x, usr, group)
    do.call("rbind", x)
}


x <- read.csv("data/networks2.csv", stringsAsFactors = FALSE)
x[, 1] <- as.character(x[, 1])
x[, 2] <- as.character(x[, 2])
x <- as.matrix(x)
##.x <- x
x <- x[x[, 1] %in% keep, ]

g <- graph_from_edgelist(x[sample(seq_len(nrow(x)),
                                  nrow(x)), 1:2])


## color matching
dmatch <- data.frame(
    'userid' = c(d$user_id, e$user_id),
    'group' = c(d$group, e$g),
    'screen_name' = c(d$user_id, e$screen_name),
    stringsAsFactors = FALSE)
dmatch$col <- "#2244ffaa"
dmatch$col[dmatch$group == "r"] <- "#dd3333aa"
dmatch$border <- "#002266cc"
dmatch$border[dmatch$group == "r"] <- "#661111cc"

cols <- V(g)$name %>%
       match(dmatch$userid) %>%
       dmatch$col[.]
bords <- V(g)$name %>%
       match(dmatch$userid) %>%
       dmatch$border[.]
## network plot





png(paste0("docs/images/networkT2.", "34b", ".png"),
    6, 5, "in", res = 127.5)
set.seed(102 + 34)
par(mar = c(1,0,1.45,0), family = "Avenir Next Condensed",
    adj = .325,  cex.main = 1.4)
igraph_options(plot.layout=layout_nicely)
plot(g, vertex.color = cols,
     main = "Twitter network cluster around partisanship",
     edge.width = .4,
     ylim = c(-1, 1.1),
     xlim = c(-1, 1),
     edge.color = "#00000066",
     edge.arrow.size = .075,
     edge.label = NA,
     vertex.size = 6, ##5,
     vertex.label = NA,
     vertex.frame.color = bords)
mtext("Networks of R (red) and D (blue) partisans (N = 1000) on Nov. 11, 2016",
      side = 3, adj = .305, line = -1.1, cex = .95)
dev.off()
