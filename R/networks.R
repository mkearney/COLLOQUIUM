
library("rjson")
library("jsonlite")

l <- fromJSON("https://raw.githubusercontent.com/michaeljules/social-network-viz/master/data/data.js",flatten=TRUE)

# l <- fromJSON("/Users/michael/GitHub/social-network-viz/data/data.js",flatten=TRUE)

#####
# Install and Load Libraries
#####

#install.packages("networkD3")
library("networkD3")

#######
# Translate Data to Network Object
#######
library(igraph)
library(networkD3)

x <- read.csv("data/networks.csv", stringsAsFactors = FALSE)
keep <- table(x[, 1]) %>%
    sort(decreasing = TRUE) %>%
    head(1000) %>%
    names() %>%
    sample(50)
##.x <- x
x <- x[x[, 1] %in% keep, ]

g <- graph_from_edgelist(x[, 1:2])
hs <- hub_score(g, weights=NA)$vector
dmatch <- data.frame(
    'userid' = c(d$user_id, e$user_id),
    'group' = c(d$group, e$g),
    stringsAsFactors = FALSE)
dmatch$col <- "#1122ff"
dmatch$col[dmatch$group == "r"] <- "#dd2222"

cols <- V(g)$name %>%
       match(dmatch$userid) %>%
       dmatch$col[.]
png("docs/images/networkanalysis.png", 6, 4.25, "in", res = 127.5)
par(mar = c(1,0,2,0), family = "Avenir Next Condensed", adj = .15,
    cex.main = 1.4)
igraph_options(plot.layout=layout_nicely)
plot(g, vertex.color = cols,
     main = "Network hubs cluster around partisanship",
     mark.border = NA,
     edge.width = .1,
     edge.color = "#666666",
     edge.arrow.size = .05,
     edge.lty = 1,
     edge.label = NA,
     vertex.size = 5, vertex.label = NA)
dev.off()
?par
igraph_options(plot.layout=layout_nicely)


sj_list
cluster_fast_greedy(sj_list)
cwt <- cluster_edge_betweenness(sj_list)
cwt <- cluster_leading_eigen(sj_list)
cwt
igraph(cwt)
memb <- membership(cwt)
library(networkD3)
## Translate to iGraph to D3
g_d3 <- igraph_to_networkD3(sj_list, group = memb)
forceNetwork(Links=g_d3$links,
             Nodes=g_d3$nodes,
             NodeID = 'headline')
?forceNetwork
?cluster_walktrap

attributes(g_d3)
forceNetwork(Links = g_d3$links, Nodes = g_d3$nodes, Source = 'source',
             Target = 'target', NodeID = 'name', Group = 'group',
             zoom = TRUE, linkDistance = 200)
?forceNetwork
forceNetwork(Links=g_d3$links,
             Nodes=g_d3$nodes,
             width=960,
             height=700,
             NodeID = 'headline',
             Group  = 'c3',
             # radiusCalculation = 0,
             radiusCalculation = JS("Math.sqrt(d.nodesize)+5"),
             Nodesize = 'betweenness',
             #linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
             charge = -60,
             linkWidth = .4,
             colourScale = JS("d3.scale.category10()"),
             # linkColour = "rgb(230,159,0,200, maxColorValue=255)",
             linkColour = "goldenrod",
             zoom = FALSE,
             legend = TRUE,
             opacity = .85,
             opacityNoHover = 0,
             #  linkDistance = 10,
             bounded = FALSE,
             fontFamily = "San Francisco",
             fontSize = 14) %>%
  saveNetwork(file = '/Users/michael/GitHub/social-network-viz/html/linkedin-network-viz.html')





#####
# Write Data Object To JSON File
####

library("jsonlite")
library("rjson")
exportJson <- toJSON(g_d3)
write(exportJson, file="/Users/michael/GitHub/social-network-viz/data/output_data.json")












mu <- readr::read_rds("/Users/mwk/mizzou/wrangled/FINALUSERSDF.rds")
library(ggplot2)
head(mu)
options(tibble.width = 300)
mytheme <- theme_minimal() +
    theme(legend.title = element_blank(),
          text = element_text("Avenir Next Condensed", size = 13),
          plot.title = element_text(face = "bold", size = 16),
          legend.position = "none",
          strip.text = element_blank())
theme_set(mytheme)
hist(mu$int.raw %>% log10)
head(mu)
hist(mu$rd.w %>% log10)
head(mu)
hist(abs(mu$rd) %>% log)
with(mu, aggregate(plr, by = list(group = group), mean, na.rm = TRUE))
hist(1/mu$plr[mu$plr < 1.0 & mu$plr > 0.0])
hist(1/log(mu$rd))
mu$polarized <- mu$r.w
mu$polarized[mu$group == "d"] <- mu$d.w[mu$group == "d"]
xx <- (mu$polarized - mean(mu$polarized)) / sd(mu$polarized) - min((mu$polarized - mean(mu$polarized)) / sd(mu$polarized))
xx <- xx / max(xx)
hist(xx)
mu$polarized <- xx
head(mu)

png("docs/images/int2plr.png", 6, 4.25, "in", res = 127.5)
subset(mu, group %in% c("d", "r")) %>%
    ggplot(aes(x = int.adj, y = abs(rd))) +
    geom_point(
        alpha = .1, aes(color = group)) +
    stat_smooth(
        method = "lm", alpha = .25, color = "#444444") +
    scale_color_manual(
        values = c("#0022ff", "#dd1111")) +
    scale_size(range = c(.15, 2)) +
    facet_wrap(
        ~ group, scales = "free_x") +
    theme(
        legend.position = "none",
        text = ggplot2::element_text(family = "Avenir Next Condensed")) +
    labs(
        title = "Political interest and network polarization",
        subtitle = "Real-time data from a random sample (N = 2,000) of Twitter users in 2016",
        x = "Political interest", y = "Network polarization")
dev.off()
names(mu)

png("np.int.png", 900, 900)
ggplot(mu, aes(x = date, y = e, color = group)) +
    geom_smooth(size = 1, method = "lm") +
    labs(title = "Network political interest",
         subtitle = "Number of partisan follow decisions",
         x = "",
         y = expression(f['in']-f['out']))

hist(sqrt(mu$int.raw), breaks = 60, col = "gray")
hist(sqrt(mu$d), breaks = 60, col = "gray")
hist(sqrt(mu$d.w), breaks = 60, col = "gray")
hist(sqrt(mu$e), breaks = 60, col = "gray")

library(gganimate)
names(mu)
mu.lg <- subset(mu, date %in% sort(unique(mu$date))[c(1, 17)])
mu.lg <- tbl(reshape2::melt(mu.lg[, c(12, 15, 4, 5)], id.var = c("user_id", "group"),
                            variable.name = "party"))
mu.lg <- subset(mu.lg, value != 0)
mu.lg$value <- sqrt(mu.lg$value)
mu.lg$group

subset(mu.lg, value < 3 & value > -3)
mu.lg$value[mu.lg$party == "d.w"] <- mu.lg$value[mu.lg$party == "d.w"] * -1
ggplot(mu.lg[mu.lg$group != "e", ], aes(x = value, color = group, fill = group)) +
    geom_density(bw = 5) + ggcolor(dkcols(a)[c(1, 3)]) + ggfill(ltcols(6)[c(1, 3)]) +
    labs(x = "sqrt(Political follows)", y = "sqrt(Entertainment follows)")

ggplot(mu, aes(x = sqrt(int.raw), y = sqrt(e), color = group, fill = group)) +
    geom_jitter(width = .5, height = .5, pch = 21, size = .25, alpha = .33) +
    ggcolor(ltcols()) + ggfill(ltcols()) +
    labs(x = "sqrt(Political follows)", y = "sqrt(Entertainment follows)")



with(mu, plot(n.size, int.raw))

mlm1 <- lme4::lmer(scale(int.raw) ~ I(group == "e") +
                       scale(n.size)+
                       scale(followers_count) +
                       scale(statuses_count)+
                       plr +
                       (1 | user_id),
                   data = mu)
summary(mlm1)

names(mu)

mu1<- mu[mu$date == unique(mu$date)[1], c(1:11, 21, 22, 24, 12, 15)]
mu17<- mu[mu$date == unique(mu$date)[17], c(1:11, 21, 22, 24, 12, 15)]
names(mu1)[1:14] <- paste0(names(mu1)[1:14], "1")
names(mu17)[1:14] <- paste0(names(mu17)[1:14], "17")

fl <- fj(mu1, mu17, by = c("user_id", "group"))
fl$chg.r <- fl$r17 - fl$r1
fl$chg.d <- fl$d17 - fl$d1
fl$chg.e <- fl$e17 - fl$e1
fl$chg.e <- fl$e17 - fl$e1
fl$chg.int.raw <- fl$int.raw17 - fl$int.raw1
fl$chg.int.adj <- fl$int.adj17 - fl$int.adj1
fl$chg.plr <- fl$plr17 - fl$plr1
fl$chg.rd <- fl$rd17 - fl$rd1
fl$chg.plr[1:3]
head(fl)
abs(fl$chg.r) > 200
sum(abs(fl$chg.r) > 50, na.rm = TRUE)
fl

ggplot(fl, aes(x = as.numeric(factor(group)) + runif(NROW(fl), -.4, .4),
               y = chg.rd, fill = group, color = group)) +
    geom_point(height = 0, pch = 21, size = 2.5) +
    ggfill(ltcols(6)) + ggcolor(dkcols(c)) +
    coord_flip() + ylim(-255, 255) +
    labs(y = "D - R follow decisions", x = "D users, E users, R users")

ggplot(fl[abs(fl$chg.rd) < 250, ], aes(x = as.numeric(factor(group)) +
                                           runif(NROW(fl[abs(fl$chg.rd) < 250, ]), -.4, .4),
               y = chg.rd, fill = group, color = group)) +
    geom_point(pch = 21, size = 2.5) +
    ggfill(ltcols(6)) + ggcolor(dkcols(c)) +
    coord_flip() +
    labs(y = "D - R follow decisions", x = "D users, E users, R users")

with(fl, aggregate(chg.int.raw, by = list(group), mean, na.rm = TRUE))
fl[!abs(fl$chg.r) > 10, ]
mlm1 <- lme4::lmer(plr ~ I(group == "e") +
                       n.size +
                       followers_count +
                       statuses_count +
                       plr +
                       (1 | user_id),
                   data = mu)

mu
summary(mlm1)
dev.off()
?glm
?glmer
?family


mu[, c(1:6, 20:23)]
ggplot(mu, aes(x = date, y = abs(rd), color = group)) +
    geom_smooth(size = 1, method = "lm") +
    facet_grid(~ group) +
    ggcolor(ltcols())


mutbl
dd <- data.frame(t(mkdat(d[["friends"]])))
tbl(dd)
df <- data.frame(
    r = unlist(dd[[1]]),
    d = unlist(dd[[2]]),
    e = unlist(dd[[3]]))
df <- tbl(df)
df
reshape2::melt(df[, c(1, 2, 4, 8, 9)], id.var = c(""))
df$d2 <- df$d * -1
df$pi <- df$d2 + df$r
df$pp <- df$rp - df$dp
hist(log(df$pi), breaks = 100)
hist(log10(scale(df$pi)), breaks = 100)
df
ggplot(df[df$group != "e", ], aes(x = pi, color = group, fill = group)) +
##    geom_jitter(pch = 21, alpha = .01) +
##   geom_boxplot(alpha = .6, outlier.shape = NA) +
    geom_density(alpha = .5, bw = 5) +
    ggcolor(dkcols()[c(1, 3)]) + #coord_flip() +
    ggfill(ltcols()[c(1, 3)])

ggplot(df[df$group != "e", ], aes(x = d, y = r, color = group, fill = group)) +
    geom_jitter(pch = 21, alpha = .1) +
##   geom_boxplot(alpha = .6, outlier.shape = NA) +
    ggcolor(dkcols()[c(1, 3)]) + #coord_flip() +
    ggfill(ltcols()[c(1, 3)])
df
df$pint <- df$d + df$r
ggplot(df, aes(x = date, y = abs(pi), color = group, fill = group)) +
    stat_smooth(method = "lm") +
    ggcolor(dkcols()) + #coord_flip() +
    ggfill(ltcols())
ggplot(df, aes(x = date, y = pint, color = group, fill = group)) +
    stat_smooth(method = "lm") +
    ggcolor(dkcols()) + #coord_flip() +
    ggfill(ltcols())


facet_wrap(~ group, nrow = 3)

df
df
ggplot(df, aes(x = rp, y = dp, color = group)) +
    geom_smooth()
df$rp <- df$r / rowSums(df[, 5:6])
df$dp <- df$d / rowSums(df[, 5:6])
df
df <- tbl(cbind(d[, c(1, 2, 4, 5)], df))

with(ee, aggregate(screen_name, by = list(g = g), NROW))

ee <- reshape2::melt(ee, id.var = "screen_name")
ee <- tbl(ee)

ggplot(ee, aes(x = screen_name, y = value, color = variable, fill = variable)) +
           geom_boxplot() + ggcolor(ltcols(6)) + ggfill(dkcols(6))
ee
NROW(ee)


e <- e[e$screen_name %in% c(Rs, Ds, Es), ]
e$group <- NA_character_
e$group[e$screen_name %in% Rs] <- "r"
e$group[e$screen_name %in% Ds] <- "d"
e$group[e$screen_name %in% Es] <- "e"
e[, c("g", "group")]
with(e, table(g, group))
ggplot(ee, aes(x = d, y = r, color = g)) +
    geom_jitter(alpha = .25) + gcolor(cols1)
NROW(ee)

readr::write_rds(e, "/Users/mwk/mizzou/wrangled/eONLY20s.rds")
length(c(Rs, Ds))


ncol(eee)

eee[unlist(apply(eee[, c(1, 3)], 1, function(x) sum(x == 0) < 1)), ]
range(e$int)
unique(e$user_id)
smplr <- function(d) {
    x <- d[["friends"]]
    date <- d[["date"]]
    user_id <- d[["user_id"]]
    if ("group" %in% names(d)) {
        group <- d[["group"]]
    } else {
        group <- d[["g"]]
    }
    df <- d
    rm(d)
    der <- p.id(ee)

    plr <- vector("list", NROW(df))
    dem <- vector("list", NROW(df))
    gop <- vector("list", NROW(df))
    ent <- vector("list", NROW(df))
    for (i in seq_len(NROW(df))) {
        g <- group[[i]]
        plr[[i]] <- NA_real_
        d <- sum(der[[1]] %in%
                 unlist(x[[i]], use.names = FALSE),
                 na.rm = TRUE)
        dem[[i]] <- d
        e <- sum(der[[2]] %in%
                 unlist(x[[i]], use.names = FALSE),
                 na.rm = TRUE)
        ent[[i]] <- e
        r <- sum(der[[3]] %in%
                 unlist(x[[i]], use.names = FALSE),
                 na.rm = TRUE)
        gop[[i]] <- r
        if (identical(sum(d, r), 0L)) {
            plr[[i]] <- NA_real_
        } else if (identical(g, "d")) {
            plr[[i]] <- d / (d + r)
        } else if (identical(g, "r")) {
            plr[[i]] <- r / (d + r)
        } else if (identical(g, "e")) {
            if (all(identical(d, r),
                    runif(1) > .500000)) {
                plr[[i]] <- d / (d + r)
            } else {
                plr[[i]] <- r / (d + r)
            }
        } else if (identical(which.max(c(d, r)), 1)) {
                plr[[i]] <- d / (d + r)
        } else if (identical(which.max(c(d, r)), 2)) {
                plr[[i]] <- r / (d + r)
        }
    }
    dem <- do.call("c", dem)
    gop <- do.call("c", gop)
    ent <- do.call("c", ent)
    plr <- do.call("c", plr)
    data.frame(
        user_id = user_id, date = date, group = group,
        d = dem, r = gop, e = ent, plr = plr)
}

ddd <- sort(unique(d$date))

d <- .d

.d <- d
d
d1 <- subset(d, date == ddd[1])

d1s1 <- lj(d1, s1[, c(1, 3:7)])
with(d1s1, aggregate(g, by = list(group), NROW))
d1s1$g <- as.character(d1s1$g)
names(s1)[3] <- "g"

smplr(d)
s1 <- smplr(d)
head(s1)
tbl(s1)

fcol <- paste0(cols1, "33")
ccol <- paste0(cols2, "66")
ggplot(s, aes(x = d, y = r, fill = group, color = group)) +
    geom_point(pch = 21, size = 4) +
    gcolor(ccol) + gfill(fcol) + theme_bw()
ggplot(s, aes(x = as.factor(date), y = d, color = group, fill = group)) +
    geom_boxplot(outlier.shape = NA) + gcolor(ccol) + gfill(fcol) +
    coord_flip() +
    facet_wrap(~ group, ncol = 1) + theme(axis.text = element_blank())
?ggplot
s <- s1
readr::write_rds(s, "/Users/mwk/mizzou/wrangled/samp.calc.rds")
s$user_id <- as.character(s$user_id)
s$group <- as.character(s$group)
s <- tbl(s)

ggplot(s, aes(x = d, y = r

lsmplr <- function(x, g) lapply(seq_along(x), smplr,
                               function(i) x[i], g[i])
lsmplr(d[["friends"]][1:100], d[["group"]][1:100])

## save elites tbl
readr::write_rds(
    e, "/Users/mwk/mizzou/wrangled/elites.tbl.rds")
dev.off()


et.lg <- reshape2::melt(
    et, id.var = c("user_id", "date", "g"))
et.lg$variable <- as.character(et.lg$variable)
et.lg <- dplyr::tbl_df(et.lg)
et.lg

p <- ggplot(et.lg, aes(x = date, y = value,
                       color = g, frame = date))
p + geom_point()

## unique
## cols
dd <- dplyr::left_join(dd, gelt[, c(1, 3)])
dd
cols <- c("#4455dd", "#dd3333", "yellow")
cols3d <- apply(elt.tbl[, 2:4], 1, which.max)
cols <- cols[cols3d]
saveRDS(cols, "/Users/mwk/mizzou/cols.rds")

elt.lg <- reshape2::melt(
    elt.tbl, id.var = c("user_id", "e"))
library(ggplot2)
elt.lg
elt.tbl
elt.lg <- reshape2::melt(elt.tbl[, 1:3],
                         id.var = "user_id")
ggplot(elt.tbl, aes(x = d, y = r)) +
                   #color = , fill = variable)) +
    geom_jitter(width = 20, height = 20,
                alpha = .4, size = 2,
                pch = 21, fill = "gray") +
    theme_minimal()
gop <- with(elt.tbl, identifyPch(d, r))
identifyPch <- function(x, y = NULL, n = length(x), pch = 19, ...)
     {
         xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
         sel <- rep(FALSE, length(x)); res <- integer(0)
         while(sum(sel) < n) {
             ans <- identify(x[!sel], y[!sel], n = 1, plot = FALSE, ...)
             if(!length(ans)) break
             ans <- which(!sel)[ans]
             points(x[ans], y[ans], pch = pch)
             sel[ans] <- TRUE
             res <- c(res, ans)
         }
         res
     }
dem <- c(90, 43, 492, 313, 736, 290, 225, 599, 232, 176, 308, 717,
         264, 686, 870, 1306, 334, 298, 259, 700, 898, 1211, 1377, 1050)

elt.tbl$dr <- elt.tbl$d - elt.tbl$r
elt.tbl2$dr <- elt.tbl2$d - elt.tbl2$r
elt.tbl <- elt.tbl[order(abs(elt.tbl$dr), decreasing = TRUE), ]
elt.ids <- elt.tbl$user_id[1:1000]
elt.ids



elt.usr <- rtweet::lookup_users(elt.ids[1:100])
elt.usr2 <- rtweet::lookup_users(elt.ids[101:200])
elt.usr <- rbind(elt.usr, elt.usr2)

elt.usr2 <- rtweet::lookup_users(elt.ids[201:300])
elt.usr <- rbind(elt.usr, elt.usr2)

elt.usr2 <- rtweet::lookup_users(elt.ids[301:400])
elt.usr <- rbind(elt.usr, elt.usr2)

elt.usr2 <- rtweet::lookup_users(elt.ids[401:500])
elt.usr <- rbind(elt.usr, elt.usr2)

elt.usr2 <- rtweet::lookup_users(elt.ids[501:600])
elt.usr <- rbind(elt.usr, elt.usr2)

elt.usr2 <- rtweet::lookup_users(elt.ids[601:700])
elt.usr <- rbind(elt.usr, elt.usr2)

elt.usr2 <- rtweet::lookup_users(elt.ids[701:800])
elt.usr <- rbind(elt.usr, elt.usr2)

elt.usr2 <- rtweet::lookup_users(elt.ids[801:900])
elt.usr <- rbind(elt.usr, elt.usr2)

elt.usr2 <- rtweet::lookup_users(elt.ids[901:1000])
elt.usr <- rbind(elt.usr, elt.usr2)
#elt.usr <- dplyr::tbl_df(elt.usr[, 1:3])

elt.pop <- dplyr::left_join(elt.usr[, 1:3], elt.tbl)
elt.pop <- dplyr::tbl_df(elt.pop)
elt.pop <- elt.pop[-c(199:203), ]
elt.pop

elt.pop$ddr <- elt.pop$d / rowSums(elt.pop[, 4:5])
elt.pop$rdr <- elt.pop$r / rowSums(elt.pop[, 4:5])

elt.pop2$ddr <- elt.pop2$d / rowSums(elt.pop2[, 4:5])
elt.pop2$rdr <- elt.pop2$r / rowSums(elt.pop2[, 4:5])

ep <- rbind(elt.pop, elt.pop2)

names(elt.pop)
names(elt.pop2)
elt.usr
n <- 100

names(ep2) <- c("screen_name", "d2", "r2", "ddr2", "rdr2")
ep2 <- elt.pop2[, c(3, 4, 5, 10, 11)]
names(elt.pop2)

epl <- dplyr::full_join(elt.pop, ep2)
names(epl)
epl <- epl[, c(3, 4, 12, 5, 13, 7, 9:11, 14:15)]
data.frame(epl[200:225, ])

which(epl$screen_name == "wikileaks")

ee <-  epl[epl$screen_name %in% c("wikileaks", "TimTebow", "DrJillStein",
                           "carrieunderwood", "melindagates",
                           "lenadunham", "RBReich", "CornelWest",
                           "tomfriedman"), c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11)]
ee <- reshape2::melt(ee[, c(1:5, 8)],
                     id.var = c("screen_name", "g"))
235-197
ee$variable <- as.character(ee$variable)
ee$variable <- factor(ee$variable, levels = c("r", "r2", "d", "d2"))
ep$date

levels(ee$variable) <- c("R\nJune 13", "R\nOct 22\n(+199)",
                         "D\nJune 13", "D\nOct 22\n(+38)")
png("wikileaks.png", 700, 700)
ggplot(subset(ee, screen_name == "wikileaks"),
       aes(x = variable, y = value, fill = variable, color = variable)) +
    geom_bar(stat = "identity", position = "dodge", lwd = .75) +
    theme(legend.position = "none",
          text = element_text(size = 18, family = "Georgia")) +
    scale_color_manual(values = c("#aa1111ee", "#660000ee",
                                  "#0033aaee", "#001166ee")) +
    scale_fill_manual(values = c(
                          "#ff3333ee", "#cc1111ee",
                          "#3366ffee", "#2244ccee")) +
    labs(title = "Partisanship of @wikileaks followers during 2016 election",
         subtitle = "Changes in random sample of partisan followers (N = 3,000)",
         x = "", y = "Number of Followers")
dev.off()
?geom_bar
?scale_fill_manual
ee
    facet_wrap(~ screen_name) +
    scale_fill_manual(values = c("dark red", "red"))

    scale_fill_manual(labels = c(
                          dem = "d2", "d", "r", "r2"),
                          values = c(
                          "blue", "dark blue",
                          "red", "dark red"))

epl$ddr2 <- round(epl$ddr2, 3)

data.frame(epl[order(epl$rdr - epl$rdr2, decreasing = TRUE),
               c(1, 7, 10, 8, 11)])

ggplot(ep, aes(x = date, y = dr)) +
    geom_bar(stat = "identity")


elt.tbl

ggplot(elt.pop, aes(x = date, y = dr)) + geom_bar(stat= "identity")

elt.pop[order(elt.pop$rdr, decreasing = TRUE), c(2, 3, 4, 5, 8:10)]

elt.pop2 <- elt.pop2[-c(199:202), ]
elt.pop2 <- dplyr::left_join(elt.usr[, 1:3], elt.tbl2)
elt.usr[, 1:3]

elt.pop2 <- dplyr::tbl_df(elt.pop2)
elt.pop
elt.pop2





gids <- apply(elt.pop[, 4:5], 1, which.max)
gids <- apply(elt.pop2[, 4:5], 1, which.max)
gs <- c("D", "R")

elt.pop$g <- gs[gids]
elt.pop <- rbind(elt.pop, elt.pop2)

e <- e[!e$date %in% baddates, ]
gelt <- e
gelt$D <- gelt$d + runif(NROW(gelt), 0, 20)
gelt$R <- gelt$r + runif(NROW(gelt), 0, 20)
gelt$size <- 1
gops <- c("DRUDGE_REPORT", "seanhannity", "SarahPalinUSA", "foxnewspolitics")
dems <- c("paulkrugman", "HuffPostPol", "Salon", "maddow")
ents <- c("AMC_TV", "SInow", "AmericanIdol", "survivorcbs")
gelt$size[gelt$screen_name %in% c(gops, dems, ents)] <- 1
gelt$text <- gelt$screen_name
gelt$text[!gelt$screen_name %in% c(gops, dems, ents)] <- NA
p
png("initselts.png", 600, 600)

gelt$D <- gelt$d + runif(NROW(gelt), -50, 50)
gelt$R <- gelt$r + runif(NROW(gelt), -50, 50)
gelt <- gelt[gelt$date == sort(unique(gelt$date))[1], ]
p <- ggplot(gelt, aes(x = D, y = R)) +
    geom_jitter(width = 10, height = 10, alpha = .3, pch = 16, size = 5,
               aes(color = g)) +
    xlim(-50, 625) + ylim(-50, 625) +
    labs(x = "D followers", y = "R followers",
         title = "Partisan clustering around source accounts") +
    theme(axis.title.x = element_text(vjust = 0,
                                      hjust = .5,
                                      size = 26)) +
    ggcolor(ltcols()) + ggfill(ltcols())

subset(elt.pop[, c(3:9)], d > 250 & r > 250)
subset(elt.pop[, c(3:9)], screen_name %in%
                          c("realDonaldTrump",
                            "HillaryClinton"))
grep("mike", elt.pop$screen_name, ignore.case = TRUE, value = TRUE)
dev.new()

gops <- c("DRUDGE_REPORT", "seanhannity", "SarahPalinUSA", "foxnewspolitics")
dems <- c("paulkrugman", "HuffPostPol", "Salon", "maddow")
gelt$D <- gelt$d + runif(NROW(gelt), -10, 10)
gelt$R <- gelt$r + runif(NROW(gelt), -10, 10)

gelt$text <- gelt$screen_name
gelt$text[gelt$screen_name %in% c(gops, dems)] <- NA
gelt$text[gelt$d < 100 | gelt$r < 150 | gelt$d > 300 | gelt$r > 300] <- NA
gelt$text[gelt$r < 200] <- NA

ggplot(gelt, aes(x = D, y = R,
                    label = text, frame = date)) +
    geom_text(family = "Georgia", size = 3, aes(color = g)) +
    xlim(-50, 700) + ylim(0, 600) +
    labs(x = "Democrat", y = "Republican") +
    theme(legend.position = "none") +
    ggcolor(ltcols(c)) +
    ggfill(dkcols(c))


