
data <- readRDS("data/sentanalysis.rds")
df <- data.frame(
    x = lapply(data, getElement, "x") %>%
        unlist(use.names = FALSE),
    y = lapply(data, getElement, "y") %>%
        unlist(use.names = FALSE),
    stringsAsFactors = FALSE)
df$candidate <- NA_character_
df$candidate[1:6169] <- data[[1]]$name
df$candidate[6170:12338] <- data[[2]]$name
df$candidate[12339:18507] <- data[[3]]$name
df$candidate[18508:24676] <- data[[4]]$name
df$party <- NA_character_
df$party[1:12338] <- "R"
df$party[12339:24676] <- "D"
df$x <- as.POSIXct(df$x)
df$y <- as.numeric(df$y)

rm(data)
##df$month <- as.Date(gsub("-[0-9][0-9]$", "-01", as.Date(df$x)))
df$day <- as.Date(df$x)
df <- with(df, aggregate(y, by = list(
                                candidate = candidate,
                                day = day), mean))
names(df)[2:3] <- c("x", "y")

df$party <- "D"
df$party[df$candidate %in% c("Donald Trump", "Ted Cruz")] <- "R"

png("images/sentanalysisprimary.png", 6, 4.25, "in", res = 127.5)
ggplot(df, aes(x = x, y = y, color = candidate)) +
    geom_line(size = .5, alpha = .5) +
    geom_point(size = 1, alpha = .5) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          text = element_text("Avenir Next Condensed", size = 13),
          axis.text.y = element_blank(),
          axis.title.y = element_text(size = 9),
          plot.title = element_text(face = "bold", size = 16),
          legend.position = "bottom",
          axis.ticks.y = element_blank(),
          strip.text = element_blank()) +
    scale_color_manual(
        values = c("Bernie Sanders" = "#66aa00",
                   "Hillary Clinton" = "#0022FF",
                   "Ted Cruz" = "#cc9900",
                   "Donald Trump" = "#ff0066"),
        labels = c("Bernie Sanders",
                   "Hillary Clinton",
                   "Ted Cruz",
                   "Donald Trump")
    ) +
    facet_wrap(~ party, ncol = 1) +
    labs(title = "Sentiment analysis of tweets during presidential primaries",
         subtitle = "Tweets collected daily using mentions and hashtags",
         x = "",
         y = paste0("Negative       -       Positive                          ",
                    "    Negative       -       Positive"))
dev.off()
