

mu <- readr::read_rds("/Users/mwk/mizzou/wrangled/FINALUSERSDF.rds")
library(ggplot2)
mytheme <- theme_minimal() +
    theme(legend.title = element_blank(),
          text = element_text("Avenir Next Condensed", size = 13),
          plot.title = element_text(face = "bold", size = 16),
          legend.position = "none",
          strip.text = element_blank())
theme_set(mytheme)
mu$polarized <- mu$r.w
mu$polarized[mu$group == "d"] <- mu$d.w[mu$group == "d"]
xx <- (mu$polarized - mean(mu$polarized)) / sd(mu$polarized) - min((mu$polarized - mean(mu$polarized)) / sd(mu$polarized))
xx <- xx / max(xx)
mu$polarized <- xx

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
