

sm <- read.csv("/Users/mwk/Dropbox/sm2waveStudy.2015/socialMedia.csv",
               stringsAsFactors = FALSE)
newnames <- names(sm)
sm <- read.csv("/Users/mwk/Dropbox/sm2waveStudy.2015/socialMedia.csv",
               skip = 1, stringsAsFactors = FALSE)
names(sm) <- newnames

sm$SMUSE <- rowMeans(sm[, c(27:32, 34:37)], na.rm = TRUE)
sm$SMAGR <- rowMeans(sm[, 43:47], na.rm = TRUE)
sm$INT <- rowMeans(sm[, 72:75], na.rm = TRUE)
sm$polar <- abs(sm$polth_1 - sm$polth_2)
sm$POLAR <- sm$polth_1 - sm$polth_2

sm$PARTY <- NA_character_
sm$PARTY[sm$favD_1 - sm$favR_1 < 0 & !is.na(sm$favD_1)] <- "GOP"
sm$PARTY[sm$favD_1 - sm$favR_1 > 0 & !is.na(sm$favD_1)] <- "DEM"
sm$polar <- as.numeric(sm$polar)
## plot

png("images/intnpsurvey.png", 6, 4.25, "in", res = 127.5)
sm[!is.na(sm$PARTY), ] %>%
    ggplot2::ggplot(ggplot2::aes(
        x = INT, y = SMAGR, size = polar)) +
    ggplot2::geom_point(
        alpha = .4, aes(color = PARTY)) +
    ggplot2::stat_smooth(
        method = "lm", alpha = .25, color = "#444444") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          text = element_text("Avenir Next Condensed", size = 13),
          axis.text.y = element_blank(),
          plot.title = element_text(face = "bold", size = 16),
          legend.position = "bottom",
          axis.ticks.y = element_blank(),
          strip.text = element_blank()) +
    ggplot2::scale_color_manual(
        values = c("#0022ff", "#dd1111")) +
    ggplot2::scale_size(range = c(.25, 4)) +
    ggplot2::facet_wrap(
        ~ PARTY, scales = "free_x") +
    ggplot2::theme(
        legend.position = "none",
        text = ggplot2::element_text(family = "Avenir Next Condensed")) +
    ggplot2::labs(
        title = "Political interest and network polarization",
        subtitle = "Self report data from a convenience sample of students in 2015",
        x = "Political interest", y = "Network polarization")
dev.off()
