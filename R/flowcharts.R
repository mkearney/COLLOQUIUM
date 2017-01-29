
png("datascience_flowchart1.png", 7, 5, "in", res = 127.5)
fig.x <- c(-.85, -.85, -.10,  .85,  .85,  .85)
fig.y <- c( .75,  .00,  .00,  .75,  .00, -.75)
n <- length(fig.x)
par(mar = c(1, 1, 1, 1), bg = "transparent",
    family = "Avenir Next Condensed")
symbols(0, 0, circles = 1, fg = NA, inches = TRUE, xaxt = "n",
        yaxt = "n", bty = "n", xlab = NA, ylab = NA)
symbols(fig.x, fig.y, circles = rep(.185, n),
        inches = FALSE, add = TRUE, lwd = 1.25,
        fg = c("transparent", "transparent", rep("#666666", 4)),
        bg = c("transparent", "transparent", rep("#dddddd", 4)))
cex <- 1.25
lwd <- 1.75
ll <- .06
col <- "#333333"
text(-.096,  .0, "Transform", cex = cex)
arrows(.15, .1, .6, .6, lwd = lwd, length = ll, code = 3, col = col)
arrows(.15, .0, .6, .0, lwd = lwd, length = ll, code = 3, col = col)
text( .85,  .75, "Visualize", cex = cex)
arrows(.85, .45, .85, .3, lwd = lwd, length = ll, code = 3, col = col)
text( .85,  .0, "Model", cex = cex)
arrows(.85, -.3, .85, -.45, lwd = lwd, length = ll, col = col)
text( .85, -.75, "Present", cex = cex)
dev.off()


png("datascience_flowchart2.png", 7, 5, "in", res = 127.5)
fig.x <- c(-.85, -.85, -.10,  .85,  .85,  .85)
fig.y <- c( .75,  .00,  .00,  .75,  .00, -.75)
n <- length(fig.x)
par(mar = c(1, 1, 1, 1), bg = "transparent",
    family = "Avenir Next Condensed")
symbols(0, 0, circles = 1, fg = NA, inches = TRUE, xaxt = "n",
        yaxt = "n", bty = "n", xlab = NA, ylab = NA)

symbols(fig.x, fig.y, circles = rep(.185, n),
        inches = FALSE, add = TRUE, lwd = 1.25,
        fg = c("#003366", "#003366", rep("#666666", 4)),
        bg = c("#6699cc", "#6699cc", rep("#dddddd", 4)))

cex <- 1.25
lwd <- 1.75
ll <- .06
col <- "#333333"
text(-.85,  .75, "Import", cex = cex, col = "white")
arrows(-.85, .45, -.85, .3, lwd = lwd, length = ll, col = col)
text(-.85,  .0, "Wrangle", cex = cex, col = "white")
arrows(-.6, .0, -.35, .0, lwd = lwd, length = ll, col = col)
text(-.096,  .0, "Transform", cex = cex)
arrows(.15, .1, .6, .6, lwd = lwd, length = ll, code = 3, col = col)
arrows(.15, .0, .6, .0, lwd = lwd, length = ll, code = 3, col = col)
text( .85,  .75, "Visualize", cex = cex)
arrows(.85, .45, .85, .3, lwd = lwd, length = ll, code = 3, col = col)
text( .85,  .0, "Model", cex = cex)
arrows(.85, -.3, .85, -.45, lwd = lwd, length = ll, col = col)
text( .85, -.75, "Present", cex = cex)
dev.off()

