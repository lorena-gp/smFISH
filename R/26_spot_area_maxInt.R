####################

p.spots.AreaVsMaxInt <-list(
    ggplot(file, aes(area, max.int, group=area)) +
    geom_boxplot(outlier.shape = NA, color = cbbPalette[5], lwd = 1) +
    geom_quasirandom(alpha = 0.1) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill=cbbPalette[1], alpha=0.075, inherit.aes = FALSE) +
    ylab("spot max intensity (a.u.)") +
    xlab("spot area (px)") +
    xlim(0.5,16) +
    labs (title = "Spot analysis") +
    theme_classic() +
    theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[5],
                                  margin = margin(t = 0, r = 0, b = 30, l = 0)),
          axis.text=element_text(size=35, color = "black"),
          axis.title=element_text(size=35),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.line = element_line(size = 3),
          axis.ticks = element_line(size = 3),
          axis.ticks.length=unit(0.5,"cm"),
          strip.text.x = element_text(size = 35, face = "bold", colour = "white"),
          strip.background = element_rect(fill=cbbPalette[5], colour = cbbPalette[5])) +
          facet_grid(. ~ channel)
)

ggsave("p.spots.AreaVsMaxInt.pdf", plot = p.spots.AreaVsMaxInt[[1]],
       width = 25, height = 10)

hist.spots.area <-list(
  ggplot(file, aes(area)) +
    geom_histogram(bins = 75) +
    labs (title = "Spot analysis") +
    scale_x_continuous(name = "spot area (px)", limits = c(0,21)) +
    scale_y_continuous(name = "count") +
    theme_classic() +
    theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[5],
                                  margin = margin(t = 0, r = 0, b = 30, l = 0)),
          axis.text=element_text(size=35, color = "black"),
          axis.title=element_text(size=35),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.line = element_line(size = 3),
          axis.ticks = element_line(size = 3),
          axis.ticks.length=unit(0.5,"cm"),
          strip.text.x = element_text(size = 35, face = "bold", colour = "white"),
          strip.background = element_rect(fill=cbbPalette[5], colour = cbbPalette[5])) +
    facet_grid(. ~ channel)
)

hist.spots.MaxInt <-list(
  ggplot(file, aes(max.int)) +
    geom_histogram(bins = 300) +
    labs (title = "Spot analysis") +
    scale_x_continuous(name = "spot max intensity (a.u.)", limits = c(0,NA)) +
    scale_y_continuous(name = "count") +
    theme_classic() +
    theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[5],
                                  margin = margin(t = 0, r = 0, b = 30, l = 0)),
          axis.text=element_text(size=25, color = "black"),
          axis.title=element_text(size=35),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.line = element_line(size = 3),
          axis.ticks = element_line(size = 3),
          axis.ticks.length=unit(0.5,"cm"),
          strip.text.x = element_text(size = 35, face = "bold", colour = "white"),
          strip.background = element_rect(fill=cbbPalette[5], colour = cbbPalette[5])) +
    facet_grid(. ~ channel)
)

####################