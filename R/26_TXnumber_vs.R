####################

p.TX.numberVsCopy  <-list(
  ggplot(spots.cell.2.expressing, aes(TX.number, copy, group = TX.number)) +
    geom_boxplot(outlier.shape = NA, color = cbbPalette[2], lwd = 2) +
    geom_quasirandom(alpha = 0.3) +
    ylab("RNAs") +
    xlab("active transcription sites") +
    ylim(0, 150) +
    xlim(-0.5,4.5) +
    labs (title = "Transcription analysis") +
    theme_classic() +
    theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[2],
                                  margin = margin(t = 0, r = 0, b = 30, l = 0)),
          axis.text=element_text(size=35, color = "black"),
          axis.title=element_text(size=35),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.line = element_line(size = 3),
          axis.ticks = element_line(size = 3),
          axis.ticks.length=unit(0.5,"cm"),
          strip.text.x = element_text(size = 35, face = "bold"),
          strip.background = element_rect(fill=cbbPalette[2], colour = cbbPalette[2])) +
    facet_grid(. ~ positive.for)
)

p.TX.numberVsDensity  <-list(
  ggplot(spots.cell.2.expressing, aes(TX.number, density, group = TX.number)) +
    geom_boxplot(outlier.shape = NA, color = cbbPalette[3], lwd = 2) +
    geom_quasirandom(alpha = 0.3) +
    ylab(expression(paste("RNAs /", mu, m^2, sep=""))) +
    xlab("active transcription sites") +
    ylim(0, 6) +
    xlim(-0.5,4.5) +
    labs (title = "Transcription analysis") +
    theme_classic() +
    theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[3],
                                  margin = margin(t = 0, r = 0, b = 30, l = 0)),
          axis.text=element_text(size=35, color = "black"),
          axis.title=element_text(size=35),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.line = element_line(size = 3),
          axis.ticks = element_line(size = 3),
          axis.ticks.length=unit(0.5,"cm"),
          strip.text.x = element_text(size = 35, face = "bold"),
          strip.background = element_rect(fill=cbbPalette[3], colour = cbbPalette[3])) +
    facet_grid(. ~ positive.for)
)


p.TX.numberVsCellArea <-list(
  ggplot(spots.cell.2.expressing, aes(TX.number, cell.area, group = TX.number)) +
    geom_boxplot(outlier.shape = NA, color = cbbPalette[1], lwd = 2) +
    geom_quasirandom(alpha = 0.3) +
    ylab(expression(paste("cell area (", mu, m^2, ")", sep=""))) +
    xlab("active transcription sites") +
    ylim(0, 100) +
    xlim(-0.5,4.5) +
    labs (title = "Transcription analysis") +
    theme_classic() +
    theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[1],
                                  margin = margin(t = 0, r = 0, b = 30, l = 0)),
          axis.text=element_text(size=35, color = "black"),
          axis.title=element_text(size=35),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.line = element_line(size = 3),
          axis.ticks = element_line(size = 3),
          axis.ticks.length=unit(0.5,"cm"),
          strip.text.x = element_text(size = 35, face = "bold", colour = "white"),
          strip.background = element_rect(fill=cbbPalette[1], colour = cbbPalette[1])) +
    facet_grid(. ~ positive.for)
)

####################