p.TX.2alleles.molecules.final <-list(
  ggplot(TX.2alleles.molecules.final, aes(TX.id.1, TX.id.2)) +
    geom_point(alpha = 0.3, size = 4) +
    #geom_density_2d(size = 1, colour = cbbPalette[5], bins = 5) +
    labs (title = "Transcription analysis") +
    scale_x_continuous(name = "RNAs per transcription site 1", limits = c(0,33)) +
    scale_y_continuous(name = "RNAs per transcription site 2", limits = c(0,33)) +
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