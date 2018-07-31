box.TX.molecules <-list(
  ggplot(TX.molecules, aes(day, TX.mol, group = day)) +
    geom_boxplot(outlier.shape = NA, color = cbbPalette[5], lwd = 2) +
    geom_quasirandom(alpha = 0.3) +
    ylab("RNAs per transcription site") +
    ylim(0, 30) +
    xlim(3.5, 6.5) +
    labs (title = "Transcription analysis") +
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

box.TX.molecules_modified <-
  ggplot(TX.molecules, aes(day, TX.mol, group = day)) +
    geom_boxplot(outlier.shape = NA, fill = NA, color = cbbPalette[5], lwd = 2) +
    geom_quasirandom(alpha = 0.5, colour = "lightgrey") +
    ylab("RNAs per transcription site") +
    ylim(0, 30) +
    xlim(3.5, 6.5) +
    labs (title = "Transcription analysis") +
    theme_classic() +
    theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[5],
                                  margin = margin(t = 0, r = 0, b = 30, l = 0)),
          axis.text=element_text(size=35, color = "lightgrey"),
          axis.title=element_text(size=35, color = "lightgrey"),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.line = element_line(size = 3, color = "lightgrey"),
          axis.ticks = element_line(size = 3, color = "lightgrey"),
          axis.ticks.length=unit(0.5,"cm"),
          strip.text.x = element_text(size = 35, face = "bold", color = "lightgrey"),
          strip.background = element_rect(fill=cbbPalette[5], colour = cbbPalette[5]),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", colour = NA), # bg of the plot
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent")) # get rid of legend panel bg) +
    facet_grid(. ~ channel)

ggsave("box.TX.molecules_modified.pdf", plot = box.TX.molecules_modified, width = 25, height = 10)

#######

TX.molecules.irx3.D6 <- TX.molecules[TX.molecules$channel == "Irx3" & TX.molecules$day == 6,]
TX.molecules.olig2.D6 <- TX.molecules[TX.molecules$channel == "Olig2" & TX.molecules$day == 6,]
TX.molecules.irx3.olig2.D6 <- rbind(TX.molecules.irx3.D6 , TX.molecules.olig2.D6)

box.TX.molecules_D6_Irx3_Olig2 <-
  ggplot(TX.molecules.irx3.olig2.D6, aes(channel, TX.mol, group = channel)) +
  geom_boxplot(outlier.shape = NA, color = c("deepskyblue","lightcoral"), lwd = 2, fill = NA) +
  geom_quasirandom(size = 3, color = "black") +
  ylab("RNAs per transcription site") +
  xlab("") +
  ylim(0, 100) +
  #labs (title = "Transcription") +
  theme_classic() +
  theme(plot.title=element_text(size=35, hjust = 0.5, colour = "olivedrab3",
                                margin = margin(t = 0, r = 0, b = 30, l = 0),
                                face = "bold"),
        axis.text=element_text(size=40, color = "black"),
        axis.title=element_text(size=40),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.line = element_line(size = 3),
        axis.ticks = element_line(size = 3),
        axis.ticks.length=unit(0.5,"cm"),
        strip.text.x = element_text(size = 35, face = "bold", colour = "white"),
        strip.background = element_rect(fill=cbbPalette[5], colour = cbbPalette[5]),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", colour = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent")) # get rid of legend panel bg) +

ggsave("TX_molecules_D6_Irx3_Olig2.pdf", plot = box.TX.molecules_D6_Irx3_Olig2, width = 8, height = 10)


box.TX.molecules_D6_Irx3_Olig2_dark <-
  ggplot(TX.molecules.irx3.olig2.D6, aes(channel, TX.mol, group = channel)) +
  geom_boxplot(outlier.shape = NA, color = "olivedrab2", lwd = 2, fill = NA) +
  geom_quasirandom(size = 2, color = "white") +
  ylab("RNAs per transcription site") +
  xlab("") +
  ylim(0, 100) +
  labs (title = "Transcription") +
  theme_classic() +
  theme(plot.title=element_text(size=35, hjust = 0.5, colour = "olivedrab2",
                                margin = margin(t = 0, r = 0, b = 30, l = 0)),
        axis.text=element_text(size=35, color = "lightgrey"),
        axis.title=element_text(size=35, color = "lightgrey"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.line = element_line(size = 3, color = "lightgrey"),
        axis.ticks = element_line(size = 3, color = "lightgrey"),
        axis.ticks.length=unit(0.5,"cm"),
        strip.text.x = element_text(size = 35, face = "bold", color = "lightgrey"),
        strip.background = element_rect(fill=cbbPalette[2], colour = cbbPalette[2]),
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", colour = NA) # bg of the plot
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent")) # get rid of legend panel bg) +

ggsave("TX_molecules_D6_Irx3_Olig2_dark.pdf", plot = box.TX.molecules_D6_Irx3_Olig2_dark, width = 8, height = 10)

#######