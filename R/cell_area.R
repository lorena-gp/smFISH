#FOR 'NEURAL PROGENITOR' SELECTED CELLS
#Boxplot CELL AREA

box.pro.area <- list( 
  ggplot(spots.cell.pro, aes(day, cell.area, group = day)) +
    geom_boxplot(outlier.shape = NA, color = cbbPalette[1], lwd = 2) +
    geom_quasirandom(alpha = 0.3) +
    ylab(expression(paste("cell area (", mu, m^2, ")", sep=""))) +
    ylim (0, max(spots.cell.pro$cell.area) + 1) +
    labs (title = "Cell area analysis") +
    theme_classic() +
    theme(plot.title=element_text(size=35, hjust = 0.5, color = cbbPalette[1],
                                  margin = margin(t = 0, r = 0, b = 30, l = 0)),
          axis.text=element_text(size=35, color = cbbPalette[1]),
          axis.title=element_text(size=35),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.line = element_line(size = 3),
          axis.ticks = element_line(size = 3),
          axis.ticks.length=unit(0.5,"cm"),
          legend.position="none")
)

box.pro.area_modified <-
  ggplot(spots.cell.pro, aes(day, cell.area, group=day)) +
  geom_boxplot(outlier.shape = NA, fill = NA, colour = "white", lwd = 2) +
  geom_quasirandom(alpha = 0.5, colour =  "lightgrey") +
  ylab(expression(paste("cell area [", mu, m^2, "]", sep=""))) +
  ylim (0, max(spots.cell.pro$cell.area) + 1) +
  labs (title = "Cell area") +
  theme_classic() +
  theme(plot.title=element_text(size=35, hjust = 0.5, colour = "white",
                                margin = margin(t = 0, r = 0, b = 30, l = 0)),
        axis.text=element_text(size=35, color = "lightgrey"),
        axis.title=element_text(size=35, color = "lightgrey"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        #axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.line = element_line(size = 3, color = "lightgrey"),
        axis.ticks = element_line(size = 3, color = "lightgrey"),
        axis.ticks.length=unit(0.5,"cm"),
        strip.text.x = element_text(size = 35, face = "bold", color = "lightgrey"),
        strip.background = element_rect(fill=cbbPalette[3], colour = cbbPalette[3]),
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", colour = NA) # bg of the plot
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent")) # get rid of legend panel bg)

ggsave("box.pro.area_modified.pdf", plot = box.pro.area_modified, width = 10, height = 10)

######

box.pro.area.dark <-
  ggplot(spots.cell.pro, aes(day, cell.area, group=day)) +
  geom_quasirandom(colour =  "white") +
  geom_boxplot(outlier.shape = NA, fill = NA, colour = "darkgrey", lwd = 2) +
  ylab(expression(paste("cell area [", mu, m^2, "]", sep=""))) +
  ylim(0,max(spots.cell.pro$cell.area) + 1) +
  labs (title = "Cell area") +
  theme_classic() +
  theme(plot.title=element_text(size=35, hjust = 0.5, colour = "darkgrey",
                                margin = margin(t = 0, r = 0, b = 30, l = 0)),
        axis.text=element_text(size=35, color = "lightgrey"),
        axis.title=element_text(size=35, color = "lightgrey"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.line = element_line(size = 3, color = "lightgrey"),
        axis.ticks = element_line(size = 3, color = "lightgrey"),
        axis.ticks.length=unit(0.5,"cm"),
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", colour = NA) # bg of the plot
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent")) # get rid of legend panel bg) +

ggsave("box_area_dark.pdf", plot = box.pro.area.dark, width = 10, height = 10)


###########################################################################