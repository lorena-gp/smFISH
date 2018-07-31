# Lorena Garcia-Perez, Developmental Dynamics lab, The Francis Crick Institute, UK
# lorena.garcia-perez@crick.ac.uk


# RNA concentration (density == concentration) ----------------------------

# 1. White background plots

#   a. Box plots:

concentration.box.plot <- function(data) {
  plots <-
    list(
      ggplot(data, aes(day, density, group=day)) +
        geom_boxplot(outlier.shape = NA, fill = NA, colour = cbbPalette[3], lwd = 2) +
        geom_quasirandom(colour =  "black") +
        ylab(expression(paste(" RNAs /", mu, m^2, sep=""))) +
        ylim(0, max(data$density)) +
        labs (title = "RNA concentration") +
        theme_classic() +
        theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[2],
                                      margin = margin(t = 0, r = 0, b = 30, l = 0)),
              axis.text=element_text(size=40),
              axis.title=element_text(size=40),
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.line = element_line(size = 3),
              axis.ticks = element_line(size = 3),
              axis.ticks.length=unit(0.5,"cm"),
              strip.text.x = element_text(size = 35, face = "bold"),
              strip.background = element_rect(fill=cbbPalette[3], colour = cbbPalette[3]),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              legend.background = element_rect(fill = "transparent"),
              legend.box.background = element_rect(fill = "transparent")) +
        facet_grid(. ~ channel)
    )
  ggsave("./figures/concentration_box_plot.pdf", plots, width = 25, height = 10)
}

#   b. Scatter and density contours plots:

# channel.1 and channel.2 must be supplied as strings
# data must have channel as factor

concentration.pairwise.scatter.plot <- function(data, channel.1, channel.2) {
  data.subset <-
    filter(data, channel == channel.1 | channel == channel.2) %>%
    spread(channel, density)
  plots <-
    list(
      ggplot(data.subset, aes_string(channel.2, channel.1)) +
        geom_point(size = 2) +
        geom_density_2d(size = 1, colour = cbbPalette[3], bins = 10) +
        labs (title = "RNA concentration") +
        scale_x_continuous(name = substitute(paste(channel.2, " RNAs /", mu, m^2, sep ="")),
                           breaks = seq(0, max(data.subset$density), 25),
                           limits = c(0,max(data.subset$copy))) +
        scale_y_continuous(name = substitute(paste(channel.1, " RNAs /", mu, m^2, sep ="")),
                           breaks = seq(0, max(data.subset$density), 25),
                           limits = c(0,max(data.subset$density))) +
        theme_classic() +
        theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[3],
                                      margin = margin(t = 0, r = 0, b = 30, l = 0)),
              axis.text=element_text(size=40),
              axis.title=element_text(size=40),
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.line = element_line(size = 3),
              axis.ticks = element_line(size = 3),
              axis.ticks.length=unit(0.5,"cm"),
              strip.text.x = element_text(size = 35, face = "bold"),
              strip.background = element_rect(fill=cbbPalette[3], colour = cbbPalette[3]),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              legend.background = element_rect(fill = "transparent"),
              legend.box.background = element_rect(fill = "transparent")) +
        facet_grid(. ~ day)
    )
  ggsave(paste("./figures/concentration_pairwise_scatter_plot_",channel.2,"_",channel1,".pdf", sep=""),
         plots, width = 25, height = 10)
}


# 2. Dark background plots

#   a. Box plots:

concentration.box.plot.dark <- function(data) {
  plots <-
    list(
      ggplot(data, aes(day, density, group=day)) +
        geom_boxplot(outlier.shape = NA, fill = NA, colour = cbbPalette[3], lwd = 2) +
        geom_quasirandom(colour =  "white") +
        ylab(expression(paste("RNAs /", mu, m^2, sep=""))) +
        ylim(0, max(data$density)) +
        labs (title = "RNA concentration") +
        theme_classic() +
        theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[3],
                                      margin = margin(t = 0, r = 0, b = 30, l = 0)),
              axis.text=element_text(size=40, colour = "lightgrey"),
              axis.title=element_text(size=40, colour = "lightgrey"),
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.line = element_line(size = 3, colour = "lightgrey"),
              axis.ticks = element_line(size = 3, colour = "lightgrey"),
              axis.ticks.length=unit(0.5,"cm"),
              strip.text.x = element_text(size = 35, face = "bold", colour = "lightgrey"),
              strip.background = element_rect(fill=cbbPalette[3], colour = cbbPalette[2]),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              legend.background = element_rect(fill = "transparent"),
              legend.box.background = element_rect(fill = "transparent")) +
        facet_grid(. ~ channel)
    )
  ggsave("./figures/concentration_box_plot_dark.pdf", plots, width = 25, height = 10)
}

#   b. Scatter and density contours plots:

# channel.1 and channel.2 must be supplied as strings
# data must have channel as factor

concentration.pairwise.scatter.plot.dark <- function(data, channel.1, channel.2) {
  data.subset <-
    filter(data, channel == channel.1 | channel == channel.2) %>%
    spread(channel, density)
  plots <-
    list(
      ggplot(data.subset, aes_string(channel.2, channel.1)) +
        geom_point(color = "white", size = 2) +
        geom_density_2d(size = 1, colour = cbbPalette[3], bins = 10) +
        labs (title = "RNA concentration") +
        scale_x_continuous(name = substitute(paste(channel.2, " RNAs /", mu, m^2, sep ="")),
                           breaks = seq(0, max(data.subset$density), 25),
                           limits = c(0,max(data.subset$copy))) +
        scale_y_continuous(name = substitute(paste(channel.1, " RNAs /", mu, m^2, sep ="")),
                           breaks = seq(0, max(data.subset$density), 25),
                           limits = c(0,max(data.subset$density))) +
        theme_classic() +
        theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[3],
                                      margin = margin(t = 0, r = 0, b = 30, l = 0)),
              axis.text=element_text(size=40, colour = "lightgrey"),
              axis.title=element_text(size=40, colour = "lightgrey"),
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.line = element_line(size = 3, colour = "lightgrey"),
              axis.ticks = element_line(size = 3, colour = "lightgrey"),
              axis.ticks.length=unit(0.5,"cm"),
              strip.text.x = element_text(size = 35, face = "bold", colour = "lightgrey"),
              strip.background = element_rect(fill=cbbPalette[3], colour = cbbPalette[3]),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              legend.background = element_rect(fill = "transparent"),
              legend.box.background = element_rect(fill = "transparent")) +
        facet_grid(. ~ day)
    )
  ggsave(paste("./figures/concentration_pairwise_scatter_plot_dark_",channel.2,"_",channel1,".pdf", sep=""),
         plots, width = 25, height = 10)
}


#########################################################################################
#########################################################################################

# Random: TO BE CLEANED

x <- list(spots.cell.expressing[[1]]$cell.area,
          spots.cell.expressing[[2]]$cell.area,
          spots.cell.expressing[[3]]$cell.area)
y <- list(spots.cell.expressing[[1]]$copy.C0,
          spots.cell.expressing[[2]]$copy.C1,
          spots.cell.expressing[[3]]$copy.C2)
cor.Pearson.copy <- cbind(c(cor(x[[1]], y[[1]] ,method="pearson"),
                  cor(x[[2]], y[[2]] ,method="pearson"),
                  cor(x[[3]], y[[3]] ,method="pearson")),
                  c(0, 1, 2))
cor.Pearson.copy  <- data.frame(cor.Pearson.copy)
colnames(cor.Pearson.copy) <- c("cor", 'positive.for')
cor.Pearson.copy$positive.for <- as.factor(cor.Pearson.copy$positive.for)
levels(cor.Pearson.copy$positive.for) <- genes

p.CopyVsArea <-list(
  ggplot(spots.cell.2.expressing, aes(cell.area, copy)) +
    geom_point(alpha = 0.2, size = 4) +
    ylab("RNAs") +
    ylim(0,150) +
    annotate("text",x = 40, y = 25, label = paste("r = ", round(cor.Pearson.copy$cor, digits=2), sep=""),
             size = 12, hjust = 0) +
    xlab(expression(paste("cell area (", mu, m^2, ")", sep=""))) +
    labs (title = "Cell area analysis") +
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

########

x <- list(spots.cell.expressing[[1]]$cell.area,
          spots.cell.expressing[[2]]$cell.area,
          spots.cell.expressing[[3]]$cell.area)
y <- list(spots.cell.expressing[[1]]$density.C0,
          spots.cell.expressing[[2]]$density.C1,
          spots.cell.expressing[[3]]$density.C2)
cor.Pearson.density <- cbind(c(cor(x[[1]], y[[1]] ,method="pearson"),
                       cor(x[[2]], y[[2]] ,method="pearson"),
                       cor(x[[3]], y[[3]] ,method="pearson")),
                     c(0, 1, 2))
cor.Pearson.density <- data.frame(cor.Pearson.density)
colnames(cor.Pearson.density) <- c("cor", 'positive.for')
cor.Pearson.density$positive.for <- as.factor(cor.Pearson.density$positive.for)
levels(cor.Pearson.density$positive.for) <- genes

p.DenVsArea <-list(
  ggplot(spots.cell.2.expressing, aes(cell.area, density)) +
    geom_point(alpha = 0.2, size = 4) +
    ylab(expression(paste(" RNAs /", mu, m^2, sep =""))) +
    ylim(0,6) +
    annotate("text",x = 30, y = 0.5, label = paste("r = ", round(cor.Pearson.density$cor, digits=2), sep=""),
             size = 12, hjust = 0) +
    xlab(expression(paste("cell area (", mu, m^2, ")", sep=""))) +
    labs (title = "Cell area analysis") +
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
