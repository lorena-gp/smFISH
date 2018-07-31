
# Lorena Garcia-Perez, Developmental Dynamics lab, The Francis Crick Institute, UK
# lorena.garcia-perez@crick.ac.uk


# Trancription probability ------------------------------------------------

prob.TX.fn <- function(data, day.chosen, gene.chosen, ploidy) {
  # day.chosen and gene.schosen must be input as character (for example, "4" and "Olig2")
  # ploidy must be input as numeric
  day.gene.TX.subset <-
    pull(filter(data, day == day.chosen & channel == gene.chosen), TX.number)
  return(sum(day.gene.TX.subset)/(ploidy*length(day.gene.TX.subset)))
}


# Mean RNA copy number ----------------------------------------------------

mean.copy.fn <- function(data, day.chosen, gene.chosen) {
  # day.chosen and gene.schosen must be input as character (for example, "4" and "Olig2")
  day.gene.copy.subset <-
    pull(filter(data, day == day.chosen & channel == gene.chosen), copy)
  return(mean(day.gene.copy.subset))
}


# Mean number of RNA pol = mean number of molecules at TX spot ------------

mean.nPol.fn <- function(data, day.chosen, gene.chosen) {
  # day.chosen and gene.schosen must be input as character (for example, "4" and "Olig2")
  day.gene.nPol.subset <-
    pull(filter(data, day == day.chosen & channel == gene.chosen), TX.mol)
  return(mean(day.gene.nPol.subset))
}

#####################################

# Chose white or dark themes as part of the plotting funtion inputs

basicTheme <-list(
  theme(plot.title = element_text(size = 35, hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 30, l = 0)),
        axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.line = element_line(size = 3),
        axis.ticks = element_line(size = 3),
        axis.ticks.length = unit(0.5,"cm"),
        strip.text.x = element_text(size = 35, face = "bold"),
        panel.spacing = unit(4, "lines"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", colour = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent")) # get rid of legend panel bg
)

whiteTheme <- list(theme())

darkTheme <- list(
  theme(axis.text = element_text(colour = "lightgrey"),
        axis.title = element_text(colour = "lightgrey"),
        axis.line = element_line(colour = "lightgrey"),
        axis.ticks = element_line(colour = "lightgrey"),
        strip.text.x = element_text(colour = "lightgrey"))
)

# RNA copy number ---------------------------------------------------------

# 1. Box plots:

copy.box.plot <-
  function(data, Theme = whiteTheme) {
    data$day <- as.factor(data$day)
    levels(data$day) <- days.names
    plots <-
      ggplot(data, aes(day, copy, group=day)) +
        geom_boxplot(outlier.shape = NA, fill = NA, colour = cbbPalette[2], lwd = 2) +
        geom_quasirandom(shape = 21, fill = "lightgrey", colour = "black", size = 2,
                         alpha = 0.4) +
        ylab("RNAs per cell") +
        xlab("") +
        ylim(0, max(data$copy)) +
        labs (title = "RNA copy number") +
        theme_classic() +
        basicTheme +
        Theme +
        theme(plot.title=element_text(colour = cbbPalette[2]),
              strip.background = element_rect(fill = cbbPalette[2], colour = cbbPalette[2])) +
        facet_wrap(~ channel)
    ggsave(paste("./figures/copy_box_plot_", deparse(substitute(Theme)), ".pdf", sep=""),
           plots, width = 25, height = 10)
  }

# 2. Scatter and density contours plots:

# channel.1 and channel.2 must be supplied as strings
# data must have channel as factor

copy.pairwise.scatter.plot <- # Could try to make axis limit specific for the pair of genes being plot each time...
  function(data, channel.1, channel.2, Theme = whiteTheme) {
    data.subset <-
      data %>%
      select(channel, day, image, cell.id, cell.area, copy) %>%
      spread(channel, copy)
    plots <-
      ggplot(data.subset, aes_string(channel.2, channel.1)) +
        geom_point(shape = 21, fill = "lightgrey", colour = "black", size = 3) +
        geom_density_2d(size = 1, colour = cbbPalette[2], bins = 10) +
        labs (title = "RNA copy number") +
        scale_x_continuous(name = substitute(paste(channel.2, " RNAs per cell", sep ="")),
                           breaks = seq(0, max(data$copy), 50),
                           limits = c(0, max(data$copy))) +
        scale_y_continuous(name = substitute(paste(channel.1, " RNAs per cell", sep ="")),
                           breaks = seq(0, max(data$copy), 50),
                           limits = c(0, max(data$copy))) +
        theme_classic() +
        basicTheme +
        Theme +
        theme(plot.title=element_text(colour = cbbPalette[2]),
              strip.background = element_rect(fill = cbbPalette[2], colour = cbbPalette[2])) +
        facet_wrap(~ day)
    ggsave(paste("./figures/copy_pairwise_scatter_plot_", deparse(substitute(Theme)),"_",
                  channel.2,"_",channel.1,".pdf", sep=""),
           plots, width = 25, height = 10)
  }


# RNA concentration (density == concentration) ----------------------------

# 1. Box plots:

concentration.box.plot <-
  function(data, Theme = whiteTheme) {
    data$day <- as.factor(data$day)
    levels(data$day) <- days.names
    plots <-
      ggplot(data, aes(day, density, group=day)) +
      geom_boxplot(outlier.shape = NA, fill = NA, colour = cbbPalette[2], lwd = 2) +
      geom_quasirandom(shape = 21, fill = "lightgrey", colour = "black", size = 2,
                       alpha = 0.4) +
      ylab(expression(paste(" RNAs /", mu, m^2, sep=""))) +
      xlab("") +
      ylim(0, max(data$density)) +
      labs (title = "RNA concentration") +
      theme_classic() +
      basicTheme +
      Theme +
      theme(plot.title=element_text(colour = cbbPalette[2]),
            strip.background = element_rect(fill = cbbPalette[2], colour = cbbPalette[2])) +
      facet_wrap(~ channel)
    ggsave(paste("./figures/concentration_box_plot_", deparse(substitute(Theme)), ".pdf", sep=""),
           plots, width = 25, height = 10)
  }


# 2. Scatter and density contours plots:

# channel.1 and channel.2 must be supplied as strings
# data must have channel as factor

concentration.pairwise.scatter.plot <- # Could try to make axis limit specific for the pair of genes being plot each time...
    function(data, channel.1, channel.2, Theme = whiteTheme) {
    data.subset <-
      data %>%
      select(channel, day, image, cell.id, cell.area, density) %>%
      spread(channel, density)
    plots <-
      ggplot(data.subset, aes_string(channel.2, channel.1)) +
      geom_point(shape = 21, fill = "lightgrey", colour = "black", size = 3) +
      geom_density_2d(size = 1, colour = cbbPalette[2], bins = 10) +
      labs (title = "RNA cconcentration") +
      scale_x_continuous(name = substitute(paste(channel.2, " RNAs /", mu, m^2, sep ="")),
                         breaks = seq(0, max(data$density), 2),
                         limits = c(0, max(data$density))) +
      scale_y_continuous(substitute(paste(channel.1, " RNAs /", mu, m^2, sep ="")),
                         breaks = seq(0, max(data$density), 2),
                         limits = c(0, max(data$density))) +
      theme_classic() +
      basicTheme +
      Theme +
      theme(plot.title=element_text(colour = cbbPalette[2]),
            strip.background = element_rect(fill = cbbPalette[2], colour = cbbPalette[2])) +
      facet_wrap(~ day)
    ggsave(paste("./figures/concentration_pairwise_scatter_plot_", deparse(substitute(Theme)),"_",
                 channel.2,"_",channel.1,".pdf", sep=""),
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
    facet_wrap(~ positive.for)
)