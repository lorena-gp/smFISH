# Lorena Garcia-Perez, Developmental Dynamics lab, The Francis Crick Institute, UK
# lorena.garcia-perez@crick.ac.uk


# RNA copy number ---------------------------------------------------------

# 1. White background plots

#   a. Box plots:

copy.box.plot <- function(data) {
  plots <-
    list(
      ggplot(data, aes(day, copy, group=day)) +
        geom_boxplot(outlier.shape = NA, fill = NA, colour = "royalblue3", lwd = 2) +
        geom_quasirandom(colour =  "black") +
        ylab("RNAs per cell") +
        ylim(0, max(data$copy)) +
        labs (title = "RNA copy number") +
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
              strip.background = element_rect(fill=cbbPalette[2], colour = cbbPalette[2]),
              panel.background = element_rect(fill = "transparent"), # bg of the panel
              plot.background = element_rect(fill = "transparent", colour = NA), # bg of the plot
              legend.background = element_rect(fill = "transparent"), # get rid of legend bg
              legend.box.background = element_rect(fill = "transparent")) + # get rid of legend panel bg
        facet_grid(. ~ channel)
    )
  ggsave("./figures/copy_box_plot.pdf", plots, width = 25, height = 10)
}


#   b. Scatter and density contours plots:

# channel.1 and channel.2 must be supplied as strings
# data must have channel as factor

copy.pairwise.scatter.plot <- function(data, channel.1, channel.2) {
  data.subset <-
    filter(data, channel == channel.1 | channel == channel.2) %>%
    spread(channel, copy)
  plots <-
    list(
      ggplot(data.subset, aes_string(channel.2, channel.1)) +
      geom_point(size = 2) +
      geom_density_2d(size = 1, colour = "royalblue3", bins = 10) +
      labs (title = "RNA copy number") +
      scale_x_continuous(name = substitute(paste(channel.2, " RNAs per cell", sep ="")),
                         breaks = seq(0, max(data.subset$copy), 25),
                         limits = c(0,max(data.subset$copy))) +
      scale_y_continuous(name = substitute(paste(channel.1, " RNAs per cell", sep ="")),
                         breaks = seq(0, max(data.subset$copy), 25),
                         limits = c(0,max(data.subset$copy))) +
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
            strip.background = element_rect(fill=cbbPalette[2], colour = cbbPalette[2]), # royalblue3 ?
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent")) +
      facet_grid(. ~ day)
    )
  ggsave(paste("./figures/copy_pairwise_scatter_plot_",channel.2,"_",channel1,".pdf", sep=""),
         plots, width = 25, height = 10)
}


# 2. Dark background plots

#   a. Box plots:

copy.box.plot.dark <- function(data) {
  plots <-
    list(
      ggplot(data, aes(day, copy, group=day)) +
        geom_boxplot(outlier.shape = NA, fill = NA, colour = cbbPalette[2], lwd = 2) +
        geom_quasirandom(colour =  "white") +
        ylab("RNAs per cell") +
        ylim(0, max(data$copy)) +
        labs (title = "RNA copy number") +
        theme_classic() +
        theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[2],
                                      margin = margin(t = 0, r = 0, b = 30, l = 0)),
              axis.text=element_text(size=40, colour = "lightgrey"),
              axis.title=element_text(size=40, colour = "lightgrey"),
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.line = element_line(size = 3, colour = "lightgrey"),
              axis.ticks = element_line(size = 3, colour = "lightgrey"),
              axis.ticks.length=unit(0.5,"cm"),
              strip.text.x = element_text(size = 35, face = "bold", colour = "lightgrey"),
              strip.background = element_rect(fill=cbbPalette[2], colour = cbbPalette[2]),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              legend.background = element_rect(fill = "transparent"),
              legend.box.background = element_rect(fill = "transparent")) +
        facet_grid(. ~ channel)
    )
  ggsave("./figures/copy_box_plot_dark.pdf", plots, width = 25, height = 10)
}


#   b. Scatter and density contours plots:

# channel.1 and channel.2 must be supplied as strings
# data must have channel as factor

copy.pairwise.scatter.plot.dark <- function(data, channel.1, channel.2) {
  data.subset <-
    filter(data, channel == channel.1 | channel == channel.2) %>%
    spread(channel, copy)
  plots <-
    list(
      ggplot(data.subset, aes_string(channel.2, channel.1)) +
        geom_point(colour = "white", size = 2) +
        geom_density_2d(size = 1, colour = cbbPalette[2], bins = 10) +
        labs (title = "RNA copy number") +
        scale_x_continuous(name = substitute(paste(channel.2, " RNAs per cell", sep ="")),
                           breaks = seq(0, max(data.subset$copy), 25),
                           limits = c(0,max(data.subset$copy))) +
        scale_y_continuous(name = substitute(paste(channel.1, " RNAs per cell", sep ="")),
                           breaks = seq(0, max(data.subset$copy), 25),
                           limits = c(0,max(data.subset$copy))) +
        theme_classic() +
        theme(plot.title=element_text(size=35, hjust = 0.5, colour = cbbPalette[2],
                                      margin = margin(t = 0, r = 0, b = 30, l = 0)),
              axis.text=element_text(size=40, colour = "lightgrey"),
              axis.title=element_text(size=40, colour = "lightgrey"),
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              axis.line = element_line(size = 3, colour = "lightgrey"),
              axis.ticks = element_line(size = 3, colour = "lightgrey"),
              axis.ticks.length=unit(0.5,"cm"),
              strip.text.x = element_text(size = 35, face = "bold", colour = "lightgrey"),
              strip.background = element_rect(fill=cbbPalette[2], colour = cbbPalette[2]),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              legend.background = element_rect(fill = "transparent"),
              legend.box.background = element_rect(fill = "transparent")) +
        facet_grid(. ~ day)
    )
  ggsave(paste("./figures/copy_pairwise_scatter_plot_dark_",channel.2,"_",channel1,".pdf", sep=""),
         plots, width = 25, height = 10)
}