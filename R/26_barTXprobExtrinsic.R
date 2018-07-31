bar.TX.prob.extrinsic <- list(
  ggplot(TX.probability.all, aes(factor(channel), extrinsic.noise, fill = factor(day))) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_manual(name = "day  ", values = cbbPalette[c(1,5,4)],
                      labels = c(" 4 ", " 5 ", " 6 ")) +
    ylab("pAB / (pA * pB)") +
    labs (title = "Transcription analysis") +
    scale_x_discrete(name = "genes", labels=c(paste0(genes[1], "\n& ", genes[2]),
                                              paste0(genes[1], "\n& ", genes[3]),
                                              paste0(genes[2], "\n& ", genes[3]))) +
    
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
          legend.position="top",
          legend.direction = "horizontal",
          legend.key.height=unit(1.5,"line"),
          legend.text = element_text(size=35),
          legend.title = element_text(size=35)) 
)

ggsave(file = "bar.TX.prob.extrinsic.pdf", bar.TX.prob.extrinsic[[1]], width = 9, height = 9)