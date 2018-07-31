
bar.TX.prob <- list(
  ggplot(TX.probability, aes(factor(channel), TX.prob, fill = factor(day))) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(name = "day  ", values = cbbPalette[c(1,5,4)],
                    labels = c(" 4 ", " 5 ", " 6 ")) +
  ylab("Transcription probability") +
  labs (title = "Transcription analysis") +
  scale_x_discrete(name = "genes", labels=c(genes[1], genes[2], genes[3])) +
  
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
ggsave(file = "bar.TX.prob.pdf", bar.TX.prob[[1]], width = 9, height = 9)