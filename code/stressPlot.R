ggplot(NULL, aes(plot.list$Stress$x, plot.list$Stress$y, size=.5)) +
 geom_point(shape=21, colour = "purple", fill = "plum", alpha=0.6) +
 geom_line(colour = "royalblue", alpha=0.6, aes(plot.list$Stress$x, plot.list$Stress$y, size=.1))
