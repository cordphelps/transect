

bugGG <- function (df, x, y, dataPoints, title, legend) {
  
  # http://t-redactyl.io/blog/2016/02/creating-plots-in-r-using-ggplot2-part-6-weighted-scatterplots.html
  ggplotObject <- ggplot(df, aes(x, y, size=dataPoints)) +
    geom_point(shape=21, colour = "purple", fill = "plum", alpha=0.6) +
    #scale_size_area(max_size = 20) +
    # geom_count() probably more appropriate http://ggplot2.tidyverse.org/reference/scale_size.html
    scale_size(range = c(1, 10)) +
    scale_fill_continuous(low = "plum1", high = "purple4") +
    annotate("rect", xmin=-5, xmax=0, ymin=0,ymax=10, alpha=0.2, fill="red") +
    annotate("rect", xmin=0, xmax=15, ymin=0,ymax=10, alpha=0.2, fill="green") +
    scale_y_continuous(breaks = seq(1, 10, 2)) +
    ggtitle(title) +
    labs(x = "border offset", y = "transect") +
    theme_bw() +
    theme() + 
    # theme(legend.position = "bottom", legend.direction = "horizontal") +
    theme(legend.position = "none", legend.direction = "horizontal") +
    theme(legend.box = "horizontal", legend.key.size = unit(1, "cm")) 
  
  return(ggplotObject)
}


bugPlot <- function (df.element, spatialDF, plotTitle) {
  
  plot(df.element, asp=1, cex.axis=0.8, col="brown", cex=df.element, main=plotTitle, xlab="x coordinate (km)", ylab="y coordinate (km)")
  lines(spatialDF, col="light blue")
  
}


# Multiple plot function
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

