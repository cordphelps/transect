linearFieldLayout <- function(vectorX, vectorY, alpha, color) {
  
  # New graphic window 
  dev.new(title= alpha[1])
  # Create an empty frame (proportional axes 1:1, with titles)
  # Geographic coordinates x and y from the spa data frame
  plot(vectorX, vectorY, asp=NA, type="n", main=alpha[1], xlab=alpha[2], ylab=alpha[3])
  # Add a blue line connecting the sites (Doubs river)
  lines(vectorX, vectorY, col=color[1])
  
  # create a vertical line separating the edge from the vineyard
  yAxisPoints <- seq.int(1, nrow(spatial.df) , 1)
  lines(rep.int(0, length(yAxisPoints)), yAxisPoints, col=color[2])
  
  # Add site labels
  text(spatial.df$X, spatial.df$Y, row.names(spatial.df), cex=0.8, col=color[3])
  # Add text blocks 
  text(-3, 4, "margin", cex=1.2, col=color[3])
  text(10, 6, "vineyard\ninterior", cex=1.2, col=color[3])
  
  
  
}

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
    labs(x = "field margin offset", y = "transect") +
    theme_bw() +
    theme() + 
    # theme(legend.position = "bottom", legend.direction = "horizontal") +
    theme(legend.position = "none", legend.direction = "horizontal") +
    theme(legend.box = "horizontal", legend.key.size = unit(1, "cm")) 
  
  return(ggplotObject)
}

bugGGfromList <- function (df, bugListdataPoints, title) {
  
  # http://t-redactyl.io/blog/2016/02/creating-plots-in-r-using-ggplot2-part-6-weighted-scatterplots.html
  ggplotObject <- ggplot() +   # 'size' is the name of the variable to plot

    geom_point(data=df[1], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[3]], 
      shape=21, colour = "purple", fill = "plum", alpha=0.6) +  # shape 21 is a circle with outline and fill colors
    geom_point(data=df[1], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[4]], 
      shape=21, colour = "purple", fill = "green", alpha=0.6) +
    geom_point(data=df[1], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[5]], 
      shape=21, colour = "purple", fill = "red", alpha=0.6) +
    geom_point(data=df[1], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[6]], 
      shape=21, colour = "purple", fill = "blue", alpha=0.6) +

    geom_point(data=df[2], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[3]], 
      shape=21, colour = "purple", fill = "plum", alpha=0.6) +  # shape 21 is a circle with outline and fill colors
    geom_point(data=df[2], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[4]], 
      shape=21, colour = "purple", fill = "green", alpha=0.6) +
    geom_point(data=df[2], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[5]], 
      shape=21, colour = "purple", fill = "red", alpha=0.6) +
    geom_point(data=df[2], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[6]], 
      shape=21, colour = "purple", fill = "blue", alpha=0.6) +

    geom_point(data=df[3], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[3]], 
      shape=21, colour = "purple", fill = "plum", alpha=0.6) +  # shape 21 is a circle with outline and fill colors
    geom_point(data=df[3], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[4]], 
      shape=21, colour = "purple", fill = "green", alpha=0.6) +
    geom_point(data=df[3], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[5]], 
      shape=21, colour = "purple", fill = "red", alpha=0.6) +
    geom_point(data=df[3], aes(x=bugListDataPoints[[1]], y=bugListDataPoints[[2]]), size=bugListDataPoints[[6]], 
      shape=21, colour = "purple", fill = "blue", alpha=0.6) +



    #scale_size_area(max_size = 20) +
    # geom_count() probably more appropriate http://ggplot2.tidyverse.org/reference/scale_size.html
    scale_size(range = c(1, 10)) +
    scale_fill_continuous(low = "plum1", high = "purple4") +
    scale_y_continuous(breaks = seq(1, 10, 2)) +

    annotate("rect", xmin=-5, xmax=0, ymin=0,ymax=10, alpha=0.2, fill="red") +
    annotate("rect", xmin=0, xmax=15, ymin=0,ymax=10, alpha=0.2, fill="green") +

    ggtitle(title) +

    labs(x = "field margin offset", y = "transect") +

    theme_bw() +
    theme(legend.justification=c(0,0), legend.position=c(0,1)) # Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
    #theme(legend.position = c(0, 7)) 
  
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
  
  # New graphic window 
  dev.new(title= "multiplot")
  
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




