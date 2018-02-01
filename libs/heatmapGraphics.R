
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

makeHeatmap <- function (data, type) {
  
  # Reorder the correlation matrix
  data <- reorder_cor_matrix(data)
  
  library(reshape2)
  meltedData <- melt(get_upper_triangle(data))
  
  library(ggplot2)
  
  ggplotObject <- ggplot(data = meltedData, aes(Var2, Var1, fill = value)) +
    ggtitle(paste(type, "Correlation", sep = " ", collapse = NULL)) +
    geom_tile(color = "gray78") +
    scale_fill_gradient2(low = "magenta", high = "royalblue", mid = "pink", 
                         na.value = "gray93",
                         midpoint = 0, limit = c(-1,1), space = "Lab") +
    theme_bw() + 
    # theme(legend.position = "bottom", legend.direction = "horizontal") +
    # theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      # legend.position = c(0.6, 0.7),
      legend.background = element_rect(fill = 'gray93', colour = 'black'),
      legend.title = element_blank(),
      legend.box = "horizontal",
      legend.direction = "horizontal") +
      
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5)) +
    coord_fixed()
  
  return(ggplotObject)
}


# Get lower triangle of the correlation matrix
get_lower_triangle <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_triangle <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# reorder the correlation matrix
reorder_cor_matrix <- function(data){
  # Use correlation between variables as distance
  dd <- as.dist((1-data)/2)
  hc <- hclust(dd)
  return(data[hc$order, hc$order])
}