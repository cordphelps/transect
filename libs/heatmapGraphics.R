
heatmapGenerate <- function(speciesMatrix) {

# produce correlations coefficients between the possible pairs of variables 
# cor(matrix, method = c("pearson", "kendall", "spearman"))
# ?cor
# ... For cor(), if method is "kendall" or "spearman", Kendall's tau or Spearman's rho 
# statistic is used to estimate a rank-based measure of association. These are 
# more robust and have been recommended if the data do not necessarily come from 
# a bivariate normal distribution.

# Correlation matrix list with significance levels (coefficients matrix plus p-value matrix)
# > ?rcorr
# .... Spearman correlations are the Pearson linear correlations computed on the ranks of 
# non-missing elements, using midranks for ties.
library("Hmisc")
source("/Users/rcphelps/code/githubPublic/transect/libs/matrixTweaking.R")


  matrix.list <- rcorr(speciesMatrix, type="spearman")
  coef <- round(matrix.list$r, 2)
  pvals <- matrix.list$P
  if (nrow(coef) > 5) {
    heatmapSpearman <- makeDenseHeatmap(coef, "Spearman", addCoefficients=TRUE)
  } else {
    heatmapSpearman <- makeHeatmap(coef, "Spearman", addCoefficients=TRUE)
  }

  matrix.list <- rcorr(speciesMatrix, type="pearson")
  coef <- round(matrix.list$r, 2)
  pvals <- matrix.list$P
  if (nrow(coef) > 5) {
    heatmapPearson <- makeDenseHeatmap(coef, "Pearson", addCoefficients=TRUE)
  } else {
    heatmapPearson <- makeHeatmap(coef, "Pearson", addCoefficients=TRUE)
  }


  plotText <- makeGgplotTextObject( paste( 
    "\n\ndissimilarity matrix\n\n.... Spearman correlations are the Pearson\nlinear correlations computed on the ranks of\n", 
    "non-missing elements, using midranks for ties.\n( R > ?rcorr ) "), rotate=FALSE )

  longText <- makeGgplotTextObject(matrixToText(speciesMatrix), rotate=TRUE )
  # ( another approach: https://magesblog.com/post/2015-04-14-plotting-tables-alsongside-charts-in-r/ )

  multiplot(heatmapSpearman, heatmapPearson, plotText, longText, plotlist=NULL, cols=2, layout=NULL)


}

makeHeatmap <- function (data, type, addCoefficients) {

  # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
  
  # Reorder the correlation matrix
  data <- reorder_cor_matrix(data)
  
  library(reshape2)
  meltedData <- melt(get_upper_triangle(data))
  
  library(ggplot2)
  
  if (addCoefficients == TRUE) {    # add gemo_text()

    ggplotObject <- ggplot(data = meltedData, aes(Var2, Var1, fill = value)) +
      ggtitle(paste(type, "Correlation", sep = " ", collapse = NULL)) +
      geom_tile(color = "gray78") +
      scale_fill_gradient2(low = "magenta", high = "royalblue", mid = "pink", 
                         na.value = "gray93",
                         midpoint = 0, limit = c(-1,1), space = "Lab") +

      geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
    
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

  } else {

      ggplotObject <- ggplot(data = meltedData, aes(Var2, Var1, fill = value)) +
        ggtitle(paste(type, "Correlation", sep = " ", collapse = NULL)) +
        geom_tile(color = "gray78") +
        scale_fill_gradient2(low = "magenta", high = "royalblue", mid = "pink", 
                         na.value = "gray93",
                         midpoint = 0, limit = c(-1,1), space = "Lab") +

        ######################################
        # geom_text(aes(Var2, Var1, label = value, color = "black", size = 2, angle = 45)
        ######################################
    
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

    }

  
  return(ggplotObject)
}

makeDenseHeatmap <- function(data, type, addCoefficients) {

  # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
  
  # Reorder the correlation matrix
  data <- reorder_cor_matrix(data)
  
  library(reshape2)

  # meltedData <- melt(get_upper_triangle(data))
  # (don't remove any data)
  meltedData <- melt(data)
  
  library(ggplot2)
  
  if (addCoefficients == TRUE) {    # add gemo_text()

    ggplotObject <- ggplot(data = meltedData, aes(Var2, Var1, fill = value)) +
      ggtitle(paste(type, "Correlation", sep = " ", collapse = NULL)) +
      geom_tile(color = "gray78") +
      scale_fill_gradient2(low = "magenta", high = "royalblue", mid = "pink", 
                         na.value = "gray93",
                         midpoint = 0, limit = c(-1,1), space = "Lab") +

      geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
    
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

      legend.position = "none" ) +

      #legend.justification = c(1, 0),
      #legend.position = c(0.6, 0.7),
      # legend.position = c(0.6, 0.7),
      #legend.background = element_rect(fill = 'gray93', colour = 'black'),
      #legend.title = element_blank(),
      # legend.box = "horizontal",
      #legend.direction = "horizontal") +
      
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5)) +
      coord_fixed()

    } else {

      ggplotObject <- ggplot(data = meltedData, aes(Var2, Var1, fill = value)) +
        ggtitle(paste(type, "Correlation", sep = " ", collapse = NULL)) +
        geom_tile(color = "gray78") +
        scale_fill_gradient2(low = "magenta", high = "royalblue", mid = "pink", 
                         na.value = "gray93",
                         midpoint = 0, limit = c(-1,1), space = "Lab") +

        ######################################
        # geom_text(aes(Var2, Var1, label = value, color = "black", size = 2, angle = 45)
        ######################################
    
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

        legend.position = "none" ) +

        #legend.justification = c(1, 0),
        #legend.position = c(0.6, 0.7),
        # legend.position = c(0.6, 0.7),
        #legend.background = element_rect(fill = 'gray93', colour = 'black'),
        #legend.title = element_blank(),
        #legend.box = "horizontal",
        #legend.direction = "horizontal") +
      
        guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5)) +
        coord_fixed()

    }

 


  return(ggplotObject)
}


# write some text on a ggplot palette
makeGgplotTextObject <- function (incomingText, rotate) {
  
  library(ggplot2)
  
  if (nchar(incomingText == 0)) {
    text <- "incoming text was 0 length......"
  } else {
    # future: control the number of characters per line
  }

  textAngle = 0

  if (rotate == TRUE) {
    textAngle = 90
    posX = 5     # seems center justification is the default 
    posY = 0
  } else {
    posX = 0
    posY = 9
  }
  
  ggplotObject <- ggplot(data = NULL, aes()) +

    geom_tile(color = "gray78") +

    xlim(0,10) +
    ylim(0,10) +

    # https://community.rstudio.com/t/why-is-geom-text-size-in-mm/3587
    # "size=3" seems to accomodate 50 characters in a 2 colmn layout
    #geom_text(aes(fontface=3, label=incomingText, x=posX, y=posY, angle = textAngle), size=3, hjust = "inward") +  # italic
    geom_text(aes(fontface=3, label=incomingText, x=posX, y=posY, angle = textAngle), size=3, hjust = 0) +

    theme_bw() + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),      
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.position="none") +
      
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