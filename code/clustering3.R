



# library(ggplot2)
# library(grid) # ggplot legend adjustment

# Load the required packages
# (vegan must be loaded after ade4 to avoid some conflicts)
library(ade4)
library(vegan)
library(gclus)
library(cluster)
library(FD)


setwd("/Users/rcphelps/code/githubPublic/transect")
source("/Users/rcphelps/code/githubPublic/transect/libs/heatmapGraphics.R")
source("/Users/rcphelps/code/githubPublic/transect/libs/graphing_functions.R")
source("/Users/rcphelps/code/githubPublic/transect/libs/numericalEcologyWithR/coldiss-mod.R")
source("/Users/rcphelps/code/githubPublic/transect/libs/numericalEcologyWithR/panelutils.R")

library(RCurl)
source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb.csv")
vineyard.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)


# ****************************************************
# Compute 'correlation matrix' = 'dissimilarity matrix' in R
# ****************************************************
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# ****************************************************

#matrix <- data.matrix(vineyard.df, rownames.force = TRUE)
# the slow way
#matrix <- subset(matrix, select = -c(transect, X, Y, elevation.change, fall.line.exposure))
#matrix <- subset(matrix, select = -c(max.aphids, max.mealybugs, max.ants...mealybug, max.moths))
#matrix <- subset(matrix, select = -c(environ.var.1, environ.var.2, environ.var.3))

speciesMatrix <- data.matrix(vineyard.df, rownames.force = TRUE)

# pretend that we have data from multiple transects; use only transect "1a"
# speciesMatrix <- subset(speciesMatrix, transect= "1a") <-- does not work

# now, in the other dimension, remove rows representing data in the "margin"
speciesMatrix <- speciesMatrix[speciesMatrix[,2] != -5,]
speciesMatrix <- speciesMatrix[speciesMatrix[,2] != -4,]
speciesMatrix <- speciesMatrix[speciesMatrix[,2] != -3,]
speciesMatrix <- speciesMatrix[speciesMatrix[,2] != -2,]
speciesMatrix <- speciesMatrix[speciesMatrix[,2] != -1,]
# finally (the fast way)
speciesMatrix <- subset(speciesMatrix, select = c(aphids, mealybugs, ants, moths, bats))

# instead of correlating by species, correlate by field position
speciesMatrix <- t(speciesMatrix)


heatmapGenerate(speciesMatrix)



# ***********************************************************************
# dissimilarity, ordination, ....
# http://ordination.okstate.edu/overview.htm
# ***********************************************************************


# ***********************************************************************
# Q-mode dissimilarity and distance measures for (semi-)quantitative data
# Numerical Ecology with R : chapter 3
# ***********************************************************************
# The concept of Dissimilarity may be used in a more general way, to determine the pairwise 
# difference between samples. The similarity notion is a key concept for Clustering, in the 
# way to decide which clusters should be combined or divided when observing sets.
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Dissimilarity_Matrix_Calculation
# ***********************************************************************

# Percentage difference (Bray-Curtis) dissimilarity matrix
# on raw species data
speciesMatrix.db <- vegdist(speciesMatrix)	# method="bray" (default)
head(speciesMatrix.db)
# Percentage difference (Bray-Curtis) dissimilarity matrix on raw species abundance data
dev.new(title="Percentage difference (Bray-Curtis), raw data", width=10, height=5)
coldiss2(speciesMatrix.db, "\n(Bray-Curtis)", byrank=FALSE, diag=TRUE)


# ***********************************************************************
# giving NMDS ordination a shot with ggplot....
# (rank order correlation via Nonmetric Multidimensional Scaling )
# https://oliviarata.wordpress.com/2014/04/17/ordinations-in-ggplot2/
# ***********************************************************************


# oliviarata wants species data and environmental data separated
#


# see ordinationNMDS.R
# > source("/Users/rcphelps/code/githubPublic/transect/code/ordinationNMDS.R")
# > source("/Users/rcphelps/code/githubPublic/transect/libs/graphing_functions.R")
# > source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb.csv")
df.list <- nmdsDataSplit(source.url)

species <- df.list$species
env <- df.list$env

plot.list <- nmdsInitialize(species, env)


multiplot(plot.list$Stress, plot.list$Ordination, plotlist=NULL, cols=2, layout=NULL)

