

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



# ***********************************************************************
# giving NMDS ordination a shot with ggplot....
# (rank order correlation via Nonmetric Multidimensional Scaling )
# https://oliviarata.wordpress.com/2014/04/17/ordinations-in-ggplot2/
# ***********************************************************************

# ****************************************************
# Compute 'correlation matrix' = 'dissimilarity matrix' in R
# ****************************************************
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# ****************************************************


source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb5.csv")

speciesMatrix <- checkSpeciesMatrix(source.url)

heatmapGenerate(speciesMatrix)
# instead of correlating by species, correlate by field position
speciesMatrix <- t(speciesMatrix)
heatmapGenerate(speciesMatrix)





# oliviarata wants species data and environmental data separated
#


# see ordinationNMDS.R
# > source("/Users/rcphelps/code/githubPublic/transect/code/ordinationNMDS.R")
# > source("/Users/rcphelps/code/githubPublic/transect/libs/graphing_functions.R")
# > source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb5.csv")
df.list <- nmdsDataSplit(source.url)

species <- df.list$species
env <- df.list$env

plot.list <- nmdsInitialize(species, env)


multiplot(plot.list$Stress, plot.list$Ordination, plotlist=NULL, cols=2, layout=NULL)

