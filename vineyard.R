



library(ggplot2)
library(grid) # ggplot legend adjustment

source("/Users/rcphelps/code/githubPublic/transect/graphing_functions.R")

library(RCurl)
source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/hvb.csv")
vineyard.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)
spatial.df <- read.csv(text=getURLContent(source.url), header=TRUE)


# Overall distribution of abundances (dominance codes)
# ****************************************************

# Minimum and maximum of abundance values in the whole data set
# range(vineyard.df)  # only defined on a data frame with all numeric variables
# Count cases for each abundance class
(ab <- table(unlist(vineyard.df)))
# Create a graphic window with title
dev.new(title="Distribution of abundance classes")
# Barplot of the distribution, all species confounded
barplot(ab, las=1, xlab="Abundance class", ylab="Frequency", col=gray(5:0/5))
# Number of absences
sum(vineyard.df==0)
# Proportion of zeros in the community data set
sum(vineyard.df==0)/(nrow(vineyard.df)*ncol(vineyard.df))


# Map of the locations of the sites
# *********************************

# New graphic window 
dev.new(title="Site Locations")
# Create an empty frame (proportional axes 1:1, with titles)
# Geographic coordinates x and y from the spa data frame
plot(spatial.df$X, spatial.df$Y, asp=NA, type="n", main="Site Locations", xlab="x 	coordinate (km)", ylab="y coordinate (km)")
# Add a blue line connecting the sites (Doubs river)
lines(spatial.df$X, spatial.df$Y, col="blue")

# create a vertical line separating the edge from the vineyard
yAxisPoints <- seq.int(1, nrow(spatial.df) , 1)
lines(rep.int(0, length(yAxisPoints)), yAxisPoints, col="green")

# Add site labels
text(spatial.df$X, spatial.df$Y, row.names(spatial.df), cex=0.8, col="red")
# Add text blocks 
text(-3, 4, "margin", cex=1.2, col="red")
text(10, 6, "vineyard\ninterior", cex=1.2, col="red")

# New graphic window (size 9x9 inches)
#dev.new(title="Species Locations", width=9, height=9)
# Divide the plot window into 4 frames, 2 per row
#par(mfrow=c(2,2))
#bugPlot(vineyard.df$aphids, spatial.df, "aphids")
#bugPlot(vineyard.df$mealybugs, spatial.df, "mealy bugs")
#bugPlot(vineyard.df$ants, spatial.df, "ants")
#bugPlot(vineyard.df$moths, spatial.df, "moths")

ggplotAphid <- bugGG(vineyard.df, vineyard.df$X, vineyard.df$Y, vineyard.df$aphids, "aphid gradient", "aphid count")
ggplotMealy <- bugGG(vineyard.df, vineyard.df$X, vineyard.df$Y, vineyard.df$mealybugs, "mealy bug gradient", "mealy bug count")
ggplotAnts <- bugGG(vineyard.df, vineyard.df$X, vineyard.df$Y, vineyard.df$ants, "ant gradient", "ant count")
ggplotMoths <- bugGG(vineyard.df, vineyard.df$X, vineyard.df$Y, vineyard.df$moths, "moth gradient", "moth count")

multiplot(ggplotAphid, ggplotMealy, ggplotAnts, ggplotMoths, plotlist=NULL, cols=2, layout=NULL)


