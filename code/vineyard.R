



library(ggplot2)
library(grid) # ggplot legend adjustment

source("/Users/rcphelps/code/githubPublic/transect/libs/graphing_functions.R")

library(RCurl)
source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb.csv")
vineyard.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)
spatial.df <- read.csv(text=getURLContent(source.url), header=TRUE)


# Overall distribution of abundances (dominance codes)
# ****************************************************

# Minimum and maximum of abundance values in the whole data set
# range(vineyard.df)  # only defined on a data frame with all numeric variables
# Count cases for each abundance class
# (ab <- table(unlist(vineyard.df)))    # <-- prints a table to the console
# Number of absences
# sum(vineyard.df==0)
# Proportion of zeros in the community data set
# sum(vineyard.df==0)/(nrow(vineyard.df)*ncol(vineyard.df))

source("/Users/rcphelps/code/githubPublic/transect/libs/graphing_functions.R")
bugListDataPoints <- list(vineyard.df$Y, vineyard.df$aphids, vineyard.df$mealybugs, vineyard.df$ants, vineyard.df$moths)
bugGGfromList(vineyard.df, bugListDataPoints, title="hoser")

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


