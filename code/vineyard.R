



library(ggplot2)
library(grid) # ggplot legend adjustment

source("/Users/rcphelps/code/githubPublic/transect/libs/graphing_functions.R")

library(RCurl)

#spatial.df <- read.csv(text=getURLContent(source.url), header=TRUE)


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

source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb2.csv")
vineyard2.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)
#bugList2 <- data.frame(vineyard2.df$X, vineyard2.df$Y, vineyard2.df$aphids, vineyard2.df$mealybugs, vineyard2.df$ants, vineyard2.df$moths, vineyard2.df$bats)

#source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb5.csv")
#vineyard5.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)

source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb8.csv")
vineyard8.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)

vineyardDfList <- list(vineyard2.df, vineyard5.df, vineyard8.df)


#bugGGfromList(vineyard.df, bugListDataPoints, title="hoser")
ggPlotObject <- bugGGfromList(vineyard2.df, vineyard5.df, vineyard8.df, title="hoser")
multiplot(ggPlotObject, plotlist=NULL, cols=2, layout=NULL)

stop()

# New graphic window (size 9x9 inches)
#dev.new(title="Species Locations", width=9, height=9)
# Divide the plot window into 4 frames, 2 per row
#par(mfrow=c(2,2))
#bugPlot(vineyard.df$aphids, spatial.df, "aphids")
#bugPlot(vineyard.df$mealybugs, spatial.df, "mealy bugs")
#bugPlot(vineyard.df$ants, spatial.df, "ants")
#bugPlot(vineyard.df$moths, spatial.df, "moths")

source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb5.csv")
vineyard.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)

ggplotAphid <- bugGG(vineyard.df, vineyard.df$X, vineyard.df$Y, vineyard.df$aphids, "aphid gradient", "aphid count")
ggplotMealy <- bugGG(vineyard.df, vineyard.df$X, vineyard.df$Y, vineyard.df$mealybugs, "mealy bug gradient", "mealy bug count")
ggplotAnts <- bugGG(vineyard.df, vineyard.df$X, vineyard.df$Y, vineyard.df$ants, "ant gradient", "ant count")
ggplotMoths <- bugGG(vineyard.df, vineyard.df$X, vineyard.df$Y, vineyard.df$moths, "moth gradient", "moth count")

multiplot(ggplotAphid, ggplotMealy, ggplotAnts, ggplotMoths, plotlist=NULL, cols=2, layout=NULL)


