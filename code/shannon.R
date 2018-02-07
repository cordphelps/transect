#
# Load the required packages
# (vegan must be loaded after ade4 to avoid some conflicts)
library(ade4)
library(vegan)
library(RCurl)



source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb5.csv")
v5.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)
v5.orig.df <- v5.df

# exclude extraneous columns
v5.df <- subset(v5.df, select = c(aphids, mealybugs, ants, moths, bats))


#######################################################################
# for each observation; normalize the counts, then create shannon index
#######################################################################

# get a blank matrix
normalized <- matrix(NA,nrow(v5.df),ncol(v5.df))
# normalize the species data for the row
for (i in 1:nrow(v5.df)) {
	sum <- apply(X=v5.df[i,], MARGIN=1, FUN=sum)

	for (j in 1:ncol(v5.df)) {
		normalized[i,j] <- v5.df[i,j]/sum   
	}

}

# https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf
shannonData <- diversity(normalized)
xy <- subset(v5.orig.df, select = c(X, Y))
shannon.df <- cbind(xy, shannonData)   # that is the shannon biodiversity index with plottable coordinates for the transect


#######################################################################
# plot
#######################################################################

library(ggplot2)
library(grid) # ggplot legend adjustment

ggplotObject <- ggplot()  +
    geom_point(aes(x=shannon.df$X, y=shannon.df$Y, size=shannon.df$shannonData), shannon.df,  
    	shape=21, colour = "purple", fill = "plum", alpha=0.6) +
    scale_size_continuous(range = c(0, 102)) +
    #scale_fill_continuous(low = "plum1", high = "purple4") +

    #scale_y_continuous(breaks = seq(4, 6, 2)) +
    ggtitle("shannon index") +
    labs(x = "field margin offset", y = "transect") +

    annotate("rect", xmin=-5, xmax=0, ymin=4,ymax=6, alpha=0.2, fill="red") +
    #annotate("rect", xmin=0, xmax=15, ymin=0,ymax=10, alpha=0.2, fill="green") +
    theme_bw() +
    theme() + 
    # theme(legend.position = "bottom", legend.direction = "horizontal") +
    theme(legend.position = "none", legend.direction = "horizontal") +
    theme(legend.box = "horizontal", legend.key.size = unit(1, "cm")) 
  

#######################################################################
# fourier transform
#######################################################################
# http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html

  library(stats)
  mat<-as.matrix(shannon.df, dimnames=NA)
  X.k <- fft(mat)

  # chop off the x and y dimensions leaving the shannon "signal"
  X.k<-subset(X.k, select=-c(X,Y))

  plot.frequency.spectrum(X.k, xlimits=c(0,20))

	

  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))

  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))






#source("/Users/rcphelps/code/githubPublic/transect/libs/fourierFunctions.R")
setup <- function(acq.freq, time) {

ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time

dc.component <- 1
component.freqs <- c(3,7,10)        # frequency of signal components (Hz)
component.delay <- c(0,0,0)         # delay of signal components (radians)
component.strength <- c(1.5,.5,.75) # strength of signal components

f   <- function(t,w) { 
  dc.component + 
  sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts=ts)
w <- 2*pi*f.0
trajectory <- sapply(ts, function(t) f(t,w))
head(trajectory,n=30)
	X.k <- fft(trajectory)                   # find all harmonics with fft()
plot.frequency.spectrum(X.k, xlimits=c(0,20))

}


