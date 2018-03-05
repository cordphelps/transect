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
  
ggplotObject


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

acq.freq <- 100  # data acquisition (sample rate) frequency (Hz) 
                 # the number of samples in the sample interval
time     <- 1    # sampling time interval (seconds) 
                 # fundamental period or sample period (should be > 1 hz)


ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time

dc.component <- 0
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


################################################################################
################################################################################
# waves
# http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html
################################################################################


xs <- seq(1.5*pi,3.5*pi,pi/100)
xs <- seq(2*pi,3*pi,pi/100)  # 
xs <- seq(0*pi,2*pi,pi/100) # traditional, high/low
xs <- seq(1*pi,3*pi,pi/100) # traditional, low/high
xs <- seq(1.5*pi,3.5*pi,pi/100) # traditional, lowest/highest/lowest

xs <- seq(0,3,.05)
wave.1 <- log(xs)
#wave.1 <- sin(xs)
#plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.1,type="l",xlim=c(0,3)); abline(h=c(0,1,2.8),v=c(1,2.8),lty=3)

################################################################################

xs <- seq(-2*pi,2*pi,pi/100)
wave.1 <- sin(2*xs)
wave.2 <- sin(3*xs)
#par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)

sineFunction <- function(x) {
    sin(x) * .5
}

ggplot(data.frame(x = c(-.5*pi, 1.5*pi)), aes(x = x)) +
        stat_function(fun = sineFunction)  # <-- superimpose a function on an existing plot


count.df <- data.frame(seq(-.5*pi, 3.5*pi, .05))
count(count.df)
rowCount<-252
pi.df <- data.frame(pi=numeric(rowCount), line1=numeric(rowCount), line2=numeric(rowCount))
pi.df$pi <- seq(-.5*pi, 3.5*pi, .05)
pi.df$line1 <- sin(pi.df$pi)
pi.df$line2 <- data.frame(sin(5*pi.df$pi))
pi.df$line3 <- data.frame(sin(10*pi.df$pi))

# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
colors <- c("#0072B2", "#D55E00", "purple", "#009E73")

ggplotObject <- ggplot() +   

    geom_line(aes(x = pi, y=.5*line1), pi.df, colour = colors[1], size=3, alpha=0.9) +
    geom_line(aes(x = pi, y=.3*line2), pi.df, colour = colors[2], size=3, alpha=0.8) + 
    geom_line(aes(x = pi, y=.2*line3), pi.df, colour = colors[3], size=3, alpha=0.8) +
    geom_line(aes(x = pi, y=.5*line1+.3*line2+.2*line3), pi.df, colour = "black", size=2, alpha=0.8) +
    ylim(-4,4) +
    theme_bw()

pi.df$line4 <- data.frame(sign(sin(7*pi.df$pi))) - data.frame(sign(sin(12*pi.df$pi)))
pi.df$line5 <- data.frame(sign(sin(12*pi.df$pi)))
#pi.df$line4 <- data.frame(sin(2*pi.df$pi))
#pi.df$line5 <- data.frame(sin(3*pi.df$pi))
ggplot() +   
    geom_line(aes(x = pi, y=line4), pi.df, colour = "green", size=1, alpha=0.9) +
    #geom_line(aes(x = (pi-.7854), y=line5), pi.df, colour = "blue", size=1, alpha=0.8) +
    geom_line(aes(x = (pi-.9), y=line5), pi.df, colour = "blue", size=1, alpha=0.8) +
    ylim(-4,4) +
    scale_x_continuous(breaks = round(seq(min(0), max(1.5*pi), by = pi),1)) +
    geom_vline(xintercept=pi) +
    geom_vline(xintercept=0) +
    geom_vline(xintercept=2*pi) +
    geom_vline(xintercept=3*pi)


    geom_line(aes(x = x, y=sin(1*x)), data.frame(x = seq(-.5*pi, 1.5*pi, .05)), colour = "black", size=2, alpha=0.9) +
    geom_line(aes(x = x, y=sin(200*x)), data.frame(x = seq(-.5*pi, 1.5*pi, .05)), colour = "green", size=3, alpha=0.8) +
    geom_line(aes(x = x, y=sin(300*x)), data.frame(x = seq(-.5*pi, 1.5*pi, .05)), colour = "blue", size=3, alpha=0.8) +
    geom_line(aes(x = x, y=sin(1*x)+sin(200*x)+sin(300*x)), data.frame(x = seq(-.5*pi, 1.5*pi, .05)), colour = "black", size=1, alpha=0.9) +
    ylim(-5,5)  
    stat_function(fun = sineFunction)
    geom_point(aes(x=df1$X, y=df1$Y), df1, size=df1$mealybugs, shape=21, colour = "purple", fill = "green", alpha=0.6) +
