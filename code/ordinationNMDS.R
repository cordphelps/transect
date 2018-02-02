
nmdsDataSplit <- function(source.url) {

# https://oliviarata.wordpress.com/2014/04/17/ordinations-in-ggplot2/

# 'dune' = species (a list of dataframes): observation, count

#> class(dune)
#[1] "data.frame"
#> class(dune[1])
#[1] "data.frame"
#> 
	# > dune[1]
    # Achimill
    # 1         1
    # 2         3
    # 3         0
    # 4         0
    # 5         2
    # 6         2
    # 7         2
    # 8         0
    # 9         0
    # 10        4
    # 11        0
    # 12        0
    # 13        0
    # 14        0
    # 15        0
    # 16        0
    # 17        2
    # 18        0
    # 19        0
    # 20        0

# 
#     dune.env, a list of data frames of 20 observations on the following 5 variables:
				# A1 (thickness)
				# Moisture (ordered factor)
				# Management (factor with levels)
				# Use (ordered factor)
				# Manure (factor with levels)
				# http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/dune.html
# 
# > class(dune.env)
# [1] "data.frame"
# > class(dune.env[1])
# [1] "data.frame"
# > dune.env[1]
#      A1
# 1   2.8
# 2   3.5
# 3   4.3
# 4   4.2
# 5   6.3
# 6   4.3
# 7   2.8
# 8   4.2
# 9   3.7
# 10  3.3
# 11  3.5
# 12  5.8
# 13  6.0
# 14  9.3
# 15 11.5
# 16  5.7
# 17  4.0
# 18  4.6
# 19  3.7
# 20  3.5

# clean re-build of vineyard.df
#
# individual observations contain transect observations which includes 
# observation, X, Y, species, and environmental data
#
big.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)
#
# step 1: read species columns and create separate dataframes
matrixBig <- data.matrix(big.df, rownames.force = TRUE)

matrixAphids <- subset(matrixBig, select = c(aphids))
matrixMealybugs <- subset(matrixBig, select = c(mealybugs))
matrixAnts <- subset(matrixBig, select = c(ants))
matrixMoths <- subset(matrixBig, select = c(moths))

dfAphids.df <- as.data.frame(matrixAphids)
dfMealybugs.df <- as.data.frame(matrixMealybugs)
dfAnts.df <- as.data.frame(matrixAnts)
dfMoths.df <- as.data.frame(matrixMoths)

dfSpecies.df <- data.frame(dfAphids.df, dfMealybugs.df, dfAnts.df, dfMoths.df)

matrixEv1 <- subset(matrixBig, select = c(ev1))
matrixEv2 <- subset(matrixBig, select = c(ev2))
matrixEv3 <- subset(matrixBig, select = c(ev3))	

dfEv1.df <- as.data.frame(matrixEv1)
dfEv2.df <- as.data.frame(matrixEv2)
dfEv3.df <- as.data.frame(matrixEv3)

dfEv.df <- data.frame(dfEv1.df, dfEv2.df, dfEv3.df)


return(list(species=df.Species.df, env=dfEv.df))

}



nmdsInitialize <- function(species, env) {

#data
#data(dune)
#data(dune.env)  

dune <- species
dune.env <- env
 
#set the seed:
set.seed(201) # this allows us to reproduce the same result in the future
meta.nmds.dune <- metaMDS(dune) #no transformation of species data is made here prior to bray curtis dissimilarities being calculated. (Bray Curtis is the default in R).
str(meta.nmds.dune) # gives stress value for plot
stressplot(meta.nmds.dune) # To gain the stress plot for stress values for your MDS

#envfit
dune.envfit <- envfit(meta.nmds.dune, env = dune.env, perm = 999) #standard envfit
dune.envfit

#data for plotting 
##NMDS points
dune.NMDS.data<-dune.env #there are other ways of doing this. But this is the way I do it for ease of plotting
dune.NMDS.data$NMDS1<-meta.nmds.dune$points[ ,1] #this puts the NMDS scores for the plots into a new dataframe. you could put them into an existing one if you preferred.
dune.NMDS.data$NMDS2<-meta.nmds.dune$points[ ,2] 

##species data
stems<-colSums(dune) #total abundances for each species
spps <- data.frame(scores(meta.nmds.dune, display = "species")) #dataframe of species scoes for plotting
spps$species <- row.names(spps) # making a column with species names
spps$colsums <- stems #adding the colSums from above
spps<-spps[!is.na(spps$NMDS1) & !is.na(spps$NMDS2),] #removes NAs
spps.colmedian <- median(spps$colsums) #create an object that is the median of the abundance of the measured species
spps.colmean <- mean(spps$colsums) #creates a mean instead if you wish to use
spps2 <- subset(spps,spps$colsums > spps.colmean) #select the most abundant species. Could discard fewer by going something like - spps$colsums>(spps.colmedian/2) instead
spps2$species <- factor(spps2$species) #otherwise factor doesn't drop unused levels and it will throw an error
 
# data for the envfit arrows
env.scores.dune <- as.data.frame(scores(dune.envfit, display = "vectors")) #extracts relevant scores from envifit
env.scores.dune <- cbind(env.scores.dune, env.variables = rownames(env.scores.dune)) #and then gives them their names
 
# function for ellipsess - just run this, is used later
#taken from the excellent stackoverflow Q+A: http://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
 
#data for ellipse, in this case using the management factor
df_ell.dune.management <- data.frame() #sets up a data frame before running the function.
for(g in levels(dune.NMDS.data$Management)){
  df_ell.dune.management <- rbind(df_ell.dune.management, cbind(as.data.frame(with(dune.NMDS.data [dune.NMDS.data$Management==g,],
                                  veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2)))))
                                  ,Management=g))
}

 
# data for labelling the ellipse
NMDS.mean.dune=aggregate(dune.NMDS.data[ ,c("NMDS1", "NMDS2")], 
                         list(group = dune.NMDS.data$Management), mean)
 
# data for labelling the ellipse
NMDS.mean=aggregate(dune.NMDS.data[,c("NMDS1", "NMDS2")], 
                    list(group = dune.NMDS.data$Management), mean)
 

nmdsGraphics(dune.NMDS.data, NMDS1, NMDS2, NMDS.mean, env.scores.dune)

}


nmdsGraphics <- function(dune.NMDS.data, NMDS1, NMDS2, NMDS.mean, env.scores.dune) {

# https://oliviarata.wordpress.com/2014/04/17/ordinations-in-ggplot2/

## finally plotting. 
mult <- 2 #multiplier for the arrows and text for envfit below. You can change this and then rerun the plot command.

(dune.nmds.gg1 <- ggplot(data = dune.NMDS.data, aes(y = NMDS2, x = NMDS1))+ #sets up the plot. brackets around the entire thing to make it draw automatically
   geom_path(data = df_ell.dune.management, aes(x = NMDS1, y = NMDS2, group = Management, alpha=Management))+ #this is the ellipse, seperate ones by Site. If you didn't change the "alpha" (the shade) then you need to keep the "group 
   scale_alpha_manual(guide = FALSE,values=c(0.3, 0.5, 0.7, 0.9))+ #sets the shade for the ellipse
   geom_point(aes(shape = Management), size = 3) + #puts the site points in from the ordination, shape determined by site, size refers to size of point
  # geom_text(data=spps2, aes(x=spps2$NMDS1, y=spps2$NMDS2, label=species), size = 3.3, hjust=1.1)+ #labelling the species. hjust used to shift them slightly from their points
      annotate("text",x = NMDS.mean$NMDS1,y = NMDS.mean$NMDS2,label=NMDS.mean$group) + #labels for the centroids - I haven't used this since we have a legend. but you could also dithc the legend, but plot will get v messy
       geom_segment(data = env.scores.dune,
                    aes(x = 0, xend = mult*NMDS1, y = 0, yend = mult*NMDS2),
                    arrow = arrow(length = unit(0.25, "cm")), colour = "grey") + #arrows for envfit.  doubled the length for similarity to the plot() function. NB check ?envfit regarding arrow length if not familiar with lengths
       geom_text(data = env.scores.dune, #labels the environmental variable arrows * "mult" as for the arrows
                 aes(x = mult*NMDS1, y = mult*NMDS2, label=env.variables),
                 size = 5,
                 hjust = -0.5)+
   #geom_point(data=spps2, alpha = .6, shape = 4)+ #these are the species points, made lighter and a specific shape
   scale_shape_manual(values = c(1,8,19,5))+ #sets the shape of the plot points instead of using whatever ggplot2 automatically provides
   coord_cartesian(xlim = c(-1,1.5))+  ## NB this changes the visible area of the plot only (this is a good thing, apparently). Can also specify ylim. Here in case you want to set xaxis manually.
   theme_bw())
 
 
# NB I usually use the "pdf" function in rstudio to export the plot. But you can also:
 
# ggsave(plot = dune.nmds.gg1, filename = "FILENAME.pdf", path = "PATH") #if you don't specify the plot it will save the last one

##many other options - see ?ggsave.  Can set dimensions, units, dpi

}