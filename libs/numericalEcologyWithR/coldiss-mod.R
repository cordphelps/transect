# coldiss()
# Color plots of a dissimilarity matrix, without and with ordering
#
# License: GPL-2 
# Author: Francois Gillet, 23 August 2012
#

"coldiss2" <- function(D, type, nc = 4, byrank = TRUE, diag = FALSE)
{
	require(gclus)

	if (max(D)>1) D <- D/max(D)

	if (byrank) {
	  # Accepts a dissimilarity matrix or dist m, and returns a matrix of colors.
	  # library = gclus
		spe.color <- dmat.color(1-D, cm.colors(nc))
	}
	else {
		spe.color <- dmat.color(1-D, byrank=FALSE, cm.colors(nc))
	}

	spe.o <- order.single(1-D)
	speo.color <- spe.color[spe.o, spe.o]
	
	op <- par(mfrow=c(1,2), pty="s")

	if (diag) {
		plotcolors(spe.color, rlabels=attributes(D)$Labels, 
			#main="Dissimilarity Matrix", 
			main = paste("Dissimilarity Matrix", type, sep = " ", collapse = NULL),
			dlabels=attributes(D)$Labels)
		plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
			# main="Ordered Dissimilarity Matrix", 
			main = paste("Ordered Dissimilarity Matrix", type, sep = " ", collapse = NULL),
			dlabels=attributes(D)$Labels[spe.o])
	}
	else {
		plotcolors(spe.color, rlabels=attributes(D)$Labels, 
			#main="Dissimilarity Matrix")
	    main = paste("Dissimilarity Matrix", type, sep = " ", collapse = NULL))
		plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
			#main="Ordered Dissimilarity Matrix")
		  main = paste("Ordered Dissimilarity Matrix", type, sep = " ", collapse = NULL))
	  }

	par(op)
}

# Usage:
# coldiss(D = dissimilarity.matrix, nc = 4, byrank = TRUE, diag = FALSE)
# If D is not a dissimilarity matrix (max(D) > 1), then D is divided by max(D)
# nc 							number of colours (classes)
# byrank= TRUE		equal-sized classes
# byrank= FALSE		equal-length intervals
# diag = TRUE			print object labels also on the diagonal

# Example:
# coldiss(spe.dj, nc=9, byrank=F, diag=T)
