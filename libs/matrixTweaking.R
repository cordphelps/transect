#
matrixToText <- function(matrix) {

	rows = nrow(matrix)
	cols = ncol(matrix)

	columnNameArray <- colnames(matrix, do.NULL=TRUE, prefix="col")
 	#print(columnNameArray)

 	colNameString = ""

	for ( j in 1:cols ) {      #   print(matrix[i,])}   prints the col names for each row
		
		if (j == 1) {

			colNameString <- toString(columnNameArray[j]) # avoid inserting a leading tab

		} else {
			# https://stackoverflow.com/questions/33047601/paste-collapse-with-tabs
			# https://gist.github.com/briandk/d9231ba1e2603eed0df1
			colNameString <- paste(c(colNameString, toString(columnNameArray[j])), collapse = ">   <")
			# table don't work, see below
			# colNameString <- paste(c(colNameString, toString(columnNameArray[j])), collapse = "\t")
		}

	} 

	print(colNameString)

	niceTextBlock <- paste(colNameString, "\n", sep = "", collapse = NULL)

	for ( i in 1:rows ) { 

		niceTextBlock <- paste(niceTextBlock, readRow(matrix, i, cols), sep = "\n", collapse = NULL)

	}

	niceTextBlock <- paste(niceTextBlock, "\n", sep = "", collapse = NULL)


	#print(niceTextBlock)

	return(niceTextBlock)
}


readRow <- function(matrix, rowNumber, numberColumns) {
	# read each element of this row, concatenating them with 
	# a tab seperator, and returning the new string

	# UPDATE: After posting the issue to the ggplot2 GitHub, the problem is that R does not allow tab characters in graphics devices.
	# https://stackoverflow.com/questions/30923852/include-t-character-in-facet-strip-text-with-custom-labeller

	string <- ""

	for (i in 1:numberColumns) { 

		#print(toString(matrix[rowNumber,i], width = NULL))

		if (i == 1) {

			string <- strtrim(toString(matrix[rowNumber,i], width = NULL), 7) # avoid inserting a leading tab

		} else {
			# string <- paste(string, "\t", strtrim(toString(matrix[rowNumber,i], width = NULL), 7), sep="", collapse = NULL)
			string <- paste(string, ">  <", strtrim(toString(matrix[rowNumber,i], width = NULL), 7), sep="", collapse = NULL)
		}
	}

	#print(string)
	
	return(string)
}