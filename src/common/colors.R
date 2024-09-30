###############################################################################
# Colors used in the plots.
###############################################################################
CAT_COLORS_8 <- c(# basic color brewer palette, see http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
	rgb(228,26,28,maxColorValue=255),		# red
	rgb(77,175,74,maxColorValue=255),		# green
	rgb(152,78,163,maxColorValue=255),		# purple
	rgb(255,127,0,maxColorValue=255),		# orange
	rgb(166,86,40,maxColorValue=255),		# brown
	rgb(247,129,191,maxColorValue=255),		# pink
	rgb(255,255,51,maxColorValue=255),		# yellow
	rgb(55,126,184,maxColorValue=255)		# blue
)

lv.nbr <- 6		# for 6 levels of gray
lvs <- as.integer(seq(from=0,to=255,by=255/(lv.nbr+1))[2:(lv.nbr+1)])
GRAY_LEVELS <- sapply(lvs, function(lv) rgb(lv,lv,lv,max=255))




#############################################################
# Combines two colors using a weighted sum of their RGB chanels.
#
# col1: first color.
# col2: second color.
# transparency: alpha level of the first color (percent).
#				0 means pure 1st color, 100 is pure 2nd color.
#
# returns: color resulting from the combination.
#############################################################
combine.colors <- function(col1, col2, transparency=50)
{	transp <- transparency/100.0
	
	# convert to RGB triplet
	rgb1 <- col2rgb(col1, alpha=TRUE)
	rgb2 <- col2rgb(col2, alpha=TRUE)
	
	# create new color using specified transparency
	res <- rgb(
			transp*rgb1[1] + (1-transp)*rgb2[1], 
			transp*rgb1[2] + (1-transp)*rgb2[2], 
			transp*rgb1[3] + (1-transp)*rgb2[3],
			max=255,
			alpha=transp*rgb1[4] + (1-transp)*rgb2[4]
	)
	
	return(res)
}
