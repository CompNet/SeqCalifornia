#############################################################################################
# Returns the duration, in days, of the intersection between the periods, or zero if they are
# exactly consecutive (i.e. one starts exactly one day after the other ends), or NA if they do
# not intersect at all.
#
# start1: start date of the first period.
# end1: end date of the first period.
# start2: start date of the second period.
# end2: end date of the second period.
#
# returns: duration of the intersection, zero if they are consecutive, or NA if there is no overlap.
#############################################################################################
date.intersect.val <- function(start1, end1, start2, end2)
{	#cat(format(start1),"--",format(end1)," vs ",format(start2),"--",format(end2),"\n",sep="")
	
	if(is.na(start1))
		start1 <- min(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(start2))
		start2 <- min(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(end1))
		end1 <- max(c(start1,end1,start2,end2), na.rm=TRUE)
	if(is.na(end2))
		end2 <- max(c(start1,end1,start2,end2), na.rm=TRUE)
	
	result <- min(end1,end2) - max(start1,start2) + 1
	if(result<=0)
		result <- NA
	
	return(result)
}




#############################################################################################
# Converts the mandate table into a Traminer-compatible data structure.
# 
# tab.mandates: mandate table.
# start.date: beginning of the period to process.
# end.date: end of the period to process.
# left: left alignment of the sequences ("DEL" = all aligned on their first state, NA = keep 
#	    the sequences as they are).
# colors: colors used when plotting the sequences.
#
# returns: a Traminer object.
#############################################################################################
convert.to.sequences <- function(tab.mandates, start.date=as.Date("2000/1/1"), end.date=Sys.Date(), left=NA, colors=CAT_COLORS_8)
{	cat("Converting data to sequences \n")
	
	COL_ID <- "Id"
	COL_START <- "Beginning term"
	COL_END <- "End of term"
	COL_OFFICE <- "Office"
	COL_SEQ <- "Sequence"
	
	# temporal resolution
	granularity <- "year"
	dates <- seq(start.date, end.date, granularity)
	semi.duration <- as.integer((dates[2]-dates[1])/2)
	empty.seq <- paste(rep(NA,length(dates)-1), collapse="-")

	# short version of the mandate names
	short.names <- c()
	short.names["USR"] <- "USR"
#	short.names["US Senate"] <- "USS"
	short.names["State Assembly"] <- "StA"
	short.names["State Senate"] <- "StS"
#	short.names["State Board"] <- "StB"
	short.names["M"] <- "M"
	short.names["CC"] <- "CC"
	short.names["BS"] <- "BS"
	
	# at first, let's just use the mandate names
	mdt.order <- c("USR", "USS", "StA", "StS", "StB", "M", "CC", "BS")
	unique.ids <- sort(unique(tab.mandates[,COL_ID]))
	cat("  Processing each id separately \n")
	seqs <- t(sapply(1:length(unique.ids), function(j)
	{	id <- unique.ids[j]
		cat("    Processing id ",id, " (",j,"/",length(unique.ids),")\n",sep="")
		
		# retrieve the date for the current id
		rows <- which(tab.mandates[,COL_ID]==id)
		tmp <- tab.mandates[rows,c(COL_OFFICE,COL_START,COL_END)]
		#print(tmp)
		
		# convert mandate names to short form
		tmp[,COL_OFFICE] <- short.names[tmp[,COL_OFFICE]]
		
		# complete missing end dates
		no.end.date <- which(is.na(tmp[,COL_END]))
		tmp[no.end.date,COL_END] <- rep(end.date, length(no.end.date))
		
		# order by mandate dates and types
		mnd.types <- match(tmp[,COL_OFFICE], mdt.order)
		idx <- order(tmp[,COL_START], mnd.types)
		mnd.types <- mnd.types[idx]
		tmp <- tmp[idx,]
		
		# adjust dates to avoid overlapping between different types of mandates
		if(nrow(tmp)>1)
		{	i <- 1
			while(i<nrow(tmp))
			{	#cat("      row ",i,"----------------------------\n")
				#print(tmp)
				
				if(mnd.types[i]>mnd.types[i+1])
				{	tmp[i,COL_END] <- min(tmp[i,COL_END], tmp[i+1,COL_START]-1)
					if(tmp[i,COL_START]>tmp[i,COL_END])
					{	tmp <- tmp[-i,,drop=FALSE]
						mnd.types <- mnd.types[-i]
						i <- max(1, i - 1)
					}
					else
						i <- i + 1
				}
				else
				{	tmp[i+1,COL_START] <- max(tmp[i+1,COL_START], tmp[i,COL_END]+1)
					if(tmp[i+1,COL_START]>tmp[i+1,COL_END])
					{	tmp <- tmp[-(i+1),,drop=FALSE]
						mnd.types <- mnd.types[-(i+1)]
						i <- max(1, i - 1)
					}
					else
						i <- i + 1
				}
			}
		}
		
		# note : point médian entre deux dates (finalement pas utilisé)
		# as.Date((as.integer(dates[1])+as.integer(dates[2]))/2, origin=as.Date("1970-01-01"))		
		
		# build string representing sequence
		matches <- sapply(1:(length(dates)-1), function(d)
		{	# compute intersection durations
			inters <- sapply(1:nrow(tmp), function(r)
			{	date.intersect.val(start1=dates[d], end1=dates[d+1], 
						start2=tmp[r,COL_START], end2=tmp[r,COL_END])
			})
			# take the longest
			idx <- which(!is.na(inters) & inters>=semi.duration)
			if(length(idx)>0)
			{	idx <- idx[which.max(inters[idx])]
				str <- tmp[idx,COL_OFFICE]
			}
			else
				str <- NA
		})
		
		res <- paste(matches, collapse="-")
		return(cbind(id, res))
	}))
	cat("  Processing of ids complete\n")
	colnames(seqs) <- c(COL_ID, COL_SEQ)
	
	# remove empty trajectories (all mandates out of the period)
	idx <- which(seqs[,COL_SEQ]==empty.seq)
	if(length(idx)>0)
	{	cat("  Removing",length(idx),"empty trajectories (no mandates, or all of them out of the specified period)\n")
		seqs <- seqs[-idx,]
	}
	
	# convert to dataframe
	seqs <- data.frame(
		seqs, 
		stringsAsFactors=FALSE,
		check.names=FALSE
	)
	
	# create the traminer object
	sd <- seqdef(
		data=seqs,									# data to process
		left=left,									# how to handle missing data at the beginning of the sequence (NA vs. "DEL") 
		gap=NA,										# how to handle missing data inside the sequence (same as above)
		right=NA,									# how to handle missing data at the end of the sequence (same as left) 
		var=COL_SEQ,								# name of the columns containing the formatted sequences
		id=seqs[,COL_ID],							# ids of the politicians
		alphabet=short.names,						# list of position codes
		labels=names(short.names),					# names of these positions
		cpal=colors[1:length(short.names)],			# colors of these positions
		missing.color="#AAAAAA",					# color of missing values
		cnames=format(dates[1:(length(dates)-1)])	# x-axis labels
	)
	
	return(sd)
}
