#############################################################
# Builds the transition graph based on the list of sequences,
# and records it as a plot and a graphml file.
#
# sd: object containing the collection of sequences.
# seq.folder: folder that will contain the produced files.
# file.name: base name of the produced files.
#############################################################
plot.transition.graph <- function(sd, seq.folder, file.name)
{	# init adjacency matrix
	alpha <- alphabet(sd)
	adj <- matrix(0, nrow=length(alpha)+1, ncol=length(alpha)+1, dimnames=list(c(alpha,"*"), c(alpha,"*")))
	
	# init size vector
	state.count <- rep(0, length(alpha)+1)
	names(state.count) <- c(alpha,"*")
	
	# compute the ajacency matrix
	for(s in 1:nrow(sd))
	{	si <- sd[s,]
		prev <- NA
		for(c in 1:ncol(si))
		{	cur <- as.character(si[1,c])
			if(!is.na(prev))
				adj[prev,cur] <- adj[prev,cur] + 1
			state.count[cur] <- state.count[cur] + 1
			prev <- cur
		}
	}
	
	# record adjacency matrix
	adj.file <- file.path(seq.folder, paste0(file.name,"transition_graph_adjmat.csv"))
	write.csv(x=adj, file=adj.file, row.names=TRUE, fileEncoding="UTF-8")#, col.names=TRUE)
	
	# remove "*" state
	sc <- which(colnames(adj)=="*")
	adj <- adj[-sc,-sc]
	state.count <- state.count[-sc]
	
	# remove self-edges (loops)
	diag(adj) <- 0
	
	# build the graph based on the adjacency matrix
	g <- graph_from_adjacency_matrix(
		adjmatrix=adj,
		mode="directed",
		weighted=TRUE
	)
	V(g)$occurrences <- state.count
	
	# possibly load the layout
	lay.file <- file.path(seq.folder, paste0(file.name,"transition_graph_layout.txt"))
	if(file.exists(lay.file))
	{	lay <- as.matrix(read.table(file=lay.file, row.names=1, header=TRUE))
		lay <- lay[alpha,]
	}
	# otherwise, init and export
	else
	{	lay <- layout_with_fr(g)
		rownames(lay) <- alpha
		colnames(lay) <- c("x", "y")
		write.table(x=lay, file=lay.file)
	}
	
	# graphical parameters
	v.sizes <- 2000 + 2*V(g)$occurrences
	e.widths <- 1 + 0.3*E(g)$weight
	# colors
	v.cols <- cpal(sd)
	e.cols <- c()
	# edge curvature and arraows
	curvature <- c()
	arrows <- c()
	el <- as_edgelist(g, names=FALSE)
	for(r in 1:nrow(el))
	{	curvature <- c(curvature, if(are_adjacent(g, el[r,2], el[r,1])) 0.25 else 0)
		arrows <- c(arrows, if(el[r,1]==el[r,2]) "--" else "->")
		e.cols <- c(e.cols, combine.colors(col1=v.cols[el[r,1]], col2=v.cols[el[r,2]], transparency=66))
	}
	
	# record as a graphml file
	V(g)$color <- v.cols
	E(g)$color <- e.cols
	graph.file <- file.path(seq.folder, paste0(file.name,"transition_graph.graphml"))
	write.graph(graph=g, file=graph.file, format="graphml")
	
	# plot bounds
	xmargin <- (max(lay[,1])-min(lay[,1]))*0.1
	xlim <- c(min(lay[,1])-xmargin, max(lay[,1])+xmargin)
	ymargin <- (max(lay[,2])-min(lay[,2]))*0.1
	ylim <- c(min(lay[,2])-ymargin, max(lay[,2])+ymargin)

	# plot the graph
	plot.file <- file.path(seq.folder, paste0(file.name,"transition_graph"))
	for(format in c("png","pdf"))
	{	if(format=="png")
			png(paste0(plot.file,".png"))
		else if(format=="pdf")
			pdf(paste0(plot.file,".pdf"))
		
		plot(g,											# graph to plot
			layout=lay,									# layout
			xlim=xlim, ylim=ylim,						# plot y bounds
#			ylim=c(min(lay[,2]),max(lay[,2]*0.1)),		# plot bounds
			vertex.size=v.sizes,						# node size
			vertex.color=v.cols,						# node color
			vertex.label.cex=1.2,						# label size
			vertex.label.family="sans",					# font type
			vertex.label.font=2,						# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
			vertex.label.label.dist=0,					# label distance to node center (0=center)
			vertex.label.color="BLACK",					# label color
			edge.color=e.cols,							# edge color
#			edge.arrow.size=E(g)$weight,				# size of the arrows
			edge.arrow.mode=arrows,						# whether or not to draw the arrow heads
			edge.width=e.widths,						# link thickness
			rescale=FALSE,								# keep the actual coordinates
#			asp=1,										# keep the x/y ratio
			edge.curved=curvature						# specify edge curvature
		)
		dev.off()
	}
}
