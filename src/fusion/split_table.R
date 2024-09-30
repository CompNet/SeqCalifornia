#######################################################
##### Divise les sequences en deux sous ensembles:
##### - pre90: elus qui ont un mandat de depute avant 1990
##### - post90: elus dont le premier mandat de depute a eu lieu en 1990 ou apres.
#####
##### JUILLET 2023
#######################################################




# on définit le dossier qui recevra les fichiers produits lors de l'analyse de sequences
{	if(is.na(filter90))
		seq.folder <- file.path(plots.folder, "sequences", "all")
	else
		seq.folder <- file.path(plots.folder, "sequences", filter90)
}

# on filtre les deputes si besoin
{	# pas de filtrage, on prend tout
	if(is.na(filter90))
	{	repr.seq <- repr.all
		tab.seq <- tab.all
	}
	
	# uniquement pre-90
	else if(filter90=="pre90")
	{	# on identifie les elus qui ont un mandat de depute avant 1990
		idx <- which(tab.ass[,"Term"]<1990)
		pre90.ids <- sort(unique(tab.ass[idx,"Id"]))
		
		# on filtre les tables de mandats et d'elus
		repr.seq <- repr.all[repr.all[,"Id"] %in% pre90.ids,]
		tab.seq <- tab.all[tab.all[,"Id"] %in% pre90.ids,]
	}
	
	# uniquement post-90
	else
	{	# on identifie les elus qui n'ont pas de mandat de depute avant 1990
		idx <- which(tab.ass[,"Term"]<1990)
		pre90.ids <- sort(unique(tab.ass[idx,"Id"]))
		post90.ids <- setdiff(sort(unique(tab.ass[,"Id"])), pre90.ids)
		
		# on filtre les tables de mandats et d'elus
		repr.seq <- repr.all[repr.all[,"Id"] %in% post90.ids,]
		tab.seq <- tab.all[tab.all[,"Id"] %in% post90.ids,]
	}
}

# on enregistre la table de mandats sous forme de CSV
out.file <- file.path(seq.folder, "mandates.csv")
cat("Enregistrement de la table des mandats dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=tab.seq, 			# donnees qu'on veut enregistrer
	file=out.file,		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)

# on enregistre la table des elus sous forme de CSV
out.file <- file.path(seq.folder, "representatives.csv")
cat("Enregistrement de la table des elus dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=repr.seq, 		# donnees qu'on veut enregistrer
	file=out.file,		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)
