#######################################################
##### DONNEES CALIFORNIA PARLEMENT COMPLET #####
#####
##### Fusion de l'Assemblee, du Senat et des autres mandats
#####
##### AVRIL 2021 - JUILLET 2022
#######################################################




###########################################################
############ CHARGEMENT DES MANDATS AUTRES ################
###########################################################

# chargement du fichier des mandats supplementaires
file <- "O_xxxx.csv"

tab.oth <- read.csv(
	file=file.path(data.folder,file),	# chemin complet du fichier a lire 
	header=TRUE, 						# le fichier possede une en-tete
	sep=";", 							# caractere utilise dans le fichier comme separateur de colonnes
	quote="\"",							# caractere utilise dans le fichier pour delimiter les chaines de caracteres
	dec=".",							# caractere utilise dans le fichier comme separateur decimal
	fill=FALSE,							# ne pas rajouter automatiquement les cellules manquant dans le fichier (>> erreur)
	check.names=FALSE					# ne pas modifier les noms des colonnes
)

summary(tab.oth)


# Stats sur les dates dans tab.other : 
summary(tab.oth[,"Beginning term"])
sort(unique(tab.oth[,"Beginning term"]))
table(tab.oth[,"Beginning term"])

#Je remarque une erreur de format pour la date "20/01/2021" qui devrait etre dans le format US : 
idx <- which(tab.oth[,"Beginning term"]=="20/01/2021")
tab.oth[idx,"Beginning term"] <- "01/20/2021"

# Stats sur les dates de fin dans tab.oth : 
sort(unique(tab.oth[,"End of term"]))
table(tab.oth[,"End of term"])

# Je remarque une erreur avec une date mal saisie "14/06/1905" et "21/01/2006"
idx <- which(tab.oth[,"End of term"]=="14/06/1998")
tab.oth[idx, "End of term"] <- "06/14/1998"

idx <- which(tab.oth[,"End of term"]=="21/01/2006")
tab.oth[idx, "End of term"] <- "01/21/2006"

# on convertit les dates en vraies dates R
tab.oth[,"Beginning term"] <- as.Date(tab.oth[,"Beginning term"], format="%m/%d/%Y")
tab.oth[,"End of term"] <- as.Date(tab.oth[,"End of term"], format="%m/%d/%Y")


# Je corrige aussi l'orthographe des noms qui n'apparaissent pas comme tels dans les autres tables :

# Caballero, Anna M.
idx <- which(tab.oth[,"Name"]=="Caballero, Anna M.")
tab.oth[idx,"Name"] <- "Caballero, Anna"

idx <- which(tab.oth[,"Name"]=="Hertzberg, Robert")
tab.oth[idx,"Name"] <- "Hertzberg, Bob"

# McCarthy, Kevin
idx <- which(tab.oth[,"Name"]=="McCarthy. Kevin")
tab.oth[idx,"Name"] <- "McCarthy, Kevin"




##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################
cat("Attribution d'un Id unique aux mandats 'autres' \n")

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
tab.oth <- cbind(rep(NA,nrow(tab.oth)), tab.oth)
colnames(tab.oth)[1] <- "Id"

# on recupere tous les noms uniques
unique.names <- sort(unique(tab.oth[,"Name"]))
cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")

# dernier id utilise pour l'assemblee
last.id <- max(as.integer(tab.ass[,"Id"]))

# on traite chaque nom unique separemment
for(i in 1:length(unique.names))
{	# on traite le ieme nom unique
	unique.name <- unique.names[i]
	cat("Traitement du nom '",unique.name,"' (",i,"/",length(unique.names),")\n",sep="")
	
	# on cherche le nom dans la table de l'assemblee
	idx <- which(tab.ass[,"Name"]==unique.name)
	
	# si on l'y trouve, on reutilise son id existant
	if(length(idx)>0)
	{	id.char <- tab.ass[idx[1],"Id"]
		cat("  Trouve dans l'assemblee: id existant=",id.char,"\n",sep="")
	}
	# sinon, on cherche dans la table du senat
	else
	{	idx <- which(tab.sen[,"Name"]==unique.name)
		if(length(idx)>0)
		{	id.char <- tab.sen[idx[1],"Id"]
			cat("  Trouve dans le senat: id existant=",id.char,"\n",sep="")
		}
		# sinon, on cherche dans la table du senat
		else
			stop("  ERREUR : elu trouve ni dans l'assemblee ni dans le senat")
	}
	
	# on recupere les lignes contenant le nom unique dans la table
	idx <- which(tab.oth[,"Name"]==unique.name)
	# on met l'ID associe au nom sur ces lignes-ci
	tab.oth[idx, "Id"] <- id.char
}


#Plutot logiquement, tous les ID de la table "autre" ont ete trouves dans la table Senat ou Ass
#Aucune erreur a signaler




###########################################################
############### FUSION DES TROIS TABLES ###################
###########################################################

# on rajoute une colonne a la table assemblee, contenant le type de mandat
tab.ass <- cbind(tab.ass, rep("State Assembly",nrow(tab.ass)))
colnames(tab.ass)[ncol(tab.ass)] <- "Office"
# on renomme sa colonne district
colnames(tab.ass)[which(colnames(tab.ass)=="District")] <- "Local Division"

# meme chose pour le senat
tab.sen <- cbind(tab.sen, rep("State Senate",nrow(tab.sen)))
colnames(tab.sen)[ncol(tab.sen)] <- "Office"
# on renomme sa colonne district
colnames(tab.sen)[which(colnames(tab.sen)=="District")] <- "Local Division"

# liste des colonnes communes aux trois tables
col.names <- c("Id", "Name", "Office", "Local Division", "Beginning term", "End of term")
# on cree la nouvelle table en fusionnant mais seulement sur les colonnes communes
tab.all <- rbind(tab.ass[,col.names], tab.sen[,col.names], tab.oth[,col.names])


### OPTIONNEL ###
# on retire les elu(e)s qui n'ont aucun mandat de depute(e)
idx <- which(tab.all[,"Office"]=="State Assembly")		# lignes correspondant a un mandat de depute
ids <- sort(unique(tab.all[idx,"Id"]))					# ids (uniques) des elu(e)s associe(e)s a ces mandats
ids.comp <- setdiff(sort(unique(tab.all[,"Id"])), ids)	# ids des elu(e)s jamais associe(e)s a ces mandats
cat("Elu(e)s n'ayant jamais ete(e) depute(e)s")			# msg de verification
tt <- t(sapply(ids.comp, function(id) tab.all[which(tab.all[,"Id"]==id)[1], c("Id","Name")]))
print(tt)
idx <- which(tab.all[,"Id"] %in% ids)					# lignes associees aux ids des depute(e)s (pour inclure les autres mandats que deputes) 
tab.all <- tab.all[idx,]								# on ne garde que ces lignes dans la table generale 
		
# on enregistre la table fusionnee sous forme de CSV
out.file <- file.path(data.folder, "All_clean.csv")
cat("Enregistrement de la table des mandats dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=tab.all, 			# donnees qu'on veut enregistrer
	file=out.file,		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)





###########################################################
############ RECHERCHE DOUBLONS DE MANDATS ################
###########################################################

# on cherche si le meme mandat n'apparait pas plusieurs fois par erreur
cat("Recherche de doublons parmi les mandats de la table fusionnee \n")

# on parcourt chaque ligne de la table
for(r in 1:(nrow(tab.all)-1))
{	#cat("Verification du mandat ",r,"\n",sep="")
	idx <- which(tab.all[,"Name"]==tab.all[r,"Name"]
				& tab.all[,"Office"]==tab.all[r,"Office"])
	idx <- setdiff(idx, c(r))
	
	if(length(idx)>0)
		idx <- idx[which(!is.na(tab.all[idx,"Beginning term"]))]
	
	if(length(idx)>0)
	{	inters <- sapply(idx, function(s)
					{	#print(tab.all[s,c("Beginning term","End of term")])
						date.intersect.val(start1=tab.all[r,"Beginning term"], end1=tab.all[r,"End of term"], 
							start2=tab.all[s,"Beginning term"], end2=tab.all[s,"End of term"])
					})
		idx <- idx[!is.na(inters)]
	}
	
	if(length(idx)>1)
	{	cat("Mandat redondant detecte : \n")
		print(tab.all[c(r,idx),])
	}
}




#####################################################
############ CREATION TABLE DES ELUS ################
#####################################################

# on initialise la table des elus
repr.all <- rbind(repr.ass, repr.sen)

# liste des ids uniques
id.chars <- sort(unique(repr.all[,"Id"]))

# on traite chaque id
cat("Verification de la coherence des donnees individuelles decrites dans les deux tables Ass vs Sen \n")
for(id in id.chars)
{	# on trouve la premiere occurrence de cet id dans la table
	idx <- which(repr.all[,"Id"]==id)
	
	# s'il y a plus d'une occurrence, il faut tester la cohérence
	if(length(idx)>1)
	{	# on compare les infos des autres occurrences
		atts <- c("Birth", "Race", "White", "Hispanic", "Black", "Asian", "Female")
		for(att in atts)
		{	# on verifie juste s'ils sont tous egaux ou pas
			vals <- repr.all[idx,att]
			vals[is.na(vals)] <- ""
			if(!all(vals[1]==vals))
			{	# on affiche un message d'avertissement
				cat("\n>>Probleme d'inconsistance de l'attribut '",att,"' pour l'id '",id, "'\n", sep="")
				# on affiche les differentes valeurs avec la ligne correspondant dans la table
				#print(cbind(idx, repr.all[idx,att]))
				print(repr.all[idx,])
			}
		}
		
		# on supprime les occurrences surnuméraires
		repr.all <- repr.all[-idx[2:length(idx)],]
	}
}
# pr des raisons pratiques, on supprime les noms de lignes (rajoutes automatiquement)
rownames(repr.all) <- repr.all[,"Id"]

# on enregistre la table des elus sous forme de CSV
out.file <- file.path(data.folder, "All_repr.csv")
cat("Enregistrement de la table des elus dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=repr.all, 		# donnees qu'on veut enregistrer
	file=out.file,		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)
