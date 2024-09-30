#######################################################
##### DONNEES CALIFORNIA SENAT 2000-2020 #####
#####
##### Chargement et nettoyage des donnees brutes du senat
#####
##### AVRIL 2021 - REPRISE EN JANVIER 2022
#######################################################




#####################################
############ CHARGEMENT #############
#####################################

# on construit la sequence d'annees a traiter
years.sen <- seq(from=1998, to=2020, by=2)

# CHARGER LES DONNEES

file <- file.path(data.folder,"S_all.csv")

tab.sen <- read.csv(file,
                header = TRUE,
                sep = ";",
                check.names=FALSE	# ne pas modifier les noms des colonnes
            )

# on remplace les cellules vides par des NA explicites
tab.sen[tab.sen==""] <- NA

# A ce stade, la variable tab.sen contient toutes les donnees : 266 lignes x 17 colonnes
cat("Dimension de la table complete : ",dim(tab.sen)[1],"x",dim(tab.sen)[2],"\n", sep="")
# on affiche juste le debut de la table pour controle
cat("Apercu de la table :\n"); print(head(tab.sen))  








######################################
############ CORRECTIONS #############
######################################



### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###


# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(tab.sen[,"Name"]))
cat("Nombre de noms uniques dans la table : ", length(unique.names), "\n", sep="")

#Pour les 220 sieges mis a election, 266 avec elections speciales, on retrouve 147 personnes uniques.

# on les affiche tous, pour controle : 
print(unique.names)

#Quelques pbs d'orthographe : 
#Oller, Thomas "Rico" / Oller, Thomas �Rico�
idx <- which(tab.sen[,"Name"]=="Oller, Thomas �Rico�")
tab.sen[idx,"Name"] <- "Oller, Thomas Rico"


#Resume de la repartition des valeurs:
#Ici pas tres utile, ne nous montre rien
#Je remarque cependant que Female, Asian, Black... sont traitees comme des valeurs numeriques

summary(tab.sen)



### CORRECTIONS RACE ET GENRE ###

#La colonne a une valeur numerique alors que j'aimerai qu'elle soit en logical:
tab.sen[,"Female"] <- as.logical(tab.sen[,"Female"])
tab.sen[,"White"] <- as.logical(tab.sen[,"White"])
tab.sen[,"Hispanic"] <- as.logical(tab.sen[,"Hispanic"])
tab.sen[,"Black"] <- as.logical(tab.sen[,"Black"])
tab.sen[,"Asian"] <- as.logical(tab.sen[,"Asian"])

#Je verifie si cela a bien fonctionne grace a un summary
summary(tab.sen[,"Female"])
summary(tab.sen[,"White"])
summary(tab.sen[,"Hispanic"])
summary(tab.sen[,"Black"])
summary(tab.sen[,"Asian"])

#Je ne remarque aucune anomalie pour female ou les races
#En effet, une fois de plus, le nettoyage a ete realise sur Googlesheet lors de la saisie


#Je passe aux verifications tout de meme : 
summary(tab.sen[,"Race"])
table(tab.sen[,"Race"])
table(tab.sen[,"Female"])


#Quelques modifs diverses suite a des messages d'erreur rencontres plus loin : 
idx <- which(tab.sen[,"Name"]=="Alarcon, Richard")
tab.sen[idx,"Race"] <- "W"
tab.sen[idx,"White"] <- TRUE
tab.sen[idx,"Hispanic"] <- FALSE

idx <- which(tab.sen[,"Name"]=="Murray, Kevin")
tab.sen[idx,"Birth"] <- '03/12/1960'

idx <- which(tab.sen[,"Name"]=="Wyland, Mark")
tab.sen[idx,"Birth"] <- '10/27/1946'

idx <- which(tab.sen[,"Name"]=="Cogdill, Dave")
tab.sen[idx,"Birth"] <- '12/31/1950'

idx <- which(tab.sen[,"Name"]=="Florez, Dean")
tab.sen[idx,"Race"] <- "W"
tab.sen[idx,"White"] <- TRUE
tab.sen[idx,"Hispanic"] <- FALSE


### CORRECTIONS SUR LES BORNES MANDATS ###

#D'abord, je m'assure de la regularite des dates de debut et fin de mandat.
summary(tab.sen[,"Beginning term"])
sort(unique(tab.sen[,"Beginning term"]))
table(tab.sen[,"Beginning term"])




#Je recherche les idx des elues avec erreurs dates pour appliquer harmonisation dates de debut : 
#Lorsqu'une seule modif j'inscris la ligne manuellement, sinon je regroupe en idx
which(tab.sen[,'Beginning term']== "28/05/2015")
tab.sen[74,"Beginning term"] <- "05/28/2015"

which(tab.sen[,'Beginning term']=='25/06/2018')
tab.sen[52,"Beginning term"] <- '06/25/2018'


which(tab.sen[,"Beginning term"]=='12/04/1994')
tab.sen[120,"Beginning term"]<-'12/05/1994'

idx <- which(tab.sen[,"Beginning term"]== '07/12/2020')
tab.sen[idx,"Beginning term"] <- '12/07/2020'

idx <- which(tab.sen[,"Beginning term"]== '07/12/1998')
tab.sen[idx,"Beginning term"] <- '12/07/1998'

idx <- which(tab.sen[,"Beginning term"]== '05/12/2016')
tab.sen[idx,"Beginning term"] <- '12/05/2016'

which(tab.sen[,"Beginning term"]=='05/12/1994')
tab.sen[107,"Beginning term"] <- '12/05/1994'

idx <- which(tab.sen[,"Beginning term"]=='03/12/2018')
tab.sen[idx,"Beginning term"] <- '12/03/2018'

which(tab.sen[,"Beginning term"]=='03/12/2012')
tab.sen[21,"Beginning term"] <- '12/03/2012'

idx <- which(tab.sen[,"Name"]=="Chang, Ling-Ling")
tab.sen[idx,"Beginning term"] <- '12/01/2018'

#Je verifie : 
table(tab.sen[,"Beginning term"])
sort(unique(tab.sen[,"Beginning term"]))



#Je regarde maintenant la regularite des dates de fin de mandat:
table(tab.sen[,"End of term"])
sort(unique(tab.sen[,"End of term"]))

#Ensuite, il y a egalement qques problemes d'harmonisation des dates:
idx <- which(tab.sen[,"End of term"]=='11/20/2016')
tab.sen[idx,"End of term"] <- '11/30/2016'

which(tab.sen[,"End of term"]=='2/22/2018')
tab.sen[143,"End of term"] <- '02/22/2018'

which(tab.sen[,"End of term"]=='12/03/2012')
tab.sen[17,"End of term"] <- '11/30/2012'


#Je verifie:
table(tab.sen[,"End of term"])
sort(unique(tab.sen[,"End of term"]))



## On convertit les dates de la table principales en vraies dates R
tab.sen[,"Beginning term"] <- as.Date(tab.sen[,"Beginning term"], format="%m/%d/%Y")
tab.sen[,"End of term"] <- as.Date(tab.sen[,"End of term"], format="%m/%d/%Y")




### CORRECTIONS SUR LES MOTIFS DE FIN DE MANDAT ###

#Ensuite, je regarde la regularite des Exits de fin de mandats:
summary(tab.sen[,'Exit'])
table(tab.sen[,'Exit'])



### CORRECTIONS SUR LES "OCCUPATIONS SUIVANTES" ###

sort(unique(tab.sen[,"Next occupation"]))
table(tab.sen[,"Next occupation"])

#Je nettoie pour tenter d'harmoniser un peu:


idx <- which(tab.sen[,"Next occupation"] == "Association")
tab.sen[idx,"Next occupation"] <- "Foundation"

idx <- which(tab.sen[,"Next occupation"] == "Mayor candidate")
tab.sen[idx,"Next occupation"] <- "M candidate"

idx <- which(tab.sen[,"Next occupation"] == "Lieutenant Governor")
tab.sen[idx,"Next occupation"] <- "Lieutenant Governor candidate"

idx <- which(tab.sen[,"Next occupation"] == "Attorney candidate")
tab.sen[idx,"Next occupation"] <- "Attorney General candidate"





##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
tab.sen <- cbind(rep(NA,nrow(tab.sen)), tab.sen)
colnames(tab.sen)[1] <- "Id"

# on recupere tous les noms uniques
unique.names <- sort(unique(tab.sen[,"Name"]))
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
	# sinon, on cree un nouvel id
	else
	{	last.id <- last.id + 1
		id.char <- sprintf("%03d", last.id)
		cat("  Pas trouve dans l'assemblee: nouvel id=",id.char,"\n",sep="")
	}
	
	# on recupere les lignes contenant le nom unique dans la table du senat
	idx <- which(tab.sen[,"Name"]==unique.name)
	# on met l'ID associe au nom sur ces lignes-ci
	tab.sen[idx, "Id"] <- id.char
	
	# test : le nom unique est-il toujours associe a la meme valeur d'attribut ?
	atts <- c("Birth", "Party", "Race")
	# on teste chaque attribut de la liste ci-dessus, tu peux en rajouter d'autres si tu veux
	for(att in atts)
	{	# on verifie juste s'ils sont tous egaux ou pas
		vals <- tab.sen[idx,att]
		vals[is.na(vals)] <- ""
		if(!all(vals[1]==vals))
		{	# on affiche un message d'avertissement
			cat("\n>>Probleme d'inconsistance de l'attribut '",att,"' pour le nom '",unique.name, "'\n", sep="")
			# on affiche les differentes valeurs avec la ligne correspondant dans la table
			print(cbind(idx, tab.sen[idx,att]))
		}
	}
}





#####################################################
############ CREATION TABLE DES ELUS ################
#####################################################

# on initialise la table des elus (vide)
repr.sen <- NULL

# liste des colonnes de la table principale qu'on veut copier dans la table des elus
cols <- c("Id", "Name", "Birth", "Race", "White", "Hispanic", "Black", "Asian", "Female", "Term", "Party")

# liste des id des senateurs
id.chars <- sort(unique(tab.sen[,"Id"]))

# on traite chaque id de senateur
for(id in id.chars)
{	# on trouve la premiere occurrence de cet id dans la table principale
	idx <- which(tab.sen[,"Id"]==id)

	# on utilise cette ligne pour completer la table des elus
	if(is.null(repr.sen))
		repr.sen <- tab.sen[idx[1], cols]
	else
		repr.sen <- rbind(repr.sen, tab.sen[idx[1], cols])
	
	# on place le mandat le plus ancien dans la colonne Term
	term <- min(tab.sen[idx,"Term"])
	repr.sen[nrow(repr.sen),"Term"] <- term
}
# pr des raisons pratiques, on supprime les noms de lignes (rajoutes automatiquement)
rownames(repr.sen) <- c()

# on enregistre la table des elus sous forme de CSV
out.file <- file.path(data.folder, "S_repr.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=repr.sen, 		# donnees qu'on veut enregistrer
	file=out.file,		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)




#####################################################
######### ENREGISTREMENT TABLE NETTOYEE #############
#####################################################

# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "S_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=tab.sen, 			# donnees qu'on veut enregistrer
	file=out.file, 		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tête contenant les noms des colonnes
)
