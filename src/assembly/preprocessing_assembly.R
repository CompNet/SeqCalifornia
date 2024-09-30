#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Chargement et nettoyage des donnees brutes de l'assemblee
#####
##### FEVRIER 2021
#######################################################




#####################################
############ CHARGEMENT #############
#####################################

# CHARGER LES DONNEES

#Importer les donnees en fichiers individuels : Import Dataset, From Text (puisque je les ai telecharge en CSV sur mon ordi, directement dans le doc R)

#OU faire une boucle: 

# on construit la sequence d'annees a traiter
years <- seq(from=1980, to=2020, by=2)

# on definit les noms des fichiers correspondants
files <- paste("A_",years,".csv",sep="")

# on initialise la table principale
tab.ass <- NULL

# on lit les fichiers un par un
for (i in 1:length(files))
{	# on lit les donnees suivantes
	file <- file.path(data.folder,files[i])
	cat("Lecture de",file,"\n")
	tmp <- read.csv(
		file=file,			# chemin complet du fichier a lire 
		header=TRUE, 		# le fichier possede une en-tete
		sep=";", 			# caractere utilise dans le fichier comme separateur de colonnes
		quote="\"",			# caractere utilise dans le fichier pour delimiter les chaines de caracteres
		dec=".",			# caractere utilise dans le fichier comme separateur decimal
		fill=FALSE,			# ne pas rajouter automatiquement les cellules manquant dans le fichier (>> erreur)
		check.names=FALSE	# ne pas modifier les noms des colonnes
	)
  	
	#on rajoute la colonne de legislature
	tmp <- cbind(tmp, rep(years[i], nrow(tmp)))
	colnames(tmp)[ncol(tmp)] <- "Term"
  	
	# on rajoute les donnees a la table principale
	if(is.null(tab.ass))
		tab.ass <- tmp
	else
		tab.ass <- rbind(tab.ass, tmp)
}

# on remplace les cellules vides par des NA explicites
tab.ass[tab.ass==""] <- NA

# on supprime les espace de debut/fin de chaine dans les colonnes contenant du texte
for(col in c("Name", "Birth", "Party", "Other terms", "Race", "Beginning term", "End of term","End Type", "Exit", "Next occupation"))
	tab.ass[,col] <- trimws(tab.ass[,col])

# A ce stade, la variable tab.ass contient toutes les donnees : 891 lignes x 20 colonnes
cat("Dimension de la table complete : ",dim(tab.ass)[1],"x",dim(tab.ass)[2],"\n", sep="")
# on affiche juste le debut de la table pour controle
cat("Apercu de la table :\n"); print(head(tab.ass))  








######################################
############ CORRECTIONS #############
######################################

### NETTOYER LES DOUBLONS, LES PBS D'IDENTITE ###

# on recupere la liste des noms uniques, dans l'ordre alphabetique  
unique.names <- sort(unique(tab.ass[,"Name"]))

cat("Nombre de noms uniques dans la table (1) : ", length(unique.names), "\n", sep="")
# on les affiche tous, pour controle : 
print(unique.names)

#Je remarque a ce stade 16 erreurs de personnes dont le nom est orthographie de deux manieres differentes : 
#ATTENTION IL Y A BIEN 2 KEVIN MCCARTY ET MCCARTHY.
#Pour eviter que les lignes soient fausses en cas d'ajouts de data,
#Je passe par un idx <- which : 


idx <- which(tab.ass[,"Name"]=="Aanestad, Sam")
tab.ass[idx,"Name"] <- "Aanestad, Samuel"

idx <- which(tab.ass[,"Name"]=="Aroner, Dion Louise")
tab.ass[idx,"Name"] <- "Aroner, Dion"

idx <- which(tab.ass[,"Name"]=="Bradfordn Steven")
tab.ass[idx,"Name"] <- "Bradford, Steven"

idx <- which(tab.ass[,"Name"]=="Briggs, mike")
tab.ass[idx,"Name"] <- "Briggs, Mike"

idx <- which(tab.ass[,"Name"]=="Caballero, Anna M.")
tab.ass[idx,"Name"] <- "Caballero, Anna"

idx <- which(tab.ass[,"Name"]=="Calderon, Thomas M.")
tab.ass[idx,"Name"] <- "Calderon, Thomas"

idx <- which(tab.ass[,"Name"]=="Collins, B. T.")
tab.ass[idx,"Name"] <- "Collins, B.T"

idx <- which(tab.ass[,"Name"]=="Frazee, Robert C. \"Bob\"")
tab.ass[idx,"Name"] <- "Frazee, Robert C. (Bob)"

idx <- which(tab.ass[,"Name"]=="Hernandez, Edward P. (Ed)")
tab.ass[idx,"Name"] <- "Hernandez, Edward"

idx <- which(tab.ass[,"Name"]=="Hertzberg, Robert")
tab.ass[idx,"Name"] <- "Hertzberg, Bob"

idx <- which(tab.ass[,"Name"]=="Kehoe, Chirstine")
tab.ass[idx,"Name"] <- "Kehoe, Christine"

idx <- which(tab.ass[,"Name"]=="Lancaster, William H. \"Bill\"")
tab.ass[idx,"Name"] <- "Lancaster, William H.\"Bill\""

idx <- which(tab.ass[,"Name"]=="Quirk-Silva, Sharon D")
tab.ass[idx,"Name"] <- "Quirk-Silva, Sharon"

idx <- which(tab.ass[,"Name"]=="Talamantes Eggman, Suan")
tab.ass[idx,"Name"] <- "Talamantes Eggman, Susan"

idx <- which(tab.ass[,"Name"]=="Holden, Chirs")
tab.ass[idx,"Name"] <- "Holden, Chris"

idx <- which(tab.ass[,"Name"] == ", Steve")
tab.ass[idx,"Name"] <- "Baldwin, Steve"

idx <- which(tab.ass[,"Name"] == "Alquist, Elaine White")
tab.ass[idx,"Name"] <- "Alquist, Elaine"

idx <- which(tab.ass[,"Name"] == "Gordon, Rich")
tab.ass[idx,"Name"] <- "Gordon, Richard"

#Je verifie si cela a fonctionne en refaisant les etapes precedentes:
unique.names <- sort(unique(tab.ass[,"Name"]))
cat("Nombre de noms uniques dans la table (2) : ", length(unique.names), "\n", sep="")

#Je m'occupe maintenant de corriger tous les problemes releves dans la console:


#La date de naissance de Gil Cedillo :
idx <- which(tab.ass[,"Birth"] == "3/25/1954")
tab.ass[idx,"Birth"] <- "03/25/1954"

#La date de naissance de Vicencia Frank
idx <- which(tab.ass[,"Birth"] == "08/23/31")
tab.ass[idx,"Birth"] <- "08/23/1931"

#La date de naissance de Aroner, Baca, Baldwin, Cortese, Duffy, Dutra, Horcher, Leonard, Nakano, Reyes, Strom Martin, Thomson, Washington, Wesson,  : 
idx <- which(tab.ass[,"Name"] == "Aroner, Dion")
tab.ass[idx,"Birth"] <- "06/06/1945"


idx <- which(tab.ass[,"Name"] == "Dutra, John")
tab.ass[idx,"Birth"] <- "10/15/1935"

idx <- which(tab.ass[,"Name"] == "Leonard, Bill")
tab.ass[idx,"Birth"] <- "10/29/1947"

idx <- which(tab.ass[,"Name"] == "Nakano, George")
tab.ass[idx,"Birth"] <- "11/24/1935"

idx <- which(tab.ass[,"Name"] == "Reyes, Sarah")
tab.ass[idx,"Birth"] <- "02/24/1961"

idx <- which(tab.ass[,"Name"] == "Strom-Martin, Virginia")
tab.ass[idx,"Birth"] <- "03/29/1948"

idx <- which(tab.ass[,"Name"] == "Thomson, Helen")
tab.ass[idx,"Birth"] <- "06/08/1940"

idx <- which(tab.ass[,"Name"] == "Washington, Carl")
tab.ass[idx,"Birth"] <- "01/25/1965"

idx <- which(tab.ass[,"Name"] == "Wesson, Herb")
tab.ass[idx,"Birth"] <- "11/11/1951"

idx <- which(tab.ass[,"Name"] == "Atkins, Toni")
tab.ass[idx,"Birth"] <- "08/01/1962"

idx <- which(tab.ass[,"Name"] == "Knight, Steve")
tab.ass[idx,"Birth"] <- "12/17/1966"

idx <- which(tab.ass[,"Name"] == "Liu, Carol")
tab.ass[idx,"Birth"] <- "09/12/1941"

#Les inversions dates / mois avec le format us : 
idx <- which(tab.ass[,"Birth"] == "13/01/1932")
tab.ass[idx,"Birth"] <- "01/13/1932"


idx <- which(tab.ass[,"Birth"] == "6/29/1974")
tab.ass[idx,"Birth"] <- "06/29/1974"

idx <- which(tab.ass[,"Birth"] == "5/15/1969")
tab.ass[idx,"Birth"] <- "05/15/1969"

idx <- which(tab.ass[,"Birth"] == "3/31/1959")
tab.ass[idx,"Birth"] <- "03/31/1959"

idx <- which(tab.ass[,"Birth"] == "3/18/1965")
tab.ass[idx,"Birth"] <- "03/18/1965"

idx <- which(tab.ass[,"Birth"] == "3/16/1959")
tab.ass[idx,"Birth"] <- "03/16/1959"

idx <- which(tab.ass[,"Birth"] == "27/01/1956")
tab.ass[idx,"Birth"] <- "01/27/1956"

idx <- which(tab.ass[,"Birth"] == "25/02/1939")
tab.ass[idx,"Birth"] <- "02/25/1939"

idx <- which(tab.ass[,"Birth"] == "24/11/1964")
tab.ass[idx,"Birth"] <- "11/24/1964"

idx <- which(tab.ass[,"Birth"] == "23/10/1950")
tab.ass[idx,"Birth"] <- "10/23/1950"

idx <- which(tab.ass[,"Birth"] == "2/17/1968")
tab.ass[idx,"Birth"] <- "02/17/1968"

idx <- which(tab.ass[,"Birth"] == "17/05/1977")
tab.ass[idx,"Birth"] <- "05/17/1977"

idx <- which(tab.ass[,"Birth"] == "13/10/1966")
tab.ass[idx,"Birth"] <- "10/13/1966"

idx <- which(tab.ass[,"Birth"] == "1974")
tab.ass[idx,"Birth"] <- "01/01/1974"

idx <- which(tab.ass[,"Birth"] == "1969")
tab.ass[idx,"Birth"] <- "01/01/1969"

idx <- which(tab.ass[,"Birth"] == "1960")
tab.ass[idx,"Birth"] <- "01/01/1960"

idx <- which(tab.ass[,"Birth"] == "1957")
tab.ass[idx,"Birth"] <- "01/01/1957"

idx <- which(tab.ass[,"Birth"] == "1956")
tab.ass[idx,"Birth"] <- "01/01/1956"

idx <- which(tab.ass[,"Birth"] == "1953")
tab.ass[idx,"Birth"] <- "01/01/1953"

idx <- which(tab.ass[,"Birth"] == "1952")
tab.ass[idx,"Birth"] <- "01/01/1952"

idx <- which(tab.ass[,"Birth"] == "1951")
tab.ass[idx,"Birth"] <- "01/01/1951"

idx <- which(tab.ass[,"Birth"] == "1948")
tab.ass[idx,"Birth"] <- "01/01/1948"

idx <- which(tab.ass[,"Birth"] == "1947")
tab.ass[idx,"Birth"] <- "01/01/1947"

idx <- which(tab.ass[,"Birth"] == "1946")
tab.ass[idx,"Birth"] <- "01/01/1946"

idx <- which(tab.ass[,"Birth"] == "1945")
tab.ass[idx,"Birth"] <- "01/01/1945"

idx <- which(tab.ass[,"Birth"] == "1935")
tab.ass[idx,"Birth"] <- "01/01/1935"

idx <- which(tab.ass[,"Birth"] == "1939")
tab.ass[idx,"Birth"] <- "01/01/1939"

idx <- which(tab.ass[,"Birth"] == "05/1201946")
tab.ass[idx,"Birth"] <- "05/12/1946"

#La race de Rivas Luz
idx <- which(tab.ass[,"Name"]=="Rivas, Luz")
tab.ass[idx,"Race"] <- "H"

#La race de Rivas Robert
idx <- which(tab.ass[,"Name"]=="Rivas, Robert")
tab.ass[idx,"Race"] <- "H"

#La race de Fong Vince
idx <- which(tab.ass[,"Name"]=="Fong, Vince")
tab.ass[idx,"Race"] <- "A"

#La race de Thurmond, Tony
idx <- which(tab.ass[,"Name"]=="Thurmond, Tony")
tab.ass[idx,"Race"] <- "B"

#La race de Gomez, Jimmy
idx <- which(tab.ass[,"Name"]=="Gomez, Jimmy")
tab.ass[idx,"Race"] <- "H"

#La race de Dutra, John
idx <- which(tab.ass[,"Name"]=="Dutra, John")
tab.ass[idx,"Race"] <- "W"

#La race de Reyes, Sarah
idx <- which(tab.ass[,"Name"]=="Reyes, Sarah")
tab.ass[idx,"Race"] <- "H"

#La race de Bermudez, Rudy
idx <- which(tab.ass[,"Name"]=="Bermudez, Rudy")
tab.ass[idx,"Race"] <- "H"

#La race de Pacheco, Robert
idx <- which(tab.ass[,"Name"]=="Pacheco, Robert")
tab.ass[idx,"Race"] <- "W"

#La race de Pacheco, Rod
idx <- which(tab.ass[,"Name"]=="Pacheco, Rod")
tab.ass[idx,"Race"] <- "H"

#La race de Wright, Roderick
idx <- which(tab.ass[,"Name"]=="Wright, Roderick")
tab.ass[idx,"Race"] <- "B"

#La race de Romero, Gloria
idx <- which(tab.ass[,"Name"]=="Romero, Gloria")
tab.ass[idx,"Race"] <- "H"
tab.ass[idx,"White"] <- "0"
tab.ass[idx, "Hispanic"] <- "1"



#La race de Gordon, Mile
idx <- which(tab.ass[,"Name"]=="Gordon, Mile")
tab.ass[idx,"Race"] <- "W"
#Je remarque d'ailleurs que son nom est faux, il manque son sexe et sa date : 
idx <- which(tab.ass[,"Name"]=="Gordon, Mile")
tab.ass[idx,"Name"] <- "Gordon, Mike"
idx <- which(tab.ass[,"Name"]=="Gordon, Mike")
tab.ass[idx,"Birth"] <- '11/15/1957'
idx <- which(tab.ass[,"Name"]=="Gordon, Mike")
tab.ass[idx,"Female"] <- FALSE



#Resume de la repartition des valeurs:
#Ici pas tres utile, ne nous montre rien, mise a part qu'il manque par ex 5 sexe non renseigne, etc.
summary(tab.ass)



### CORRECTION SUR LE PARTI ###
summary(tab.ass[,"Party"])
table(tab.ass[,"Party"])

#J'aimerai remplacer le Ind en I et le RFM en I egalement
idx <- which(tab.ass[,"Party"]=="Ind")
tab.ass[idx,"Party"] <- "I"

idx <- which(tab.ass[,"Party"]=="RFM")
tab.ass[idx,"Party"] <- "I"


### CORRECTIONS SUR LE GENRE ###

summary(tab.ass [,"Female"])


# La colonne a une valeur numerique alors que j'aimerai qu'elle soit en logical:
tab.ass[,"Female"] <- as.logical(tab.ass[,"Female"])

# Je verifie si cela a bien fonctionne grace a un summary
summary(tab.ass[,"Female"])

# Je remarque qu'il y a 3 NA dans summary pour le genre.
# Les 3 sont des hommes Bermudez Rudyx2 et John Dutra:
idx <- which(tab.ass[,"Name"]=="Dutra, John")
tab.ass[idx,"Female"] <- FALSE

idx <- which(tab.ass[,"Name"]=="Bermudez, Rudy")
tab.ass[idx,"Female"] <- FALSE




# Je verifie si cela a bien fonctionne grace a un summary
summary(tab.ass[,"Female"])




### CORRECTIONS SUR LA RACE ###

#On met des NA à la place des "?" (quand la race est inconnue)
tab.ass[!is.na(tab.ass[,"White"]) & tab.ass[,"White"]=="?","White"] <- NA
tab.ass[!is.na(tab.ass[,"Hispanic"]) & tab.ass[,"Hispanic"]=="?","Hispanic"] <- NA
tab.ass[!is.na(tab.ass[,"Asian"]) & tab.ass[,"Asian"]=="?","Asian"] <- NA
tab.ass[!is.na(tab.ass[,"Black"]) & tab.ass[,"Black"]=="?","Black"] <- NA


summary(tab.ass[,"Race"])
table(tab.ass[,"Race"])
View(tab.ass[,"Race"])

#Sur le meme modele que la colonne femmes, je transforme les colonnes W - H - A et B en true/false : 
#Je dois d'abord m'assurer que toutes les colonnes concernees soient bien en numerique et pas characteres:
tab.ass[,"White"] <- as.numeric(tab.ass[,"White"])
tab.ass[,"Hispanic"] <- as.numeric(tab.ass[,"Hispanic"])
tab.ass[,"Asian"] <-as.numeric(tab.ass[,"Asian"])
tab.ass[,"Black"] <-as.numeric(tab.ass[,"Black"])

#Puis je transforme en logical chacune des colonnes numeriques : 
tab.ass[,"White"] <- as.logical(tab.ass[,"White"])
tab.ass[,"Hispanic"] <- as.logical(tab.ass[,"Hispanic"])
tab.ass[,"Black"] <- as.logical(tab.ass[,"Black"])
tab.ass[,"Asian"] <- as.logical(tab.ass[,"Asian"])



### CORRECTIONS SUR LES BORNES MANDATS ###

#D'abord, je m'assure de la regularite des dates de debut et fin de mandat.
summary(tab.ass[,"Beginning term"])
View(tab.ass[,"Beginning term"])
sort(unique(tab.ass[,"Beginning term"]))

#J'applique quelques modif d'harmonisation : 
idx <- which(tab.ass[,"Beginning term"]=="12/01/1980")
tab.ass[idx,"Beginning term"] <- "12/05/1980"

idx <- which(tab.ass[,"Beginning term"]=="12/03/1984")
tab.ass[idx,"Beginning term"] <- "12/05/1984"
  
  idx <- which(tab.ass[,"Beginning term"]=="12/01/1986")
tab.ass[idx,"Beginning term"] <-"12/05/1986"
  
  idx <- which(tab.ass[,"Beginning term"]=="12/05/1990")
tab.ass[idx,"Beginning term"] <-"12/03/1990"
  
  idx <- which(tab.ass[,"Beginning term"]=="12/01/1994")
tab.ass[idx,"Beginning term"] <-"12/05/1994"
  
  idx <- which(tab.ass[,"Beginning term"]=="12/04/1994")
tab.ass[idx,"Beginning term"] <-"12/05/1994"
  
  idx <- which(tab.ass[,"Beginning term"]=="12/01/1996")
tab.ass[idx,"Beginning term"] <-"12/02/1996"
  
  idx <- which(tab.ass[,"Beginning term"]=="12/05/1996")
tab.ass[idx,"Beginning term"] <-"12/02/1996"
  
  idx <- which(tab.ass[,"Beginning term"]=="12/04/1998")
tab.ass[idx,"Beginning term"] <-"12/07/1998"




#Je verifie : 
table(tab.ass[,"Beginning term"])
sort(unique(tab.ass[,"Beginning term"]))



#Je regarde maintenant la regularite des dates de fin de mandat:
table(tab.ass[,"End of term"])
sort(unique(tab.ass[,"End of term"]))


### CORRECTIONS SUR LES MOTIFS DE FIN DE MANDAT ###

#Ensuite, je regarde la regularite des Exits de fin de mandats:
summary(tab.ass[,'Exit'])
View(tab.ass[,'Exit'])
table(tab.ass[,'Exit'])

#J'applique quelques modifications d'harmonisation : 

#D'abord je cherche quelles lignes sont incorrectes:
#Je change la valeur de chacune de mes idx:
#nb : ce n'est pas grave si tout le monde s'appelle idx car je n'ai pas besoin de garder une trace de l'info

idx <- which(tab.ass[,"Exit"]=="Termed Out")
tab.ass[idx, "Exit"] <- "Termed out"


idx <- which(tab.ass[,"Exit"]=="Resignation (sexual assault")
tab.ass[idx, "Exit"] <- "Resignation (sexual assault)"


idx <- which(tab.ass[,"Exit"]=="Not seek reekection")
tab.ass[idx, "Exit"] <- "Not seek reelection"


#Je verifie le bon fonctionnement de mes changements:
table(tab.ass[,'Exit'])



### CORRECTIONS SUR LES "OCCUPATIONS SUIVANTES" ###

sort(unique(tab.ass[,"Next occupation"]))
table(tab.ass[,"Next occupation"])

#Je nettoie pour tenter d'harmoniser un peu:
idx <- which(tab.ass[,"Next occupation"]=="State Senate")
tab.ass[idx,"Next occupation"] <- "State Senate candidate"

idx <- which(tab.ass[,"Next occupation"]=="Defeateed")
tab.ass[idx,"Next occupation"] <- "Defeated"

idx <- which(tab.ass[,"Next occupation"] == "Board of Supervisors")
tab.ass[idx,"Next occupation"] <- "BS candidate"

idx <- which(tab.ass[,"Next occupation"]=="non-profit organisation")
tab.ass[idx,"Next occupation"] <- "non-profit organization"

idx <- which(tab.ass[,"Next occupation"]=="City Attorney")
tab.ass[idx,"Next occupation"] <- "Attorney"

idx <- which(tab.ass[,"Next occupation"]=="County District Attorney")
tab.ass[idx,"Next occupation"] <- "Attorney"

idx <- which(tab.ass[,"Next occupation"]=="Lobbying")
tab.ass[idx,"Next occupation"] <- "Lobbyist"

idx <- which(tab.ass[,"Next occupation"]=="Board of Education")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="California Air Resources Board")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="Unemployment Insurance Appeals Board")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="State Board of Equalization")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="Secretary of the California Natural Resources Agency")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="California State Board of Equalization")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="California Board of Equalization")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="President Pro Tem LA Board of Public Works Commission")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="California Department of Industrial Relations")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="California Healthcare Commission")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="State school superintendant")
tab.ass[idx,"Next occupation"] <- "State Board"

idx <- which(tab.ass[,"Next occupation"]=="Director of the Governor Regional Development Initiatives")
tab.ass[idx,"Next occupation"] <- "Government Agency"

idx <- which(tab.ass[,"Next occupation"]=="Board if Trustees LA Community College")
tab.ass[idx,"Next occupation"] <- "University"



##############################################
######### ATTRIBUTION ID UNIQUE ##############
##############################################

# on recupere tous les noms uniques
unique.names <- sort(unique(tab.ass[,"Name"]))
cat("Nombre de noms uniques dans la table (3) : ", length(unique.names), "\n", sep="")

# on calcule un numero unique pour chaque nom unique
id.vals <- 1:length(unique.names)

# on rajoute des zeros devant les numeros pour qu'ils contiennent tous le meme nombre de chiffres (ici : 3, tu peux ajuster)
id.chars <- sprintf("%03d", id.vals)

# on rajoute une colonne ID a la table (pour l'instant elle ne contient que des NA)
tab.ass <- cbind(rep(NA,nrow(tab.ass)), tab.ass)

# on definit le nom de la colonne dans la table
colnames(tab.ass)[1] <- "Id"

# on traite chaque nom unique un par un
for(i in 1:length(unique.names))
{	# on traite le ieme nom unique
	unique.name <- unique.names[i]
	#cat("Traitement du nom '",unique.name,"' (",i,"/",length(unique.names),")\n",sep="")
	# on recupere les lignes contenant ce nom dans la table
	idx <- which(tab.ass[,"Name"]==unique.name)
	# on met l'ID associe au nom sur ces lignes-ci
	tab.ass[idx, "Id"] <- id.chars[i]
  
	# test : le nom unique est-il toujours associe a la meme valeur d'attribut ?
	atts <- c("Birth", "Race")
	# on teste chaque attribut de la liste ci-dessus, tu peux en rajouter d'autres si tu veux
	for(att in atts)
	{	# on verifie juste s'ils sont tous egaux ou pas
		vals <- tab.ass[idx,att]
		vals[is.na(vals)] <- ""
		if(!all(vals[1]==vals))
		{	# on affiche un message d'avertissement
			cat("\n>>Probleme d'inconsistance de l'attribut '",att,"' pour le nom '",unique.name, "'\n", sep="")
			# on affiche les differentes valeurs avec la ligne correspondant dans la table
			print(cbind(idx, tab.ass[idx,att]))
		}
	}
}








#####################################################
############ CREATION TABLE DES ELUS ################
#####################################################

# on initialise la table des elus (vide)
repr.ass <- NULL

# liste des colonnes de la table principale qu'on veut copier dans la table des elus
cols <- c("Id", "Name", "Birth", "Race", "White", "Hispanic", "Black", "Asian", "Female", "Term", "Party")

# on traite chaque id
for(id in id.chars)
{	# on trouve la premiere occurrence de cet id dans la table principale
  	idx <- which(tab.ass[,"Id"]==id)

	# on utilise cette ligne pour completer la table des elus
	if(is.null(repr.ass))
    	repr.ass <- tab.ass[idx[1], cols]
	else
		repr.ass <- rbind(repr.ass, tab.ass[idx[1], cols])
	
	# on place le mandat le plus ancien dans colonne Term
	term <- min(tab.ass[idx,"Term"])
	repr.ass[nrow(repr.ass),"Term"] <- term
}
# pr des raisons pratiques, on supprime les noms de lignes (rajoutes automatiquement)
rownames(repr.ass) <- c()

# on enregistre la table des elus sous forme de CSV
out.file <- file.path(data.folder, "A_repr.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=repr.ass, 		# donnees qu'on veut enregistrer
	file=out.file,		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tete contenant les noms des colonnes
)










#########################################################
############ CONVERSION DATE ET ENREGISTREMENT TABLES ################
#########################################################

# on convertit les dates de la table principales en vraies dates R
tab.ass[,"Beginning term"] <- as.Date(tab.ass[,"Beginning term"], format="%m/%d/%Y")
tab.ass[,"End of term"] <- as.Date(tab.ass[,"End of term"], format="%m/%d/%Y")

# on enregistre la table principale sous forme de CSV
out.file <- file.path(data.folder, "A_clean.csv")
cat("Enregistrement de la table dans le fichier '",out.file,"'\n", sep="")
write.table(
	x=tab.ass, 			# donnees qu'on veut enregistrer
	file=out.file, 		# nom du fichier a creer
	quote=TRUE, 		# mettre des guillemets autour des chaines de caracteres
	sep=",",			# caractere de separation des colonnes a utiliser dans le fichier cree
	row.names=FALSE,	# on ne veut pas enregistrer de nom de lignes (sous forme de colonne separee)
	col.names = TRUE	# par contre on veut un en-tête contenant les noms des colonnes
)
