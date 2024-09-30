#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Analyse : qui remplace qui 
#####
##### FEVRIER 2021
#####
#######################################################

# on identifie les blancs termed out
idx <- which(tab.ass[,"White"] & tab.ass[,"Exit"]=="Termed out")
# (il y en a 143)

#J'identifie combien sont des femmes blanches (47) :
whiwoto <- which(tab.ass[,"White"] & tab.ass[,"Exit"]=="Termed out" & tab.ass[,"Female"]== TRUE)

# on recupere la legislature de ces individus :
terms <- tab.ass[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years[length(years)])    # l'expression years[length(years)] correspond a la derniere legislature dans notre liste d'annees years
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
	terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.ass[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years) represente le numero de la legislature dans notre liste d'annees years
# donc match(terms,years)+1 correspond a la position de l'annee suivante, toujours dans years
# et donc years[match(terms,years)+1] est la valeur (l'annee) localisee a cette position dans years, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years[match(terms,years)+1]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{	# on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
	tmp <- which(tab.ass[,"Term"]==next.terms[i] & tab.ass[,"District"]==districts[i])
	# on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
	if(length(tmp)!=1)
	{	# on affiche un message d'erreur avec la ligne problematique
		cat("Probleme avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste(tmp,colapse=","),"\n",sep="")
		next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
	}else
	{	# sinon, alors tout est ok et on rajoute a la liste de successeurs
    	next.mp <- c(next.mp, tmp)
    }
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)
# si tu veux afficher les lignes de la table correspondant a un individu et son successeur, tu fais par exemple pour le 105eme individu identifie :
#rbind(tab.ass[idx[50],], tab.ass[next.mp[50],])

# parmi ces successeurs, lesquels sont noirs ? (spoiler : 4)
ii <- which(tab.ass[next.mp,"Black"])
print(tab.ass[next.mp[ii],])

#lesquels sont Asian : (13)
ij <- which(tab.ass[next.mp,"Asian"])
print(tab.ass[next.mp[ij],])

#lesquels sont Hispanic : 
ik <- which(tab.ass[next.mp,"Hispanic"])
print(tab.ass[next.mp[ik],])

#lesquels sont White : 
il <- which(tab.ass[next.mp,"White"])
print(tab.ass[next.mp[il],])


# lesquelles sont des femmes ? (spoiler : 36)
ii <- which(tab.ass[next.mp,"Female"])
print(tab.ass[next.mp[ii],])
# lequelles sont des femmes noires ? (spoiler: zero)
ii <- which(tab.ass[next.mp,"Female"] &  tab.ass[next.mp,"Black"])
print(tab.ass[next.mp[ii],])
# si tu preferes avoir le decompte plutot que la liste complete, il faut utiliser en plus la fonction length comme ca : length(which(...............))

# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.ass[next.mp,"Race"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.ass[next.mp,"Race"], tab.ass[next.mp,"Female"], useNA="always"  )







#Je refais la meme chose pour chacune des races : 



# on identifie les hispanic termed out
idx <- which(tab.ass[,"Hispanic"] & tab.ass[,"Exit"]=="Termed out")
# (il y en a 42)

#J'identifie cb de femmes parmi les 30 hispanic termed out : 
hiswoto <- which(tab.ass[,"Hispanic"] & tab.ass[,"Exit"]=="Termed out" & tab.ass[,"Female"]== TRUE)

# on recupere la legislature de ces individus :
terms <- tab.ass[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years[length(years)])    # l'expression years[length(years)] correspond a la derniere legislature dans notre liste d'annees years
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.ass[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years) represente le numero de la legislature dans notre liste d'annees years
# donc match(terms,years)+1 correspond a la position de l'annee suivante, toujours dans years
# et donc years[match(terms,years)+1] est la valeur (l'annee) localisee a cette position dans years, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years[match(terms,years)+1]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.ass[,"Term"]==next.terms[i] & tab.ass[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste(tmp,colapse=","),"\n",sep="")
    next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
  }else
    # sinon, alors tout est ok et on rajoute a la liste de successeurs
    next.mp <- c(next.mp, tmp)
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)
# si tu veux afficher les lignes de la table correspondant a un individu et son successeur, tu fais par exemple pour le 105eme individu identifie :
#rbind(tab.ass[idx[50],], tab.ass[next.mp[50],])

# parmi ces successeurs, lesquels sont noirs ? (spoiler : 0)
ii <- which(tab.ass[next.mp,"Black"])
print(tab.ass[next.mp[ii],])

#lesquels sont Asian : (2)
ij <- which(tab.ass[next.mp,"Asian"])
print(tab.ass[next.mp[ij],])

#lesquels sont Hispanic (16) : 
ik <- which(tab.ass[next.mp,"Hispanic"])
print(tab.ass[next.mp[ik],])

#lesquels sont White : 
il <- which(tab.ass[next.mp,"White"])
print(tab.ass[next.mp[il],])

#Decompte version table + ajout du sexe : 
# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.ass[next.mp,"Race"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.ass[next.mp,"Race"], tab.ass[next.mp,"Female"], useNA="always"  )




# on identifie les asian termed out
idx <- which(tab.ass[,"Asian"] & tab.ass[,"Exit"]=="Termed out")
# (il y en a 16)

#J'identifie cb de femmes parmi les 12 asiat termed out : 
aswoto <- which(tab.ass[,"Asian"] & tab.ass[,"Exit"]=="Termed out" & tab.ass[,"Female"]== TRUE)

# on recupere la legislature de ces individus :
terms <- tab.ass[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years[length(years)])    # l'expression years[length(years)] correspond a la derniere legislature dans notre liste d'annees years
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.ass[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years) represente le numero de la legislature dans notre liste d'annees years
# donc match(terms,years)+1 correspond a la position de l'annee suivante, toujours dans years
# et donc years[match(terms,years)+1] est la valeur (l'annee) localisee a cette position dans years, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years[match(terms,years)+1]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.ass[,"Term"]==next.terms[i] & tab.ass[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste(tmp,colapse=","),"\n",sep="")
    next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
  }else
    # sinon, alors tout est ok et on rajoute a la liste de successeurs
    next.mp <- c(next.mp, tmp)
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)
# si tu veux afficher les lignes de la table correspondant a un individu et son successeur, tu fais par exemple pour le 105eme individu identifie :
#rbind(tab.ass[idx[50],], tab.ass[next.mp[50],])

###################################
#GROS PROBLEME j'AI UN INDIVIDU COMPLETEMENT NA QUI REMPLACE LE IDX 134...
###################################

# parmi ces successeurs, lesquels sont noirs ? (spoiler : 1)
ii <- which(tab.ass[next.mp,"Black"])
print(tab.ass[next.mp[ii],])

#lesquels sont Asian : 3
ij <- which(tab.ass[next.mp,"Asian"])
print(tab.ass[next.mp[ij],])

#lesquels sont Hispanic : 
ik <- which(tab.ass[next.mp,"Hispanic"])
print(tab.ass[next.mp[ik],])

#lesquels sont White : 
il <- which(tab.ass[next.mp,"White"])
print(tab.ass[next.mp[il],])

#Decompte version table + ajout du sexe : 
# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.ass[next.mp,"Race"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.ass[next.mp,"Race"], tab.ass[next.mp,"Female"], useNA="always"  )



# on identifie les noirs termed out
idx <- which(tab.ass[,"Black"] & tab.ass[,"Exit"]=="Termed out")
# (il y en a 11)

#J'identifie cb de femmes parmi les 10 noirs termed out:
blwoto <- which(tab.ass[,"Black"] & tab.ass[,"Exit"]=="Termed out" & tab.ass[,"Female"]== TRUE)

# on recupere la legislature de ces individus :
terms <- tab.ass[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years[length(years)])    # l'expression years[length(years)] correspond a la derniere legislature dans notre liste d'annees years
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.ass[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years) represente le numero de la legislature dans notre liste d'annees years
# donc match(terms,years)+1 correspond a la position de l'annee suivante, toujours dans years
# et donc years[match(terms,years)+1] est la valeur (l'annee) localisee a cette position dans years, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years[match(terms,years)+1]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.ass[,"Term"]==next.terms[i] & tab.ass[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste(tmp,colapse=","),"\n",sep="")
    next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
  }else
    # sinon, alors tout est ok et on rajoute a la liste de successeurs
    next.mp <- c(next.mp, tmp)
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)
# si tu veux afficher les lignes de la table correspondant a un individu et son successeur, tu fais par exemple pour le 105eme individu identifie :
#rbind(tab.ass[idx[50],], tab.ass[next.mp[50],])

# parmi ces successeurs, lesquels sont noirs ? (spoiler : 8)
ii <- which(tab.ass[next.mp,"Black"])
print(tab.ass[next.mp[ii],])

#lesquels sont Asian : (12)
ij <- which(tab.ass[next.mp,"Asian"])
print(tab.ass[next.mp[ij],])

#lesquels sont Hispanic : 
ik <- which(tab.ass[next.mp,"Hispanic"])
print(tab.ass[next.mp[ik],])

#lesquels sont White : 
il <- which(tab.ass[next.mp,"White"])
print(tab.ass[next.mp[il],])


#Decompte version table + ajout du sexe : 
# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.ass[next.mp,"Race"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.ass[next.mp,"Race"], tab.ass[next.mp,"Female"], useNA="always"  )





### PARTIS ###



#J'identifie les Dem puis les Rep termed out, et je regarde le party de la personne qui les remplace : 
idx <- which(tab.ass[,"Party"]== "R" & tab.ass[,"Exit"]=="Termed out")
#(80 R)

table(tab.ass[,"Party"])

#J'identifie cb de femmes parmi les 51 R termed out:
Rwoto <- which(tab.ass[,"Party"]=="R" & tab.ass[,"Exit"]=="Termed out" & tab.ass[,"Female"]== TRUE)
#(15 femmes R termed out)

# on recupere la legislature de ces individus :
terms <- tab.ass[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years[length(years)])    # l'expression years[length(years)] correspond a la derniere legislature dans notre liste d'annees years
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
	terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.ass[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years) represente le numero de la legislature dans notre liste d'annees years
# donc match(terms,years)+1 correspond a la position de l'annee suivante, toujours dans years
# et donc years[match(terms,years)+1] est la valeur (l'annee) localisee a cette position dans years, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years[match(terms,years)+1]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.ass[,"Term"]==next.terms[i] & tab.ass[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste(tmp,colapse=","),"\n",sep="")
    next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
  }else
    # sinon, alors tout est ok et on rajoute a la liste de successeurs
    next.mp <- c(next.mp, tmp)
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)

#Decompte version table + ajout du sexe : 
# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.ass[next.mp,"Party"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.ass[next.mp,"Race"], tab.ass[next.mp,"Female"], useNA="always"  )


#J'identifie les Dem  termed out, et je regarde le party de la personne qui les remplace : 
idx <- which(tab.ass[,"Party"]== "D" & tab.ass[,"Exit"]=="Termed out")
#(97 R)

#J'identifie cb de femmes parmi les 51 R termed out:
Dwoto <- which(tab.ass[,"Party"]=="D" & tab.ass[,"Exit"]=="Termed out" & tab.ass[,"Female"]== TRUE)
#(38 femmes R termed out)

# on recupere la legislature de ces individus :
terms <- tab.ass[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years[length(years)])    # l'expression years[length(years)] correspond a la derniere legislature dans notre liste d'annees years
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.ass[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years) represente le numero de la legislature dans notre liste d'annees years
# donc match(terms,years)+1 correspond a la position de l'annee suivante, toujours dans years
# et donc years[match(terms,years)+1] est la valeur (l'annee) localisee a cette position dans years, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years[match(terms,years)+1]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.ass[,"Term"]==next.terms[i] & tab.ass[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste(tmp,colapse=","),"\n",sep="")
    next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
  }else
    # sinon, alors tout est ok et on rajoute a la liste de successeurs
    next.mp <- c(next.mp, tmp)
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)

#Decompte version table + ajout du sexe : 
# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.ass[next.mp,"Party"], useNA="always")