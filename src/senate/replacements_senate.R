#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Analyse : qui remplace qui - SENATE
#####
##### JANVIER 2022
#####
#######################################################

# on identifie les blancs termed out
idx <- which(tab.sen[,"White"] & tab.sen[,"Exit"]=="Termed out")
# (il y en a 47)

#J'identifie combien sont des femmes blanches (12) :
whiwoto <- which(tab.sen[,"White"] & tab.sen[,"Exit"]=="Termed out" & tab.sen[,"Female"]== TRUE)

# on recupere la legislature de ces individus :
terms <- tab.sen[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years.sen[length(years.sen)])    # l'expression years.sen[length(years.sen)] correspond a la derniere legislature dans notre liste d'annees years.sen
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
	terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.sen[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years.sen) represente le numero de la legislature dans notre liste d'annees years.sen
# donc match(terms,years.sen)+2 correspond a la position de l'annee deux fois suivante, toujours dans years.sen
# et donc years.sen[match(terms,years.sen)+2] est la valeur (l'annee) localisee a cette position dans years.sen, i.e. l'annee de la legislature deux fois suivante qu'on veut
next.terms <- years.sen[match(terms,years.sen)+2]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.sen[,"Term"]==next.terms[i] & tab.sen[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme (L44) avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste0(tmp,colapse=", "),"\n",sep="")
	if(length(tmp)>1)
	{	earliest <- tmp[which.min(tab.sen[tmp,"Beginning term"])]
		cat(">> On garde le mandat plus precoce (",earliest,")\n",sep="")
		next.mp <- c(next.mp, earliest)
	}
	else
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
ii <- which(tab.sen[next.mp,"Black"])
print(tab.sen[next.mp[ii],])

#lesquels sont Asian : (3)
ij <- which(tab.sen[next.mp,"Asian"])
print(tab.sen[next.mp[ij],])

#lesquels sont Hispanic (6): 
ik <- which(tab.sen[next.mp,"Hispanic"])
print(tab.sen[next.mp[ik],])

#lesquels sont White (36) : 
il <- which(tab.sen[next.mp,"White"])
print(tab.sen[next.mp[il],])


# lesquelles sont des femmes ? (spoiler : 14)
ii <- which(tab.sen[next.mp,"Female"])
print(tab.sen[next.mp[ii],])
# lequelles sont des femmes noires ? (spoiler: zero)
ii <- which(tab.sen[next.mp,"Female"] &  tab.sen[next.mp,"Hispanic"])
print(tab.sen[next.mp[ii],])
# si tu preferes avoir le decompte plutot que la liste complete, il faut utiliser en plus la fonction length comme ca : length(which(...............))

# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.sen[next.mp,"Race"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.sen[next.mp,"Race"], tab.sen[next.mp,"Female"], useNA="always"  )







#Je refais la meme chose pour chacune des races : 



# on identifie les hispanic termed out
idx <- which(tab.sen[,"Hispanic"] & tab.sen[,"Exit"]=="Termed out")
# (il y en a 15)

#J'identifie cb de femmes parmi les 15 hispanic termed out (5) : 
hiswoto <- which(tab.sen[,"Hispanic"] & tab.sen[,"Exit"]=="Termed out" & tab.sen[,"Female"]== TRUE)

# on recupere la legislature de ces individus :
terms <- tab.sen[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years.sen[length(years.sen)])    # l'expression years.sen[length(years.sen)] correspond a la derniere legislature dans notre liste d'annees years.sen
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
	terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.sen[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years.sen) represente le numero de la legislature dans notre liste d'annees years.sen
# donc match(terms,years.sen)+2 correspond a la position de l'annee deux fois suivante, toujours dans years.sen
# et donc years.sen[match(terms,years.sen)+2] est la valeur (l'annee) localisee a cette position dans years.sen, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years.sen[match(terms,years.sen)+2]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.sen[,"Term"]==next.terms[i] & tab.sen[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme (L129) avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste0(tmp,colapse=", "),"\n",sep="")
	if(length(tmp)>1)
	{	earliest <- tmp[which.min(tab.sen[tmp,"Beginning term"])]
		cat(">> On garde le mandat plus precoce (",earliest,")\n",sep="")
		next.mp <- c(next.mp, earliest)
	}
	else
		next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
  }else
    # sinon, alors tout est ok et on rajoute a la liste de successeurs
    next.mp <- c(next.mp, tmp)
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)
# si tu veux afficher les lignes de la table correspondant a un individu et son successeur, tu fais par exemple pour le 105eme individu identifie :
#rbind(tab.ass[idx[50],], tab.ass[next.mp[50],])

# parmi ces successeurs, lesquels sont noirs ? (spoiler : 1)
ii <- which(tab.sen[next.mp,"Black"])
print(tab.sen[next.mp[ii],])

#lesquels sont Asian : (1)
ij <- which(tab.sen[next.mp,"Asian"])
print(tab.sen[next.mp[ij],])

#lesquels sont Hispanic (6) : 
ik <- which(tab.sen[next.mp,"Hispanic"])
print(tab.sen[next.mp[ik],])

#lesquels sont White (6) : 
il <- which(tab.sen[next.mp,"White"])
print(tab.sen[next.mp[il],])

#Decompte version table + ajout du sexe : 
# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.sen[next.mp,"Race"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.sen[next.mp,"Race"], tab.sen[next.mp,"Female"], useNA="always"  )




# on identifie les asian termed out
idx <- which(tab.sen[,"Asian"] & tab.sen[,"Exit"]=="Termed out")
# (il y en a 2)

#J'identifie cb de femmes parmi les 2 asiat termed out (1) : 
aswoto <- which(tab.sen[,"Asian"] & tab.sen[,"Exit"]=="Termed out" & tab.sen[,"Female"]== TRUE)

# on recupere la legislature de ces individus :
terms <- tab.sen[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years.sen[length(years.sen)])    # l'expression years.sen[length(years.sen)] correspond a la derniere legislature dans notre liste d'annees years.sen
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
	terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.sen[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years.sen) represente le numero de la legislature dans notre liste d'annees years.sen
# donc match(terms,years.sen)+1 correspond a la position de l'annee suivante, toujours dans years.sen
# et donc years.sen[match(terms,years.sen)+1] est la valeur (l'annee) localisee a cette position dans years.sen, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years.sen[match(terms,years.sen)+2]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.sen[,"Term"]==next.terms[i] & tab.sen[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme (L199) avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste0(tmp,colapse=", "),"\n",sep="")
	if(length(tmp)>1)
	{	earliest <- tmp[which.min(tab.sen[tmp,"Beginning term"])]
		cat(">> On garde le mandat plus precoce (",earliest,")\n",sep="")
		next.mp <- c(next.mp, earliest)
	}
	else
		next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
  }else
    # sinon, alors tout est ok et on rajoute a la liste de successeurs
    next.mp <- c(next.mp, tmp)
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)
# si tu veux afficher les lignes de la table correspondant a un individu et son successeur, tu fais par exemple pour le 105eme individu identifie :
#rbind(tab.ass[idx[50],], tab.ass[next.mp[50],])



#Decompte version table + ajout du sexe : 
# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.sen[next.mp,"Race"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.sen[next.mp,"Race"], tab.sen[next.mp,"Female"], useNA="always"  )



# on identifie les noirs termed out
idx <- which(tab.sen[,"Black"] & tab.sen[,"Exit"]=="Termed out")
# (il y en a 2)

#J'identifie cb de femmes parmi les 10 noirs termed out:
blwoto <- which(tab.sen[,"Black"] & tab.sen[,"Exit"]=="Termed out" & tab.sen[,"Female"]== TRUE)

# on recupere la legislature de ces individus :
terms <- tab.sen[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years.sen[length(years.sen)])    # l'expression years.sen[length(years.sen)] correspond a la derniere legislature dans notre liste d'annees years.sen
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
	terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.sen[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years.sen) represente le numero de la legislature dans notre liste d'annees years.sen
# donc match(terms,years.sen)+1 correspond a la position de l'annee suivante, toujours dans years.sen
# et donc years.sen[match(terms,years.sen)+1] est la valeur (l'annee) localisee a cette position dans years.sen, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years.sen[match(terms,years.sen)+2]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.sen[,"Term"]==next.terms[i] & tab.sen[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme (L254) avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste0(tmp,colapse=", "),"\n",sep="")
	if(length(tmp)>1)
	{	earliest <- tmp[which.min(tab.sen[tmp,"Beginning term"])]
		cat(">> On garde le mandat plus precoce (",earliest,")\n",sep="")
		next.mp <- c(next.mp, earliest)
	}
	else
    	next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
  }else
    # sinon, alors tout est ok et on rajoute a la liste de successeurs
    next.mp <- c(next.mp, tmp)
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)
# si tu veux afficher les lignes de la table correspondant a un individu et son successeur, tu fais par exemple pour le 105eme individu identifie :
#rbind(tab.ass[idx[50],], tab.ass[next.mp[50],])



#Decompte version table + ajout du sexe : 
# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.sen[next.mp,"Race"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.sen[next.mp,"Race"], tab.sen[next.mp,"Female"], useNA="always"  )



#### NOTE ####
#Pour tous ceux qui ont plusieurs successeurs, je vais chercher leur identit� manuellement dans tab.sen en me r�f�rant � la ligne annonc�e dans le message d'erreur





### PARTIES ###

#J'identifie les Rep termed out, et je regarde le party de la personne qui les remplace : 
idx <- which(tab.sen[,"Party"]== "R" & tab.sen[,"Exit"]=="Termed out")
#(23 R)

#J'identifie cb de femmes parmi les 51 R termed out:
Rwoto <- which(tab.sen[,"Party"]=="R" & tab.sen[,"Exit"]=="Termed out" & tab.sen[,"Female"]== TRUE)
#(1 femmes R termed out)

# on recupere la legislature de ces individus :
terms <- tab.sen[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years.sen[length(years.sen)])    # l'expression years.sen[length(years.sen)] correspond a la derniere legislature dans notre liste d'annees years.sen
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
	terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.sen[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years.sen) represente le numero de la legislature dans notre liste d'annees years.sen
# donc match(terms,years.sen)+1 correspond a la position de l'annee suivante, toujours dans years.sen
# et donc years.sen[match(terms,years.sen)+1] est la valeur (l'annee) localisee a cette position dans years.sen, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years.sen[match(terms,years.sen)+2]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.sen[,"Term"]==next.terms[i] & tab.sen[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme (L319) avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste0(tmp,colapse=", "),"\n",sep="")
	if(length(tmp)>1)
	{	earliest <- tmp[which.min(tab.sen[tmp,"Beginning term"])]
		cat(">> On garde le mandat plus precoce (",earliest,")\n",sep="")
		next.mp <- c(next.mp, earliest)
	}
	else
		next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
  }else
    # sinon, alors tout est ok et on rajoute a la liste de successeurs
    next.mp <- c(next.mp, tmp)
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)
# si tu veux afficher les lignes de la table correspondant a un individu et son successeur, tu fais par exemple pour le 105eme individu identifie :
#rbind(tab.ass[idx[50],], tab.ass[next.mp[50],])



#Decompte version table + ajout du sexe : 
# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.sen[next.mp,"Party"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.sen[next.mp,"Party"], tab.sen[next.mp,"Female"], useNA="always"  )





#J'identifie les Dem termed out, et je regarde le party de la personne qui les remplace : 
idx <- which(tab.sen[,"Party"]== "D" & tab.sen[,"Exit"]=="Termed out")
#(45 D)

#J'identifie cb de femmes parmi les 51 R termed out:
Dwoto <- which(tab.sen[,"Party"]=="D" & tab.sen[,"Exit"]=="Termed out" & tab.sen[,"Female"]== TRUE)
#(19 femmes D termed out)

# on recupere la legislature de ces individus :
terms <- tab.sen[idx, "Term"]
# quels sont les cas relevant de la derniere legislature ? (et qui n'ont donc pas de successeur) :
last <- which(terms==years.sen[length(years.sen)])    # l'expression years.sen[length(years.sen)] correspond a la derniere legislature dans notre liste d'annees years.sen
# on les supprime de idx et de terms : (enfin, seulement si on en a trouve
if(length(last)>0)
{	idx <- idx[-last]                           # le signe "-" dans le tableau permet de retirer les valeurs designees par last
	terms <- terms[-last]                       # pareil
}
# on recupere le district des individus restants :  
districts <- tab.sen[idx, "District"]

# on recupere la legislature suivante :
# l'expression match(terms,years.sen) represente le numero de la legislature dans notre liste d'annees years.sen
# donc match(terms,years.sen)+1 correspond a la position de l'annee suivante, toujours dans years.sen
# et donc years.sen[match(terms,years.sen)+1] est la valeur (l'annee) localisee a cette position dans years.sen, i.e. l'annee de la legislature suivante qu'on veut
next.terms <- years.sen[match(terms,years.sen)+2]
# si tu veux t'assurer que ca marche, tape ensuite la commande : cbind(terms, next.terms), ca affichera les deux cote a cote

# on recupere les individus qui occupent le meme poste a la legislature suivante
next.mp <- c()                      # variable next.mp qui va contenir les successeurs (pour l'instant, elle est vide)
for(i in 1:length(idx))             # on traite iterativement chaque cas identifie precedemment dans idx
{ # on recupere toutes les lignes (normalement : une seule) qui relevent du meme district que l'individu traite a cette iteration, mais mais de la legislature suivante
  tmp <- which(tab.sen[,"Term"]==next.terms[i] & tab.sen[,"District"]==districts[i])
  # on teste qu'on a exactement 1 ligne qui correspond, sinon il y a un probleme
  if(length(tmp)!=1)
  { # on affiche un message d'erreur avec la ligne problematique
    cat("Probleme (L377) avec la ligne ",idx[i]," : on trouve zero ou plusieurs successeurs : ",paste0(tmp,colapse=", "),"\n",sep="")
	if(length(tmp)>1)
	{	earliest <- tmp[which.min(tab.sen[tmp,"Beginning term"])]
		cat(">> On garde le mandat plus precoce (",earliest,")\n",sep="")
		next.mp <- c(next.mp, earliest)
	}
	else
		next.mp <- c(next.mp, NA)       # on rajoute NA a la liste des successeurs (vu qu'on n'a pas trouve le successeur...)
  }else
    # sinon, alors tout est ok et on rajoute a la liste de successeurs
    next.mp <- c(next.mp, tmp)
}                                   # fin de la boucle for
# si tu veux voir (pour verification) la liste des numeros de lignes des individus sur une colonne, et a cote le numero de ligne de leur successeur, tu fais :
cbind(idx,next.mp)
# si tu veux afficher les lignes de la table correspondant a un individu et son successeur, tu fais par exemple pour le 105eme individu identifie :
#rbind(tab.ass[idx[50],], tab.ass[next.mp[50],])



#Decompte version table + ajout du sexe : 
# tu peux aussi faire des decomptes avec la fonction table, par exemple : distribution de la "race" parmi les successeurs des hommes blancs termed out 
table(tab.sen[next.mp,"Party"], useNA="always")
# croisement "race" x sexe pour les memes :
table(tab.sen[next.mp,"Party"], tab.sen[next.mp,"Female"], useNA="always"  )
