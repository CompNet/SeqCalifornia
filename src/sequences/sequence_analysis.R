#######################################################
##### DONNEES CALIFORNIA PARLEMENT COMPLET #####
#####
##### ANALYSE DE SEQUENCES TABLE FUSIONNEE
#####
##### AVRIL 2021 - AVRIL 2022
#######################################################




###############################################################
############## CONVERSION AU FORMAT TRAMINER ##################
###############################################################

# on recupere l'objet Traminer au moyen de la fonction prevue a cette effet
sd <- convert.to.sequences(
	# table contenant les donnees
	tab.mandates=tab.seq,
	# date de debut de la periode a couvrir, au format annee/mois/jour
	start.date=as.Date("1959/12/01"),
	# date de fin de cette periode, au meme format
	end.date=as.Date("2022/12/01"),
	# option permettant de ne *pas* aligner les sequences a gauche
	left=NA,
	# couleurs Ã  utiliser
  	colors=CAT_COLORS_8				# vraies couleurs
#	colors=get.gray.palette(6)		# 6 niveaux de gris
)
# graphe de transitions
plot.transition.graph(sd, seq.folder=seq.folder, file.name="all_")


# on recupere l'objet Traminer no2 dans sa version a-historique au moyen de cette fonction
sd.left <- convert.to.sequences(
	# table contenant les donnees
	tab.mandates=tab.seq,
	# date de debut de la periode a couvrir, au format annee/mois/jour
	start.date=as.Date("1959/01/01"),
	# date de fin de cette periode, au meme format
	end.date=as.Date("2022/12/01"),
	# option permettant d'aligner les sequences a gauche
	left="DEL",
	# couleurs Ã  utiliser
  	colors=CAT_COLORS_8				# vraies couleurs
#	colors=get.gray.palette(6)		# 6 niveaux de gris
)




##########################################################
################## ANALYSE DE SEQUENCES ##################
##########################################################

# test
summary(sd)

png(file.path(seq.folder, "seqIplot.png"))
seqIplot(
	sd, 
	sortv="from.start", 
	ylas=2,	
)
dev.off()


#idem pour la version a-historique : 
png(file.path(seq.folder, "seqIplot.2.png"),width=1000, height=480)
seqIplot(
	sd.left, 
	sortv="from.start", 
	ylas=2,
	with.legend=FALSE,
	xlab=NA,							# n'affiche pas le nom de l'axe x
	xtlab=FALSE							# n'affiche pas les valeurs sur l'axe x
)
# rajoute manuellement l'axe x, avec plus de controle
axis(
  1,									# 1 pour x et 2 pour y 
  at=0:ncol(sd), 						# position des labels sur l'axe
  labels=0:ncol(sd), 					# texte des labels : numero des etats

)
dev.off()

# Reprise du test suivant la fusion : 
# test
summary(sd)
seqIplot(
	sd,
	sortv="from.start",
	with.missing=FALSE,
	ylas=2,
	with.legend=FALSE
)


# Je commence par generer la legende separement pour plus de lisibilite: 
seqlegend(sd)

#Je trouve que les NA de debut de carriere prennent trop de place
#J'aimerai donc generer un plot ou j'aligne tous les debuts de carriere
#RESOLU : ce sont tous mes plots dans la version "a-historique"




########################################################
### J'explore les autres representations visuelles : ###
########################################################


# Distribution de tous les etats sur toute la periode, en frequence : 
png(file.path(seq.folder, "seqdplot.png"))
seqdplot(
	sd,
	border=NA,
	with.legend=FALSE
)
dev.off()

#creation d'un seqdplot dans sa version ahistorique : 
png(file.path(seq.folder, "seqdplot2.png"))
seqdplot(
  sd.left,
  border=NA,
  with.legend=FALSE
)
dev.off()

# Logiquement une majorite de StA et StS, puisque ce sont les deux types de mandats que j'observe
# Dans sa version 2 ou ahitorique, beaucoup plus revelateur de l'evolution de la carriere puisque je n'ai que des personnes ayant eu un passage par le parlement

# Afficher les valeurs des frequences du plot precedent, annee par annee: 
seqstatd(sd)


# Les 10 premieres sequences visibles dans mon objet : 
png(file.path(seq.folder, "10_first_freq.png"))
seqiplot(
	sd,
	with.legend=FALSE,
	with.missing=FALSE
)
dev.off()

# Idem dans la version a-historique :
png(file.path(seq.folder, "10_first_freq_2.png"))
seqiplot(
  sd.left,
  with.legend=FALSE,
  with.missing=FALSE
)
dev.off()


# Sequences les plus frequentes : 
png(file.path(seq.folder, "seq_more_freq.png"))
seqfplot(
	sd,
	with.legend=FALSE,
	with.missing=FALSE
)
dev.off()


# Idem, missing du debut a retirer, donc version "a-historique" : 
png(file.path(seq.folder, "seq_more_freq_2_pre.png"))
seqfplot(
  sd.left,
  with.legend=FALSE,
  with.missing=FALSE
)
dev.off()


#temps moyen passé dans chaque état
png(file.path(seq.folder, "seq_meantime.png"))
seqmtplot(
  sd.left,
  with.legend=FALSE,
  with.missing=FALSE,
  ylim=c(0,12)
)
dev.off()


#Les taux de transition / passage d'un etat a l'autre :
trans <- seqtrate(sd)
round(trans, 2)
# plus proche de 1, plus la probabilite est importante
# peut m'aider a definir les couts pour l'Optimal Matching ??

# Verification (je ne devrais avoir que 1 partout) :
rowSums(trans)
# Verification ok outre USS.





##############################################
##### VISUALISER LES SEQUENCES ##############
#############################################


# Visualiser toutes les sequences avec les successions d'etats codes : 
seqdef(sd)

# Visulaiser les sequences au format SPS : 
#Ex ligne 1 : (NA,36) - (StA, 2) - (StS, 8) - (NA, 10)
print(sd, "SPS")

# Je peux a partir du format SPS compter la liste des etats uniques dans la sequence : 
# Ex ligne 1 : StA - StS (enleve les NA)
seqdss(sd)

# Puis je peux compter le nombre de transition dans chaque sequence : 
sd.dss <- seqdss(sd)
seqlength(sd.dss)

# Le temps passe dans chaque etat pour chacune des sequences : 
seqistatd(sd)

# Calculer l'entropy : plus proche de 1, plus la sequence contient des etats divers / plus proche 0 moins il y a d'etats differents : 
sd.ent <- seqient(sd)
summary(sd.ent)

# Repartition de l'entropie en histogramme : 
png(file.path(seq.folder, "entropy.png"))
hist(sd.ent,
     col="orange",
     main=NULL,
     xlab="Entropy")
dev.off()

# Je recherche qui a la plus forte entropie : 
index <- which(sd.ent== max(sd.ent))
# ligne 526 ?
sd[sd.ent==max(sd.ent),] #Selon le guide TraMineR, cette commande fonctionne un peu mysterieusement

# Je calcule l'entropie selon le sexe : 
# Je dois utiliser boxplot



############################
##### Optimal Matching #####
############################

# Pour un premier essai, je calcule tous les couts de maniere constante (=2)
couts <- seqsubm(
	sd,
	method="CONSTANT",
	cval=2,
	with.missing=TRUE
)
# Je fixe ensuite les matrices de distance / dissimilarites en repartant de mon cout et en attribuant un cout de 1 aux indel:
seq.om <- seqdist(
	sd,
    method="OM",
    indel=1,
    sm=couts,
    with.missing=TRUE
)
seq.om

# Je refais ces 2 etapes mais en attribuant des couts differents
couts <- seqsubm(
  sd,
  method="TRATE",
  with.missing=TRUE
)

seq.om <- seqdist(
  sd,
  method="OM",
  indel=1,
  sm=couts,
  with.missing=TRUE
)
seq.om

# NB : j'ai tout mis au hasard pour tester, pas de reflexion sur les couts et substitution



# Enfin, je cree les classes / groupes de regroupement pour permettre une representation visuelle des matrices de distance: 
seq.dist <- hclust(
	as.dist(seq.om),
	method="ward.D2"
)


png(file.path(seq.folder, "dendrogram_pre.png"))
plot(as.dendrogram(seq.dist), leaflab="none")
abline(	# trace une ligne droite (horizontale ici, mais elle peut etre quelconque)
#	a=...,		# coefficient directeur (pour les lignes ni horizontales ni verticales)
#	b=...,		# ordonnee a l'origine (ni horizontal ni vertical)
	h=125,		# hauteur, si c'est une ligne horizontale	
#	v=...,		# position, si c'est une ligne verticale
	col="RED",	# couleur de la ligne
	lty=2		# type de ligne (trait plein, pointilles, etc. voir : http://www.sthda.com/english/wiki/line-types-in-r-lty
)
abline(	# trace une ligne droite (horizontale ici, mais elle peut etre quelconque)
  #	a=...,		# coefficient directeur (pour les lignes ni horizontales ni verticales)
  #	b=...,		# ordonnee a l'origine (ni horizontal ni vertical)
  h=200,		# hauteur, si c'est une ligne horizontale	
  #	v=...,		# position, si c'est une ligne verticale
  col="BLUE",	# couleur de la ligne
  lty=2		# type de ligne (trait plein, pointilles, etc. voir : http://www.sthda.com/english/wiki/line-types-in-r-lty
)
dev.off()
# Je trace une ligne verticale legermenet au dessus de 125 et coupe ainsi 6 fois une ligne verticale
#Une autre au dessus de 200 pour couper 3 fois
# note de VL : tu peux carremment tracer la ligne dans le graphique, je t'ai rajoute l'instruction ci-dessus

# Une autre methode me permettant de decider du nb de classes est le mix dendrogramme + index plot
# Je commence par creer ma propre fonction "seq_heatmap"

seq_heatmap <- function (seq, tree, with.missing=FALSE, ...) {
  if (class(tree)!="dendrogram") tree <- as.dendrogram(tree)
  mat <- seq
  for (i in 1:length(seq)){
    mat[mat[,i]=="%",i] <- NA
    mat[,i] <- as.numeric(mat[,i])
  }
  mat <- as.matrix(mat)
  col <- attr(seq,"cpal")
  if(with.missing) col <- c(col,attr(seq,"missing.color"))
  heatmap(mat, tree, NA,  na.rm=FALSE, col=col, scale="none", labRow=NA, ...)	
}

# Puis j'applique la fonction nouvellement cree dans une version historique
# Et egalement dans une version a-historique : 
# Je reprends ma fonction et lui applique le tableau de sequence sd + le dendrogramme obtenu avec hclust (seq.dist)

png(file.path(seq.folder, "tapis_dendro.png"))
seq_heatmap(sd, seq.dist)
dev.off()

# Idem pour la version a-historique
png(file.path(seq.folder, "tapis_dendro_a-histo.png"))
seq_heatmap(sd.left, seq.dist)
dev.off()

# PB : je n'ai plus les memes couleurs qu'utilisees dans la legende... 
