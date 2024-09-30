#######################################################
##### DONNEES CALIFORNIA PARLEMENT COMPLET #####
#####
##### DESCRIPTION 1 - TYPOLOGIES EN 7 CLASSES
#####
##### AVRIL 2022
#######################################################




#################################################
########### PARTITION EN 6 CLASSES ##############
#################################################

# Je vais essayer plusieurs decoupages afin de determiner le niveau le plus pertinent pour moi
# Je passe au decoupage en 6 dans ce script.


# D'abord, je coupe en 6 classes
nbcl <- 6
seq.part6 <- cutree(seq.dist, nbcl)
seq.part6 <- factor(
  seq.part6,
  labels=paste("classe", 1:nbcl, sep="." )
)

# 1ere tentative de typologie - sequences en tapis :
png(file.path(seq.folder, "Typologie_6.png"))	# cree un fichier contenant le graphique
seqdplot(
  sd,
  group=seq.part6,
  border=NA,
)
dev.off()

png(file.path(seq.folder, "sequences_classes_6.png"))
seqIplot(
  sd,
  group=seq.part6,
)
dev.off()


# Je peux ensuite reproduire ce tapis de sequences en triant les sequences
# Je trie par multidimensional scaling
# Permet d'apporter plus de lisibilite : 

ordre <- cmdscale(as.dist(seq.om), k=1)

png(file.path(seq.folder, "sequences_classes_cmdscale_6.png"))
seqIplot(
  sd,
  group=seq.part6,
  sortv=ordre,
)
dev.off()

# Trop de NA... Il faudrait aussi pouvoir creer le seqIplot non pas par date de debut de la periode mais date de debut du 1er mandat de la sequence
# J'ajoute donc la mm version de ces tapis (typo et tapis de sequences) en utilisant la version "a-historique" des sequences
# J'utilise donc "sd.left", j'aligne les sequences non pas sur une DATE mais sur un EVENEMENT
# Cet evenement est le premier mandat connu pour chacune des sequences

png(file.path(seq.folder, "Typologie_a-histo_6.png"))	# cree un fichier contenant le graphique
seqdplot(
  sd.left,
  group=seq.part6,
  border=NA,
)
dev.off()


png(file.path(seq.folder, "sequences_classes_a-histo_6.png"))
seqIplot(
  sd.left,
  group=seq.part6,
)
dev.off()


# Autre visualisation : la repartision des Etats par classes
# Je pense que la repartition est la meme qu'on soit en version DATE ou version AHISTORIQUE
# Oui, idem. 

png(file.path(seq.folder, "repartition_etats_classes_6.png"))
seqmtplot(sd,
          group=seq.part6,
          border=NA)
dev.off()

png(file.path(seq.folder, "repartition_etats_classes_a-histo_6.png"))
seqmtplot(
	sd.left,
	group=seq.part6,
	border=NA)
dev.off()



##############################################################
################# DESCRIPTION DE LA TYPOLOGIE ################
##############################################################

# Le poids des classes
table(seq.part6)

# Pourcentage
100*(table (seq.part6))/length(seq.part6)

# Homogeneite des classes
# La distance moyenne des sequences d'une classe au centre de cette classe permet de mesurer plus precisement l'homogeneite: 
dist.to.centers <- disscenter(as.dist(seq.om), group=seq.part6)
round(aggregate(dist.to.centers, list(seq.part6),mean)[,-1],1)

# Un autre indicateur de l'homogeneite interne a une classe est l'entropie transversale
# Elle decrit l'evolution de l'homogeneite de la classe pour chaque temps t
# Plus proche de 0, plus l'entreopie est faible, plus les individus de la classe sont tous dans la meme situation

png(file.path(seq.folder, "entropie_transversale_6.png"))
seqHtplot(
	sd, 
	group=seq.part6
)
dev.off()




##############################################
##### CROISEMENT AVEC ATTRIBUTS SOCIAUX ##### 
##############################################

# Je poursuis l'exploration de mes donnes et je vais au-dela des "simples" typologies
# Bien que la typologie soit deja un resultat en soi
# Je vais aller plus loin en cherchant les facteurs d'appartenance a telle ou telle classe
# La classe devient le facteur a expliquer. 



# on recupere les ids des elus presents dans la table de sequences (les autres n'ont pas de sequence)
# En effet, je ne peux travailler sur tous les elus de de repr.seq puisque certains elues n'ont pas ete representes dans les sequences
# Ces individus ont ete exclues car leur carriere etaient trop courtes ou hors periode temporelle

seq.ids <- rownames(sd)


# Le nombre de femmes dans les classes : 
assoc.twocat(factor(seq.part6),
             factor(repr.seq[seq.ids,"Female"]))


# Je realise la meme chose pour les races : 
assoc.twocat(factor(seq.part6),
             factor(repr.seq[seq.ids, "Race"]))

# Je realise la meme chose pour les partis :
assoc.twocat(factor(seq.part6),
             factor(repr.seq[seq.ids, "Party"]))





###############################################################################
################### MATRICE DE DISTANCES ###################################
##############################################################################


# Je cherche ensuite a comparer l'homogeneite entre les femmes / et celle entre les hommes
# Je passe pour ce faire par la distances intra-classe selon le sexe :

Dintrasexe <- sapply(levels(repr.seq[seq.ids,"Female"]), 
                     function(x) 
                       round(mean(dissim[repr.seq[seq.ids,"Female"]==x,repr.seq[seq.ids,"Female"]==x]),
                             1))
# RESULTAT VIDE...



###############
# La matrice des distances entre trajectoire resumee graphiquement
png(file.path(seq.folder, "distance_class_6.png"))

# projete les donnees dans le plan : chaque point represente une sequence
# les coordonnees des points sont exprimees dans des unites arbitraires :
# elles n'ont pas de signification particuliere, si ce n'est de positionner les points de facon relative.
# la projection est faite en essayant de respecter les distances,
# i.e. la distance entre 2 points reflete la distance entre les sequences correspondantes
mds <- cmdscale(seq.om, k=2)

# initialise le graphique
plot(
  mds, 									# les points a dessiner
  type="n", 								# on choisit de ne pas les dessiner (=graphique vide)
  xlab="Dimension 1", ylab="Dimension 2"	# labels places sur les axes du graphique
)

# on rajoute les points dans le graphique auparavant vide
points(
  mds, 									# coordonnees des points a dessiner
  pch=20, 								# symbole(s) utilise pour representer chaque point (cf. http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
  col=seq.part6							# couleur(s) utilisee pour dessiner les points
  # note: on peut preciser un seul symbole ou couleur pour tous les points c'est le meme symbole/couleur qui sera utilise partout
  # on peut aussi preciser un symbole/couleur different pour chaque point
  # en l'occurrence, ci-dessus on a un seul symbole (20) mais chaque point a sa propre couleur (vecteur seq.part)
)

# on rajoute une ligne droite dans le graphique
abline(
  h=0, 									# ceci indique qu'il s'agit d'une ligne horizontale placee en y=0
  v=0, 									# ceci specifie une ligne verticale placee en x=0
  lty=2, 									# type de ligne (ici des pointilles, cf. http://www.sthda.com/english/wiki/line-types-in-r-lty
  col="lightgray"							# couleur de la ligne tracee
)

# on rajoute une legende dans le graphique
legend(
  "topright", 							# position de la legende dans le graphique
  legend=paste("Classe",1:nbcl), 			# texte a inserer dans la legende
  pch=20, 								# forme des points a dessiner dans la legende (cf. "points")
  col=1:nbcl, 							# couleur de ces points
  cex=0.8									# controle la taille du texte dans la legende
)

# on finalise le graphique
dev.off()
###############






###############
# Version modifiee, avec la classe + le sexe :
png(file.path(seq.folder, "distance_class_sex_6.png"))
plot(
  mds,
  type="n",
  xlab="Dimension 1", ylab="Dimension 2"
)
# j'utilise le symbole n.4 pour les femmes et le n.6 pour les hommes
symbols <- rep(20, length(repr.seq[seq.ids,"Female"]))
symbols[repr.seq[seq.ids,"Female"]] <- 4
points(
  mds,
  pch=symbols,
  col=seq.part6
)
legend(
  "topright",
  legend=paste("Classe",1:nbcl),
  pch=20,
  col=1:nbcl,
  cex=0.8
)
# il faut rajouter une seconde legende, pour les symboles
legend(
  "bottomleft",
  title="Sex",
  legend=c("Male","Female"),
  pch=c(20,4),
  col="BLACK",
  cex=0.8
)
dev.off()
###############




###############
# Version modifiee, avec le sexe a la place de la classe:
png(file.path(seq.folder, "distance_sex_6.png"))
plot(
  mds,
  type="n",
  xlab="Dimension 1", ylab="Dimension 2"
)
# j'utilise le symbole rose pour les femmes et le bleu pour les hommes
# (cliche, je sais, mais ca permet de comprendre tout de suite le graphique !)
cols <- rep("GREEN", length(repr.seq[seq.ids,"Female"]))
cols[repr.seq[seq.ids,"Female"]] <- "ORANGE"
points(
  mds,
  pch=20,
  col=cols
)
legend(
  "topright",
  title="Sex",
  legend=c("Male","Female"),
  pch=20,
  col=c("GREEN","ORANGE"),
  cex=0.8
)
dev.off()
###############







##############
# Je realise les deux memes plots pour la race cette fois ci : 
# Version modifiee, avec la classe + le race :
png(file.path(seq.folder, "distance_class_race_7.png"))
plot(
  mds,
  type="n",
  xlab="Dimension 1", ylab="Dimension 2"
)
# j'utilise le symbole n.4 pour les femmes et le n.20 pour les hommes
symbols <- rep(20, length(repr.seq[seq.ids,"Race"]))
symbols[repr.seq[seq.ids,"Hispanic"]] <- 4
symbols[repr.seq[seq.ids,'Black']] <-17
symbols[repr.seq[seq.ids, "Asian"]]<-22
points(
  mds,
  pch=symbols,
  col=seq.part6
)
legend(
  "topright",
  legend=paste("Classe",1:nbcl),
  pch=20,
  col=1:nbcl,
  cex=0.8
)
# il faut rajouter une seconde lÃ©gende, pour les symboles
legend(
  "bottomleft",
  title="Race",
  legend=c("White","Hispanic", "Black", "Asian"),
  pch=c(20,4, 17, 22),
  col="BLACK",
  cex=0.8
)
dev.off()
###############




###############
# Version modifiee, avec la race a la place de la classe:
png(file.path(seq.folder, "distance_race.png"))
plot(
  mds,
  type="n",
  xlab="Dimension 1", ylab="Dimension 2"
)
# j'utilise le symbole vert pour les White et le orange pour les Hispanic, Bleu pour les Black et Rose pour les Asian
# (cliche, je sais, mais ca permet de comprendre tout de suite le graphique !)
cols <- rep("GREEN", length(repr.seq[seq.ids,"Race"]))
cols[repr.seq[seq.ids,"Hispanic"]] <- "ORANGE"
cols[repr.seq[seq.ids,"Black"]] <- "BLUE"
cols[repr.seq[seq.ids,"Asian"]] <- "PINK"
points(
  mds,
  pch=20,
  col=cols
)
legend(
  "topright",
  title="Class",
  legend=c("White","Hispanic", "Black", "Asian"),
  pch=20,
  col=c("GREEN","ORANGE", "Blue", "PINK"),
  cex=0.8
)
dev.off()
###############






###############
# Je realise enfin les memes graphs pour le party : 
# Version modifiee, avec la classe + le party :
png(file.path(seq.folder, "distance_class_party_7.png"))
plot(
  mds,
  type="n",
  xlab="Dimension 1", ylab="Dimension 2"
)
# j'utilise le symbole n.1 pour les dem et le n.20 pour les rep
symbols <- rep(20, length(repr.seq[seq.ids,"Party"]))
symbols[repr.seq[seq.ids,"Party"]=="D"] <- 1
points(
  mds,
  pch=symbols,
  col=seq.part6
)
legend(
  "topright",
  legend=paste("Classe",1:nbcl),
  pch=20,
  col=1:nbcl,
  cex=0.8
)
# il faut rajouter une seconde legende, pour les symboles
legend(
  "bottomleft",
  title="Party",
  legend=c("R","D"),
  pch=c(20,4),
  col="BLACK",
  cex=0.8
)
dev.off()
###############




# Je cherche enfin le temps passe dans les differents etats selon les caracteristiques socio-dem :

# Temps passe dans les etats selon sexe : 
dur <- seqistatd(sd)
durees_sexe <- aggregate(dur, by=list(repr.seq[seq.ids,"Female"]), function(x) round(mean(x),1))
rownames(durees_sexe) <- NULL
#colnames(durees_sexe) <- c("classe",labs)
durees_sexe

# Temps passe dans les etats selon race : 
dur <- seqistatd(sd)
durees_race <- aggregate(dur, by=list(repr.seq[seq.ids,"Race"]), function(x) round(mean(x),1))
rownames(durees_race) <- NULL
#colnames(durees_race) <- c("classe",labs)
durees_race

# Temps passe dans les etats selon party : 
dur <- seqistatd(sd)
durees_party <- aggregate(dur, by=list(repr.seq[seq.ids,"Party"]), function(x) round(mean(x),1))
rownames(durees_party) <- NULL
#colnames(durees_party) <- c("classe",labs)
durees_party




########### MEDOIDES ###########

#Cette methode permet de trouver l'element d'un groupe qui est le plus proche des autres en moyenne
#J'utilise la fonction seqrplot

png(file.path(seq.folder, "medoidesmultiples_6.png"))
seqrplot(
  sd, 					# objet contenant les sequences
  group=seq.part6, 		# vecteur d'appartenance aux clusters (indique a quel cluster appartient chaque seq)
  diss=seq.om,			# matrice de dissimilarit�s, comparant chaque paire de sequences
  criteria="centrality", 	# pour indiquer qu'on veut les seq plus centrales (par opposition a d'autres criteres, comme la frequence...)
)
dev.off()


medoid.ids <- disscenter(as.dist(seq.om), group=seq.part6, medoids.index="first")
print(medoid.ids)


#Je cherche maintenant a n'avoir qu'un seul parangon par classe : 

png(file.path(seq.folder, "medoidesimple_6.png"))
seqrplot(
  sd, 					# objet contenant les sequences
  group=seq.part6, 		# vecteur d'appartenance aux clusters (indique a quel cluster appartient chaque seq)
  diss=seq.om,			# matrice de dissimilarit�s, comparant chaque paire de sequences
  criteria="centrality", 	# pour indiquer qu'on veut les seq plus centrales (par opposition a d'autres criteres, comme la frequence...)
  nrep=1					# si on veut forcer a n'avoir qu'un seul medoide
)
dev.off()


