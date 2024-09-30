#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Analyse : diversite
#####
##### FEVRIER 2021
#####
#######################################################

###### Combien ai-je d'elu.es au total sur les 891 mandats attribues sur la periode ?

#Je rappelle que j'ai 555 noms uniques sur toute la periode. 
table(tab.ass[,"Id"])

# Nb femmes total
table(repr.ass[,"Female"]== TRUE)

#Nb ethnic (a modifier selon analyse en cours) total
table(repr.ass[,"Race"])

#Nb ethnic par genre: 
racegenre <- table(repr.ass[,"Race"], repr.ass[,"Female"])

png(file.path(plots.folder, "assembly", "female_race_ass.png"))
barplot(
	racegenre,
	legend.text = TRUE, 
	col=topo.colors(4),
	names.arg=c("Male","Female")
)
dev.off()

#Je realise le meme plot en pourcentage
pc <- prop.table(racegenre)* 100
png(file.path(plots.folder, "assembly", "female_race_ass_percent.png"))
barplot(
  pc,
  legend.text = TRUE, 
  col=topo.colors(4),
  names.arg=c("Male","Female")
)
dev.off()



table(repr.ass[,"Asian"]== TRUE, repr.ass[,"Female"]== TRUE)


#Femmes par parti sur les mandats totaux : 
partygenre <- table(tab.ass[,"Party"], tab.ass[,"Female"])

png(file.path(plots.folder, "assembly", "female_party_ass.png"))
barplot(
  partygenre,
  legend.text = TRUE, 
  col=topo.colors(4),
  names.arg=c("Male","Female")
)
dev.off()


#Je realise le meme plot en pourcentage
pc <- prop.table(partygenre)* 100
png(file.path(plots.folder, "assembly", "female_party_ass_percent.png"))
barplot(
  pc,
  legend.text = TRUE, 
  col=topo.colors(4),
  names.arg=c("Male","Female")
)
dev.off()


###### Cb y a-t-il de femmes chaque legis?

#Repartition des femmes a chaque legis : 
tw <-table(tab.ass[,"Female"], tab.ass[,"Term"])
View (tw)

png(file.path(plots.folder, "assembly", "men_women_legis_ass.png"))
barplot(
	tw, 
	legend.text = c("Men","Women"),
	col=topo.colors(6), 
	xlim=c(0, 1.5*ncol(tw)),
	las=2	# labels x verticaux
)
dev.off()

#Resultat de la repartition en pourcentage : 
prop.table(tw)*100


#Apres 2012 pas du tout une tendance a l'augmentation des femmes si ce n'est l'inverse.
#2016 le moins de femmes avec seulement 18 sur 80.

#Repartition des "races" a chaque legis : 
tr <-table(tab.ass[,"Race"], tab.ass[,"Term"])
View(tr)

png(file.path(plots.folder, "assembly", "race_legis_ass.png"))
barplot(
	tr, 						# donnees a afficher
	legend.text=TRUE,			# afficher la legende des couleurs
	col=topo.colors(4),			# couleurs des barres
	xlim=c(0, 1.5*ncol(tw)),	
	las=2						# labels x verticaux
)
dev.off()

#A l'inverse, la diversite est de plus en plus importante.
#2020 record minimum de "White", moins de la moitie. 
#La race "A" est celle qui gagne le plus de siege : correspond a une augmentation demo ?


#Repartition race par parti sur les mandats totaux: 
partyrace <- table(tab.ass[,"Party"], tab.ass[,"Race"])

png(file.path(plots.folder, "assembly", "race_party_ass.png"))
barplot(
  partyrace,
  legend.text = TRUE, 
  col=topo.colors(4),
  xlim=c(0, 1*ncol(tw))
)
dev.off()

#Donnees en chiffre : 
table(tab.ass[,"Party"], tab.ass[,"Race"])

#Repartition des partis a chaque legislature : 
tp <-table(tab.ass[,"Party"], tab.ass[,"Term"])
View(tp)

png(file.path(plots.folder, "assembly", "party_legis_ass.png"))
barplot(
  tp, 						# donnees a afficher
  legend.text=TRUE,			# afficher la legende des couleurs
  col=topo.colors(3),		# couleurs des barres
  xlim=c(0, 1.5*ncol(tp)),	
  las=2						# labels x verticaux
)
dev.off()

#Cb de mandats sur chaque parti : 
table(tab.ass[,"Party"])
