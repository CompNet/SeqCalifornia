#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Analyse : lien termed out, fin de mandats et nouveaux elu-es
#####
##### FEVRIER 2021 - JUIN 2022
#####
#######################################################

### Repartition de toutes les fins de mandats sur l'ensemble des donnees

#D'abord decompte des departs anticipes et des departs en fin de duree legale du mandat : 
table(tab.ass[,"End Type"])
#On compte 21 departs anticipes sur toute la periode, contre 790 departs en fin legale de mandat
#Nb : les elu-es de 2020 ne sont pas pris en compte, d'ou seulement 811 departs. 

table(tab.ass[,"End Type"], tab.ass[,"Female"])
#Les hommes anticipent plus leur depart

table(tab.ass[,"End Type"],tab.ass[,"Term"])

table(tab.ass[,"End Type"], tab.ass[,"Race"])



#Maintenant, quelles sont les suites a la fin d'un mandat (Exit) :
idx <- which(tab.ass[,"End Type"]=="On time")
vals <- table(tab.ass[idx,"Exit"], tab.ass[idx,"Term"])

png(file.path(plots.folder, "assembly", "reason_ontime_legis_ass.png"))
barplot(vals,
        legend.text=TRUE,
        col=topo.colors(4),
        xlim=c(0, 2*ncol(vals))
)
dev.off()


table(tab.ass[,"Exit"], tab.ass[,"Female"])

idx <- which(tab.ass[,"End Type"]=="Early")
finmand <- table(tab.ass[idx,"Exit"])
finmand <- finmand/sum(finmand)*100

png(file.path(plots.folder, "assembly", "frequence_early_exit_assembly.png"))
par(mar=c(10,10,10,4))			# marges plus larges
barplot(
	finmand,
	#legend.text=TRUE,
	col=topo.colors(4),
	main=TRUE,
	#xlim=c(0, 2.5*ncol(tw)),
	las=2						# labels x verticaux
)
dev.off()

idx <- which(tab.ass[,"End Type"]=="On time")
finmand <- table(tab.ass[idx,"Exit"])
finmand <- finmand/sum(finmand)*100

png(file.path(plots.folder, "assembly", "frequence_ontime_exit_assembly.png"))
par(mar=c(10,10,10,4))			# marges plus larges
barplot(
  finmand,
  #legend.text=TRUE,
  col=topo.colors(4),
  main=TRUE,
  #xlim=c(0, 2.5*ncol(tw)),
  las=2						# labels x verticaux
)
dev.off()




###############################################
###### ETUDE SUR LES ISSUES ET OCCUPATION #####
###############################################

#Parmi les personnes qui choisissent de ne pas se representer, que font-elles apres ?
idx <- which(tab.ass[,"Exit"]=="Not seek reelection")
table(tab.ass[idx,"Next occupation"])

#Parmi les personnes qui choisissent de se representer, que font-elles apres ?
idx <- which(tab.ass[,"Exit"]=="Reelection")
table(tab.ass[idx,"Next occupation"])

#Parmi les personnes termed out, que font-elles apres ?
idx <- which(tab.ass[,"Exit"]=="Termed out")
table(tab.ass[idx,"Next occupation"])

#Memes questions du cote des departs anticipes : 
idx <- which(tab.ass[,"Exit"]=="Resignation")
table(tab.ass[idx,"Next occupation"])

idx <- which(tab.ass[,"Exit"]=="Local campaign")
table(tab.ass[idx,"Next occupation"])

idx <- which(tab.ass[,"Exit"]=="Legislative campaign")
table(tab.ass[idx,"Next occupation"])




###########################################
#### REPARTITION PAR TERM DES EXIT
##########################################


### Combien de termed-out sur toute la periode ?
to <- which(tab.ass[,"Exit"]== "Termed out")
cat("Nombre de termed-outs sur tout la periode :",length(to),"\n")
summary(tab.ass[,"Exit"])





### Repartition des termed-out a chaque fin de legis
vals <- table(tab.ass[,"Exit"], tab.ass[,"Term"], useNA="ifany")
vals <- vals["Termed out",]

png(file.path(plots.folder, "assembly", "termedout_assembly_legis.png"))
par(mar=c(5.1, 4.1, 4.1, 2.1))		# marges normales
barplot(
	vals, 							# decomptes a afficher
	col="RED",
	las=2							# labels x verticaux
)
dev.off()



###########################################
##### lien entre new members et type de fin
###########################################


#A adapter selon la exit a observer
table(tab.ass[,"Exit"]== "Termed out", tab.ass[,"Term"])
table(tab.ass[,"Next occupation"]== "Defeated", tab.ass[,"Term"])
table(tab.ass[,"Next occupation"]== "State Senate candidate", tab.ass[,"Term"])
table(tab.ass[,"Next occupation"]== "USR candidate", tab.ass[,"Term"])

### Repartition des motifs de fin de mandat a chaque fin de legis
vals <- table(tab.ass[,"Next occupation"], tab.ass[,"Term"])

png(file.path(plots.folder, "assembly","nextocc_legis_ass.png"))
barplot(vals,
	legend.text=TRUE,
	col=topo.colors(ncol(vals)),
	xlim=c(0, 2.3*ncol(vals))
)
dev.off()
