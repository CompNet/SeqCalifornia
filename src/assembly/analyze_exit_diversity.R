#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Analyse : lien fin de mandat et genre et race
#####
##### JUIN 2022
#####
#######################################################



### Repartition des raisons de fins de mandats femmes: 


table(tab.ass[,"Female"]== TRUE, tab.ass[,"End Type"])
table(tab.ass[,"Female"]== TRUE, tab.ass[,"Exit"])

#Maintenant je descends un peu dans la granularite en regardant les issues par exit : 
#Je recupere d'abord toutes les femmes
women <- which(tab.ass[,"Female"]=="TRUE")
#J'observe ensuite next occupation par next occupation, 
#je lis le resultat "TRUE" qui est le seul a remplir les 2 criteres enonces
table(tab.ass[women,"Exit"]=="Reelection", tab.ass[women,"Next occupation"])
table(tab.ass[women,"Exit"]=="Not seek reelection", tab.ass[women,"Next occupation"])
table(tab.ass[women,"Exit"]=="Termed out", tab.ass[women,"Next occupation"])
table(tab.ass[women,"Exit"]=="Resignation", tab.ass[women,"Next occupation"])
table(tab.ass[women,"Exit"]=="Local campaign", tab.ass[women,"Next occupation"])
table(tab.ass[women,"Exit"]=="Legislative campaign", tab.ass[women,"Next occupation"])



endwo <- table(tab.ass[women,"Exit"])
png(file.path(plots.folder, "assembly", "exit_women_ass.png"))
par(mar=c(11,4,4,4))		# marges plus larges
barplot(endwo,
        col="RED",
        las=2					# labels x verticaux
)
dev.off()


### Repartition des fins de mandats hommes : 
men <- which(tab.ass[,"Female"]== "FALSE")
endmen <- table(tab.ass[men,"Exit"])
png(file.path(plots.folder, "assembly", "exit_men_ass.png"))
par(mar=c(11,4,4,4))			# marges plus larges
barplot(endmen,
        col="RED",
        las=2					# labels x verticaux
)
dev.off()



### Repartition des fins de mandats legis par legis pour femmes hors 2020 :
idx <- which(tab.ass[,"Female"]==TRUE & tab.ass[,"Term"]!= "2020")
endwomen <- table(tab.ass[idx,"Exit"], tab.ass[idx,"Term"])

png(file.path(plots.folder, "assembly", "exit_legis_women_ass.png"))
barplot(endwomen,
        legend.text=TRUE,
        col=topo.colors(6),
        xlim=c(0,2.3*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()

### Repartition des fins de mandats legis par legis pour hommes hors 2020 :
idx <- which(tab.ass[,"Female"]==FALSE & tab.ass[,"Term"]!= "2020")
endmen <- table(tab.ass[idx,"Exit"], tab.ass[idx,"Term"])

png(file.path(plots.folder, "assembly", "exit_legis_men_ass.png"))
barplot(endmen,
        legend.text=TRUE,
        col=topo.colors(12),
        xlim=c(0,2.5*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()


### Repartition des fin de mandats party : 
endparty <- table(tab.ass[,"Exit"],tab.ass[,"Party"])

png(file.path(plots.folder, "assembly", "exit_party_ass.png"))
barplot(endparty,
        legend.text = TRUE,
        col = topo.colors(12),
        xlim=c(0,0.75*ncol(vals)))
dev.off()

#Donnees chiffrees : 
table(tab.ass[,"Exit"], tab.ass[,"Party"])




##########################################
### Repartition des fins de mandats race : 
endrace <- table(tab.ass[,"Exit"], tab.ass[,"Race"])

png(file.path(plots.folder, "assembly", "exit_race_ass.png"))
barplot(endrace,
        legend.text = TRUE,
        col=topo.colors(8),
        xlim=c(0,1.5*ncol(vals)),
        ylim=c(0,171)
)
dev.off()

table(tab.ass[,"Race"], tab.ass[,"Exit"])

#J'aimerai passer ce plot en pourcentage pour plus de lisibilite : 




### Repartition des fins de mandats legis par legis pour H:
idx <- which(tab.ass[,"Race"]=="H" & tab.ass[,"Term"]!= "2020")
endrace1 <- table(tab.ass[idx,"Exit"], tab.ass[idx,"Term"])

png(file.path(plots.folder, "assembly", "exit_legis_H_ass.png"))
barplot(endrace1,
        legend.text=TRUE,
        col=rainbow(6),
        xlim=c(0,2.5*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()

### Repartition des fins de mandats legis par legis pour B:
idx <- which(tab.ass[,"Race"]=="B" & tab.ass[,"Term"]!= "2020")
endrace2 <- table(tab.ass[idx,"Exit"], tab.ass[idx,"Term"])

png(file.path(plots.folder, "assembly", "exit_legis_B_ass.png"))
barplot(endrace2,
        legend.text=TRUE,
        col=topo.colors(7),
        xlim=c(0,3*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()

### Repartition des fins de mandats legis par legis pour A:
idx <- which(tab.ass[,"Race"]=="A" & tab.ass[,"Term"]!= "2020")
endrace3 <- table(tab.ass[idx,"Exit"], tab.ass[idx,"Term"])

png(file.path(plots.folder, "assembly", "exit_legis_A_ass.png"))
barplot(endrace3,
        legend.text=TRUE,
        col=topo.colors(3),
        xlim=c(0,2.5*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()

### Repartition des fins de mandats legis par legis pour W:
idx <- which(tab.ass[,"Race"]=="W" & tab.ass[,"Term"]!= "2020")
endrace4 <- table(tab.ass[idx,"Exit"], tab.ass[idx,"Term"])

png(file.path(plots.folder, "assembly", "exit_legis_W_ass.png"))
barplot(endrace4,
        legend.text=TRUE,
        col=topo.colors(11),
        xlim=c(0,3*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()
