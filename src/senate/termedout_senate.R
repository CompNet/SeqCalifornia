#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Analyse : termed out - SENATE
#####
##### JANVIER 2022
#####
#######################################################

### Repartition de toutes les fins de mandats sur l'ensemble des donnees
table (tab.sen[,"Exit"])
table(tab.sen[,"Exit"], tab.sen[,"Female"])

val <- tab.sen[tab.sen[,"Exit"]!="Reelection","Exit"]
finmand <- table(val)
finmand <- finmand/sum(finmand)*100

png(file.path(plots.folder, "senate", "Exit_senate.png"))
par(mar=c(11,4,4,4))			# marges plus larval
barplot(
  finmand,
  #legend.text=TRUE,
  col=topo.colors(11),
  main=TRUE,
  #xlim=c(0, 2.5*ncol(tw)),
  las=2						# labels x verticaux
)
dev.off()


View(finmand)

### Combien de termed-out sur toute la periode ?
to <- which(tab.sen[,"Exit"]== "Termed out")
cat("Nombre de termed-outs sur tout la periode :",length(to),"\n")
summary(tab.sen[,"Exit"])





### Repartition des termed-out a chaque fin de legis
idx <- which(tab.sen[,"Term"]!="1994" & tab.sen[,"Term"]!="1996" & tab.sen[,"Term"]!="1998")
vals <- table(tab.sen[idx,"Exit"], tab.sen[idx,"Term"], useNA="ifany")
vals <- vals["Termed out",]

png(file.path(plots.folder, "senate", "termedout_legis_sen.png"))
par(mar=c(5.1, 4.1, 4.1, 2.1))		# marges normales
barplot(
  vals, 							# decomptes a afficher
  col="RED",
  las=2							# labels x verticaux
)
dev.off()

#A adapter selon la Exit a observer
table(tab.sen[,"Exit"]== "Termed out", tab.sen[,"Term"])



### Repartition des motifs de fin de mandat a chaque fin de legis
vals <- table(tab.sen[,"Exit"], tab.sen[,"Term"])

png(file.path(plots.folder, "senate", "reaso_legis_sen.png"))
barplot(vals,
        legend.text=TRUE,
        col=topo.colors(ncol(vals)),
        xlim=c(0, 2.3*ncol(vals))
)
dev.off()



### Repartition des raisons de fins de mandats femmes: 
women <- which(tab.sen[,"Female"]=="TRUE")
endwo <- table(tab.sen[women,"Exit"])

png(file.path(plots.folder, "senate", "Exit_senate_wo.png"))
par(mar=c(11,4,4,4))			# marges plus larges
barplot(endwo,
        col="RED",
        las=2					# labels x verticaux
)
dev.off()

table(tab.sen[,"Female"]== TRUE, tab.sen[,"Exit"])

### Repartition des fins de mandats hommes : 
men <- which(tab.sen[,"Female"]== "FALSE")
endmen <- table(tab.sen[men,"Exit"])

png(file.path(plots.folder, "senate", "Exit_senate_men.png"))
par(mar=c(11,4,4,4))			# marges plus larges
barplot(endmen,
        col="RED",
        las=2					# labels x verticaux
)
dev.off()

### Repartition des fins de mandats legis par legis pour femmes hors 1996 :
idx <- which(tab.sen[,"Female"]==TRUE & tab.sen[,"Term"]!="1996")
endwomen <- table(tab.sen[idx,"Exit"], tab.sen[idx,"Term"])

png(file.path(plots.folder, "senate", "Exit_women_legis_sen.png"))
barplot(endwomen,
        legend.text=TRUE,
        col=topo.colors(8),
        xlim=c(0,2.3*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()

### Repartition des fins de mandats legis par legis pour hommes hors 1996 :
idx <- which(tab.sen[,"Female"]==FALSE & tab.sen[,"Term"]!= "1996")
endmen <- table(tab.sen[idx,"Exit"], tab.sen[idx,"Term"])
png(file.path(plots.folder, "senate", "_Exit_men_legis_sen.png"))
barplot(endmen,
        legend.text=TRUE,
        col=topo.colors(11),
        xlim=c(0,2.5*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()


### Repartition des fins de mandats race : 
endrace <- table(tab.sen[,"Exit"], tab.sen[,"Race"])

png(file.path(plots.folder, "senate", "Exit_race_all_sen.png"))
barplot(endrace,
        legend.text = TRUE,
        col=topo.colors(14),
        xlim=c(0,0.75*ncol(vals)),
        ylim=c(0,151)
)
dev.off()

table(tab.sen[,"Race"], tab.sen[,"Exit"])

#J'aimerai passer ce plot en pourcentage pour plus de lisibilite : 


#Donnees chiffrees : 
table(tab.sen[,"Exit"], tab.sen[,"Race"])




### Repartition des fins de mandats legis par legis pour H:
idx <- which(tab.sen[,"Race"]=="H" & tab.sen[,"Term"]!= "2020")
endrace1 <- table(tab.sen[idx,"Exit"], tab.sen[idx,"Term"])

png(file.path(plots.folder, "senate", "Exit_H_legis_sen.png"))
barplot(endrace1,
        legend.text=TRUE,
        col=rainbow(8),
        xlim=c(0,2.5*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()

### Repartition des fins de mandats legis par legis pour B:
idx <- which(tab.sen[,"Race"]=="B" & tab.sen[,"Term"]!= "2020")
endrace2 <- table(tab.sen[idx,"Exit"], tab.sen[idx,"Term"])

png(file.path(plots.folder, "senate", "Exit_B_legis_sen.png"))
barplot(endrace2,
        legend.text=TRUE,
        col=topo.colors(5),
        xlim=c(0,3*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()

### Repartition des fins de mandats legis par legis pour A:
idx <- which(tab.sen[,"Race"]=="A" & tab.sen[,"Term"]!= "2020")
endrace3 <- table(tab.sen[idx,"Exit"], tab.sen[idx,"Term"])

png(file.path(plots.folder, "senate", "Exit_A_legis_sen.png"))
barplot(endrace3,
        legend.text=TRUE,
        col=topo.colors(5),
        xlim=c(0,2.5*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()

### Repartition des fins de mandats legis par legis pour W:
idx <- which(tab.sen[,"Race"]=="W" & tab.sen[,"Term"]!= "2020")
endrace4 <- table(tab.sen[idx,"Exit"], tab.sen[idx,"Term"])

png(file.path(plots.folder, "senate", "Exit_W_legis_sen.png"))
barplot(endrace4,
        legend.text=TRUE,
        col=topo.colors(11),
        xlim=c(0,3*ncol(vals)),
        las=2					# labels x verticaux
)
dev.off()

### Repartition des fin de mandats party : 
endparty <- table(tab.sen[,"Exit"],tab.sen[,"Party"])

png(file.path(plots.folder, "senate", "Exit_party_sen.png"))
barplot(endparty,
        legend.text = TRUE,
        col = topo.colors(14),
        xlim=c(0,0.60*ncol(vals)))
dev.off()
        
#Donnees chiffrees : 
table(tab.sen[,"Exit"], tab.sen[,"Party"])
