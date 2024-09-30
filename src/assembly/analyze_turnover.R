#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Analyse : turnover
#####
##### FEVRIER 2021
#####
#######################################################

### Combien y a-t-il de nouveaux entrants / anciens membres a chaque legis ?

# initialisation des dates utilisees ensuite
term.starts <- c('12/05/1980', '12/06/1982', '12/05/1984', '12/05/1986', '12/05/1988', '12/03/1990','12/07/1992', '12/05/1994', '12/02/1996', '12/07/1998', '12/04/2000', '12/02/2002', '12/06/2004', '12/04/2006', '12/01/2008', '12/06/2010', '12/03/2012', '12/01/2014', '12/05/2016', '12/03/2018', '12/07/2020')
term.starts <- as.Date(term.starts, format="%m/%d/%Y")
# initialisation d'une matrice destinee a recevoir les decomptes de nouveaux/anciens membres
vals <- matrix(0,nrow=length(years)-1, ncol=2)
colnames(vals) <- c("Old","New")
rownames(vals) <- years[2:length(years)]

# on boucle sur chaque legislature
for(i in 2:length(years))
{	cat("Resultat pour la legislature ",years[i]," (date de debut : ",format(term.starts[i]),") \n",sep="")
  
  # id des deputes de la legislature precedente
  idx.previous <- tab.ass[tab.ass[,"Term"]==years[i-1], "Id"]
  # id des deputes de la legislature courante
  idx.current <- tab.ass[tab.ass[,"Term"]==years[i], "Id"]
  
  # la on ne garde que ceux presents dans les deux
  idx.old <- intersect(idx.previous, idx.current)
  # et la que ceux presents dans le 2eme mais pas le 1er
  idx.new <- setdiff(idx.current, idx.previous)
  
  # on met a jour les decomptes
  vals[i-1, "Old"] <- length(idx.old)
  vals[i-1, "New"] <- length(idx.new)
 
  # petit message
  cat("   New MPs: ",length(idx.new),"/",length(idx.current),"\n",sep="")
}

# on represente les decomptes sous forme de graphique en barres
vals <- t(vals)

png(file.path(plots.folder, "assembly", "new_old_ass.png"))
barplot(
	vals, 					# decomptes a afficher
	legend.text=c("Old","New"),
	col=topo.colors(2), 
	xlim=c(0, 1.5*ncol(vals)),
	las=2					# labels x verticaux
)
dev.off()

#Etant donne que j'ai harmonise les dates d'election, je peux me focaliser sur les elu.es lors de l'election principale (hors elections speciales)
#8 nouveaux elu.es en 2018
#Ici, on voit les effets du changement de 2012, autrement dit en 2016 les derniers a avoir ete termed-out sous l'ancienne proposition l'ont ete, donc fin 2016 derniers termed out.
#Election 2018 il n'y a plus de termed out a remplacer. Les derniers etaient en 2016, les prochains seront en 2024. 






### Qui sont ces personnes nouvellement elues ?

# on fait a peu pres la meme boucle qu'avant, mais au lieu de compter on affiche les noms
for(i in 2:length(years))
{	cat("Resultat pour la legislature ",years[i]," (date de debut : ",format(term.starts[i]),") \n",sep="")
  
  # id des deputes de la legislature precedente
  idx.previous <- tab.ass[tab.ass[,"Term"]==years[i-1], "Id"]
  # id des deputes de la legislature courante
  idx.current <- tab.ass[tab.ass[,"Term"]==years[i], "Id"]
  
  # la on ne garde que ceux presents dans les deux
  idx.old <- intersect(idx.previous, idx.current)
  # et la que ceux presents dans le 2eme mais pas le 1er
  idx.new <- setdiff(idx.current, idx.previous)
  
  # on recupere et affiche les noms des nouveaux
  names <- repr.ass[match(idx.new, repr.ass[,"Id"]), "Name"]
  print(names)
}






### Combien y a-t-il de nouveaux entrants / membres de la legis precedente / membres elus avant la legis precedente mais pas a la legis precedente ?

# initialisation d'une matrice destinee a recevoir les decomptes de membres
vals <- matrix(0,nrow=length(years)-1, ncol=3)
colnames(vals) <- c("Old", "Newish", "New")
rownames(vals) <- years[2:length(years)]
# initialisation d'une liste de tous les anciens elus
idx.hist <- tab.ass[tab.ass[,"Term"]==years[1], "Id"]

# on boucle sur chaque legislature
for(i in 2:length(years))
{	cat("Resultat pour la legislature ",years[i]," (date de debut : ",format(term.starts[i]),") \n",sep="")
  
  # id des deputes de la legislature precedente
  idx.previous <- tab.ass[tab.ass[,"Term"]==years[i-1], "Id"]
  # id des deputes de la legislature courante
  idx.current <- tab.ass[tab.ass[,"Term"]==years[i], "Id"]
  
  # on met a jour la liste d'elus anciens
  idx.hist <- union(idx.hist, idx.previous)

  # la on ne garde que ceux presents dans les deux legis
  idx.old <- intersect(idx.previous, idx.current)
  # et la ceux qui sont uniquement dans la 2eme
  idx.new0 <- setdiff(idx.current, idx.previous)
  # parmi eux, on distingue ceux jamais apparu auparavant
  idx.new <- setdiff(idx.new0, idx.hist)
  # et ceux deja elu avant la legis precedente
  idx.newish <- setdiff(idx.new0, idx.new)
  
  
  # on met a jour les decomptes
  vals[i-1, "Old"] <- length(idx.old)
  vals[i-1, "Newish"] <- length(idx.newish)
  vals[i-1, "New"] <- length(idx.new)
 
  # petit message
  cat("   Newish MPs: ",length(idx.newish),"/",length(idx.current),"\n",sep="")
  cat("   New MPs: ",length(idx.new),"/",length(idx.current),"\n",sep="")
  cat("   Old: ",length(idx.old),"/",length(idx.current),"\n",sep="")
}

# on represente les decomptes sous forme de graphique a barres
vals <- t(vals)
View(vals)

png(file.path(plots.folder, "assembly", "newish_old_ass.png"))
barplot(
	vals, 					# decomptes a afficher
	legend.text=c("Old","Newish","New"),
	col=topo.colors(3),
	xlim=c(0, 1.5*ncol(vals)),
	las=2					# labels x verticaux
)
dev.off()


#Combien sont reelu-es chaque term ? 
table(tab.ass[,"Term"], tab.ass[,"Next occupation"])
