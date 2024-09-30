#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Analyse : diversite // SENATE
#####
##### JANVIER 2022
#####
#######################################################


############# GENDER #############

# Nb femmes total parmi les elu.es
table(repr.sen[,"Female"]== TRUE)
#Nb de femmes total parmi les mandats
table(tab.sen[,"Female"]== TRUE)
#Repartition des femmes par legislature : 
table(tab.sen[,"Female"]==TRUE, tab.sen[,"Term"])
womenlegis <-table(tab.sen[,"Female"]==TRUE, tab.sen[,"Term"])


png(file.path(plots.folder, "senate", "women_legis_sen.png"))
barplot(womenlegis,
        legend.text = c("Men", "Women"),
        col = topo.colors(2),
        xlim=c(0, 1.8*ncol(tw)))
dev.off()







################ DIVERSITY ####################


#Nb ethnic (a modifier selon analyse en cours) total
table(repr.sen[,"Race"])
table(tab.sen["Race"])

#Repartition ethnies par legislature :
  ethnilegis <- table(tab.sen[,"Race"], tab.sen[,"Term"])
  View(ethnilegis)

  png(file.path(plots.folder, "senate", "ethni_legis_sen.png"))
  barplot(ethnilegis,
          legend.text = c("A","B","H","W"),
          col = topo.colors(4),
          xlim=c(0, 1.7*ncol(tw)))
dev.off()
  
#Nb ethnic par genre parmi les elu.es: 
racegenre <- table(repr.sen[,"Race"], repr.sen[,"Female"])

png(file.path(plots.folder, "senate", "ethni_all_sen.png"))
barplot(
  racegenre,
  legend.text = TRUE, 
  col=topo.colors(4),
  names.arg=c("Male","Female")
)
dev.off()


#Nb ethnic par genre parmi les mandats totaux : 
racegenre <- table(tab.sen[,"Race"], tab.sen[,"Female"])

png(file.path(plots.folder, "senate", "women_ethni_sen.png"))
barplot(
  racegenre,
  legend.text = TRUE, 
  col=topo.colors(4),
  names.arg=c("Male","Female")
)
dev.off()

table(repr.sen[,"Asian"]== TRUE, repr.sen[,"Female"]== TRUE)





########################### PARTIES ################################

#Repartition des partis par legislature : 
table(tab.sen[,"Party"], tab.sen[,"Term"])
partylegis <-table(tab.sen[,"Party"], tab.sen[,"Term"])


png(file.path(plots.folder, "senate", "party_legis_sen.png"))
barplot(partylegis,
        legend.text = c("D", "R"),
        col = topo.colors(2),
        xlim=c(0, 1.8*ncol(tw)))
dev.off()


#Femmes par parti sur les mandats totaux : 
partygenre <- table(tab.sen[,"Party"], tab.sen[,"Female"])

png(file.path(plots.folder, "senate", "women_party_all_sen.png"))
barplot(
  partygenre,
  legend.text = TRUE, 
  col=topo.colors(4),
  names.arg=c("Male","Female")
)
dev.off()

#Repartition race par party sur les mandats totaux: 
partyrace <- table(tab.sen[,"Party"], tab.sen[,"Race"])

png(file.path(plots.folder, "senate", "race_party_all_sen.png"))
barplot(
  partyrace,
  legend.text = TRUE, 
  col=topo.colors(4),
  xlim=c(0, 1*ncol(tw))
)
dev.off()

#Donnees en chiffre : 
table(tab.sen[,"Party"], tab.sen[,"Race"])

