#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Analyse : diversite table fusionnee SENAT + ASSEMBLEE
#####
##### JANVIER 2022
#####
#######################################################

###### Combien ai-je d'elu.es au total sur les mandats attribues sur la periode ?

#Je rappelle que j'ai 346 noms uniques sur toute la periode. 
table(tab.all[,"Id"])

# Nb femmes total
femme <- table(repr.all[,"Female"]== TRUE)
total <- nrow(repr.all)
pourcentage <- (femme/total)*100
pourcentage

#Nb ethnic (a modifier selon analyse en cours) total
race <- table(repr.all[,"Race"])
pourcentage <- (race/total)*100
pourcentage

#Nb ethnic par genre: 
racegenre <- table(repr.all[,"Race"], repr.all[,"Female"])
print(racegenre)

png(file.path(plots.folder, "all", "gender_race_fusion.png"))
barplot(
	racegenre,
	legend.text = TRUE, 
	col=topo.colors(4),
	names.arg=c("Male","Female")
)
dev.off()


