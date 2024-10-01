#######################################################
##### DONNEES CALIFORNIA PARLEMENT COMPLET #####
#####
##### ANALYSE DES EVENEMENTS DES SEQUENCES
#####
##### JANVIER 2022
#######################################################


#D'abord, creer l'objet des sequences d'evenements : 
#Je le fais a partir de l'objet contenant mes sequences d'etat : 
se <- seqecreate(tab.all,
                 id = "Id",
                 timestamp = 
                 tevent = "Office")

#les sous-sequences d'evenements les plus frequentes : 
seqefsub(se,
         pmin.support = 100)
