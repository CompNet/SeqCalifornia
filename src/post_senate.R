#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Analyse : carriere post-Senat
#####
##### MAI 2022
#####
#######################################################

# note de VL : la ligne ci-dessous ne peut marcher que si tu completes : tab.sen[,"1st next occ"]="......"
#idx <- which(tab.sen[,"1st next occ"] & tab.sen[,"Reason"]=="Termed out")

sort(unique(tab.sen[,"1st next occ"]))


# Je regarde maintenant qui part pour une autre election immediatement apres termed out et plus tard : 
idx <- which(tab.sen[,"Reason"]== "Termed out"
             & (tab.sen[,"1st next occ"]=="BS" 
                | tab.sen[,"1st next occ"]=="BS candidate" 
                | tab.sen[,"1st next occ"]=="CC" 
                | tab.sen[,"1st next occ"]=="CC candidate" 
                | tab.sen[,"1st next occ"]=="M" 
                | tab.sen[,"1st next occ"]=="M candidate" 
                | tab.sen[,"1st next occ"]=="State Assembly" 
                | tab.sen[,"1st next occ"]=="State Assembly candidate" 
                | tab.sen[,"1st next occ"]=="USR" 
                | tab.sen[,"1st next occ"]=="USR candidate"
                | tab.sen[,"2nd next occ"]=="BS" 
                | tab.sen[,"2nd next occ"]=="BS candidate" 
                | tab.sen[,"2nd next occ"]=="CC" 
                | tab.sen[,"2nd next occ"]=="CC candidate" 
                | tab.sen[,"2nd next occ"]=="M" 
                | tab.sen[,"2nd next occ"]=="M candidate" 
                | tab.sen[,"2nd next occ"]=="State Assembly" 
                | tab.sen[,"2nd next occ"]=="State Assembly candidate" 
                | tab.sen[,"2nd next occ"]=="USR" 
                |tab.sen[,"2nd next occ"]=="USR candidate"
             ))
sort(unique(tab.sen[idx,"Id"]))


# Maintenant idem mais que elus et pas candidats : 
idx <- which(tab.sen[,"Reason"]== "Termed out"
             & (tab.sen[,"1st next occ"]=="BS" 
                | tab.sen[,"1st next occ"]=="CC" 
                | tab.sen[,"1st next occ"]=="M" 
                | tab.sen[,"1st next occ"]=="State Assembly" 
                | tab.sen[,"1st next occ"]=="USR" 
                | tab.sen[,"2nd next occ"]=="BS" 
                | tab.sen[,"2nd next occ"]=="CC" 
                | tab.sen[,"2nd next occ"]=="M" 
                | tab.sen[,"2nd next occ"]=="State State Assembly" 
                | tab.sen[,"2nd next occ"]=="USR" 
             ))
sort(unique(tab.sen[idx,"Id"]))

#Combien vont au USR
idx <- which(tab.sen[,"Reason"]== "Termed out"
             & (tab.sen[,"1st next occ"]=="USR" 
                | tab.sen[,"2nd next occ"]=="USR" 
             ))
sort(unique(tab.sen[idx,"Id"]))
