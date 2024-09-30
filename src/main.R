#######################################################
##### DONNEES CALIFORNIA 2000-2020 #####
#####
##### Programme principal
#####
##### FEVRIER 2021
#####
#######################################################




########### GENERAL ###########

# chargement des bibliotheques
library("readr")
library("dplyr")
library("TraMineR")
library("GDAtools")
library("igraph")

# chemin du dossier contenant les donnees et les scripts
#folder <- "D:/Users/Vincent/eclipse/workspaces/Extraction/SeqCalifornia"
#folder <- "C:/Users/Vincent/eclipse/workspaces/Networks/SeqCalifornia"
folder <- "U:/Thèse/AS et BREF/Californie/Version 7"

# autres dossiers
src.folder <- file.path(folder,"src")
data.folder <- file.path(folder,"data")
plots.folder <- file.path(folder,"plots")
dir.create(path=plots.folder, showWarnings=FALSE, recursive=TRUE)

# cree les sous-dossiers des graphiques, s'ils n'existent pas deja
dir.create(path=file.path(plots.folder, "assembly"), showWarnings=FALSE, recursive=TRUE)
dir.create(path=file.path(plots.folder, "senate"), showWarnings=FALSE, recursive=TRUE)
dir.create(path=file.path(plots.folder, "all"), showWarnings=FALSE, recursive=TRUE)
dir.create(path=file.path(plots.folder, "sequences"), showWarnings=FALSE, recursive=TRUE)

# chargement de la dÃ©finition des couleurs
source(file.path(src.folder,"common","colors.R"))
# chargement de la fonction de conversion pour Traminer
source(file.path(src.folder,"common","conversion.R"))
# fonction pour le graphe de transitions
source(file.path(src.folder,"common","transitions.R"))




########### ASSEMBLEE ###########
# chargement et nettoyage des donnees de l'assemblee
source(file.path(src.folder,"assembly","preprocessing_assembly.R"))

# analyse diversite
source(file.path(src.folder,"assembly","analyze_diversity.R"))
# analyse turnover
source(file.path(src.folder,"assembly","analyze_turnover.R"))
# analyse termed out
source(file.path(src.folder,"assembly","analyze_termedout_exit.R"))
# analyse remplacements
source(file.path(src.folder,"assembly","analyze_replacements.R"))
# analyse carriere post-ass
source(file.path(src.folder,"assembly","analyze_exit_diversity.R"))




############# SENAT #############
# chargement et nettoyage des donnees du senat
source(file.path(src.folder,"senate","preprocessing_senate.R"))

# analyse diversite
source(file.path(src.folder,"senate","diversity_senate.R"))
# analyse du turnover
source(file.path(src.folder,"senate","turnover_senate.R"))
# analyse des termed out
source(file.path(src.folder,"senate","termedout_senate.R"))
# analyse des remplacements 
source(file.path(src.folder,"senate","replacements_senate.R"))




############# FUSION ############
# fusion des tables assemblee, senat, et autres
source(file.path(src.folder,"fusion","merge_tables.R"))
# analyse diversite table fusionnee : 
source(file.path(src.folder,"fusion", "diversity_all.R"))




########### SEQUENCES ###########
# analyse de toutes les sequences
filter90 <- NA	# pas de filtrage
source(file.path(src.folder,"fusion", "split_table.R"))
source(file.path(src.folder,"sequences","sequence_analysis.R"))
source(file.path(src.folder,"sequences","description_typologie_3.R"))
source(file.path(src.folder,"sequences","description_typologie_6.R"))

# uniquement les deputes elus pour la 1ere fois avant 1990
filter90 <- "pre90"
source(file.path(src.folder,"fusion", "split_table.R"))
source(file.path(src.folder,"sequences","sequence_analysis.R"))
source(file.path(src.folder,"sequences","description_typologie_3.R"))
source(file.path(src.folder,"sequences","description_typologie_6.R"))

# uniquement les deputes elus pour la 1ere fois apres 1990
filter90 <- "post90"
source(file.path(src.folder,"fusion", "split_table.R"))
source(file.path(src.folder,"sequences","sequence_analysis.R"))
source(file.path(src.folder,"sequences","description_typologie_3.R"))
source(file.path(src.folder,"sequences","description_typologie_6.R"))
