setwd("~/Desktop/Credits_recherche/CR_anoure")
anoure<-read.csv("donnees/donnees_anoures.csv", header = T)

# Voir erreurs dans Recording quality
unique(sort(anoure$RecordingQuality))
# all good

# Corriger erreurs dans DisturbanceType
unique(sort(anoure$DisturbanceType))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("human et rain", "human", x) }))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("human et wind", "human", x) }))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("rain et human", "rain", x) }))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("rain et wind", "rain", x) }))
  
anoure <- data.frame(lapply(anoure, function(x){
  gsub("wind et human", "wind", x) }))
  
anoure <- data.frame(lapply(anoure, function(x){
  gsub("wind et rain", "wind", x) }))

#Changer le nom de colone (enlever le point)
colnames(anoure)[colnames(anoure) == "PSMAC.TRI"] <- "PSMACTRI"

# Nettoyage données MH
MH <-read.csv("donnees/donnees_MH.csv", header = T)

# Enlever colonnes inutiles
MH <- MH[,-c(2,3,4,5,7,8,9,10,12,13,14,15)]

unique(sort(MH$CLASSE))

# Nettoyage données utilisation du territoire
uti_terr <-read.csv("donnees/donnees_utilisation_territoire.csv", header = T)

#Enlever colones inutiles
uti_terr <- uti_terr[,-(4:7)]

#Codes pour les différentes utilisations du territoire
codes_uti_terr <- read.csv("donnees/Couleur_utilisation_territoire.txt", header = F, sep = ",")

#Enlever colones inutiles
codes_uti_terr <- codes_uti_terr[,-(2:5)]

#Donner des noms aux colones
colnames(codes_uti_terr)<-c("code", "utilisation")

# Enregistrement données nettoyées
write.csv(uti_terr, 'donnees/utilisation_territoire_nett.csv', row.names = FALSE)
write.csv(MH, 'donnees/MH_nett.csv', row.names = FALSE)
write.csv(codes_uti_terr, 'donnees/codes_utilisation_terr_nett.csv', row.names = FALSE)
write.csv(anoure, 'donnees/anoure_nett.csv', row.names = FALSE)


