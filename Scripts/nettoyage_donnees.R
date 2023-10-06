setwd("~/Desktop/Credits_recherche/CR_anoure")
#------------------------------
# données anoures
#------------------------------
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

#------------------------------
# données MH
#------------------------------
MH <-read.csv("donnees/donnees_MH.csv", header = T)

# Enlever colonnes inutiles
MH <- subset(MH, select = c(Enregistre, CLASSE, surface))

unique(sort(MH$CLASSE))

#Enlever les lignes vides
MH <- subset(MH, MH$surface != 0)
MH <- subset(MH, MH$Enregistre != "")
MH <- subset(MH, MH$CLASSE != "")

#Faire la somme des types de MH identiques pour les mêmes sites
MH <- aggregate(surface ~ CLASSE + Enregistre, data = MH, sum)

#Pivoter le tableau de MH
MH_piv <- spread(MH_sites, CLASSE, surface)
MH_piv <- replace(MH_piv, is.na(MH_piv), 0)

#------------------------------
# données routes
#------------------------------
routes <- read.csv("donnees/donnees_routes.csv", header = T)

# Enlever colonnes inutiles
routes <- routes[,-c(2,3,4,5,7,8,9,10,12,13,14,15)]

#------------------------------
# données utilisation du territoire
#------------------------------
uti_terr <-read.csv("donnees/donnees_utilisation_territoire.csv", header = T)

#Enlever colones inutiles
uti_terr <- uti_terr[,-(4:7)]

#------------------------------
# données Codes pour les différentes utilisations du territoire
#------------------------------
codes_uti_terr <- read.csv("donnees/Couleur_utilisation_territoire.txt", header = F, sep = ",")

#Enlever colones inutiles
codes_uti_terr <- codes_uti_terr[,-(2:5)]

#Donner des noms aux colones
colnames(codes_uti_terr)<-c("code", "utilisation")

#------------------------------
# Création tableaux pour analyse
#------------------------------
#Création tableau par année
anoure_2021 <- subset(anoure, anoure$Year == "2021")
anoure_2022 <- subset(anoure, anoure$Year == "2022")

#Ajouter les jours julien
anoure_2021$jour_julien <- julian(as.Date(paste(anoure_2021$Year, anoure_2021$Month, anoure_2021$Day), format = "%Y %m %d"), origin = as.Date("2021-01-01"))
anoure_2022$jour_julien <- julian(as.Date(paste(anoure_2022$Year, anoure_2022$Month, anoure_2022$Day), format = "%Y %m %d"), origin = as.Date("2022-01-01"))


### CRÉÉER TABLEAU LICAT 2021 à 15H ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_21_15h <- subset(anoure_2021, anoure_2021$Time24H == "1500" | anoure_2021$Time24H == "1400", select = c(Site, jour_julien, LICAT))

#Pivoter le tableau de licat en 2021 à 15h
library(tidyr)
licat_21_15h_piv <- spread(licat_21_15h, jour_julien, LICAT)
licat_21_15h_piv <- replace(licat_21_15h_piv, is.na(licat_21_15h_piv), 0)


### CRÉÉER TABLEAU LICAT 2021 à 21H ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_21_21h <- subset(anoure_2021, anoure_2021$Time24H == "2100", select = c(Site, jour_julien, LICAT))

#Pivoter le tableau de licat en 2021 à 15h
library(tidyr)
licat_21_21h_piv <- spread(licat_21_21h, jour_julien, LICAT)
licat_21_21h_piv <- replace(licat_21_21h_piv, is.na(licat_21_21h_piv), 0)


### CRÉÉER TABLEAU LICAT 2021 à 1H00 et minuit ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_21_1h <- subset(anoure_2021, anoure_2021$Time24H == "100" | anoure_2021$Time24H == "0", select = c(Site, jour_julien, LICAT))

#Pivoter le tableau de licat en 2021 à 15h
library(tidyr)
licat_21_1h_piv <- spread(licat_21_1h, jour_julien, LICAT)
licat_21_1h_piv <- replace(licat_21_1h_piv, is.na(licat_21_1h_piv), 0)


### CRÉÉER TABLEAU LICAT 2022 à 15H ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_22_15h <- subset(anoure_2022, anoure_2022$Time24H == "1500" | anoure_2022$Time24H == "1400", select = c(Site, jour_julien, LICAT))

#Pivoter le tableau de licat en 2021 à 15h
library(tidyr)
licat_22_15h_piv <- spread(licat_22_15h, jour_julien, LICAT)
licat_22_15h_piv <- replace(licat_22_15h_piv, is.na(licat_22_15h_piv), 0)


### CRÉÉER TABLEAU LICAT 2022 à 21H ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_22_21h <- subset(anoure_2022, anoure_2022$Time24H == "2100", select = c(Site, jour_julien, LICAT))

#Pivoter le tableau de licat en 2021 à 15h
library(tidyr)
licat_22_21h_piv <- spread(licat_22_21h, jour_julien, LICAT)
licat_22_21h_piv <- replace(licat_22_21h_piv, is.na(licat_22_21h_piv), 0)


### CRÉÉER TABLEAU LICAT 2021 à 1H00 et minuit ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_22_1h <- subset(anoure_2022, anoure_2022$Time24H == "100" | anoure_2022$Time24H == "0", select = c(Site, jour_julien, LICAT))

#Pivoter le tableau de licat en 2021 à 15h
library(tidyr)
licat_22_1h_piv <- spread(licat_22_1h, jour_julien, LICAT)
licat_22_1h_piv <- replace(licat_22_1h_piv, is.na(licat_22_1h_piv), 0)

#------------------------------
# Enregistrement données nettoyées
#------------------------------
write.csv(uti_terr, 'donnees/utilisation_territoire_nett.csv', row.names = FALSE)
write.csv(MH, 'donnees/MH_nett.csv', row.names = FALSE)
write.csv(codes_uti_terr, 'donnees/codes_utilisation_terr_nett.csv', row.names = FALSE)
write.csv(anoure, 'donnees/anoure_nett.csv', row.names = FALSE)
write.csv(routes, 'donnees/routes_nett.csv', row.names = FALSE)

write.csv(routes, 'donnees/licat_22_1h.csv', row.names = FALSE)


