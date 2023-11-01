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

#Corriger les noms de sites
anoure <- data.frame(lapply(anoure, function(x){
  gsub("X-lisiere", "X-foret_X-lisiere", x) }))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("X-foret", "X-foret_X-lisiere", x) }))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("X-foret_X-lisiere_X-lisiere", "X-foret_X-lisiere", x) }))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("O-lisiere", "L-foret_O-lisiere", x) }))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("L-foret", "L-foret_O-lisiere", x) }))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("L-foret_O-lisiere_O-lisiere", "L-foret_O-lisiere", x) }))

#------------------------------
# données MH
#------------------------------
MH <-read.csv("donnees/donnees_MH.csv", header = T)

# Enlever colonnes inutiles
MH <- subset(MH, select = c(Enregistre, CLASSE, surface))

#Enlever les lignes vides
MH <- subset(MH, MH$surface != 0)
MH <- subset(MH, MH$Enregistre != "")
#MH <- subset(MH, MH$CLASSE != "")

#Faire la somme des types de MH identiques pour les mêmes sites
MH <- aggregate(surface ~ CLASSE + Enregistre, data = MH, sum)

#Pivoter le tableau de MH
MH <- spread(MH, CLASSE, surface)
MH <- replace(MH, is.na(MH), 0)

#Enlever la colonne V1 (voir routes)
MH <- subset(MH, select = -V1)

#Mettre les tourbières ensemble
MH$Tourbiere <- MH$`Tourbière boisée indifférenciée`+MH$`Tourbière boisée minérotrophe`+MH$`Tourbière ouverte minérotrophe`

#------------------------------
# données routes
#------------------------------
routes <- read.csv("donnees/donnees_routes.csv", header = T)

# Enlever colonnes inutiles
routes <- subset(routes, select = c(ClsRte, Enregistre, longueur))

#Enlever les lignes vides
routes <- subset(routes, routes$Enregistre != "")
#routes <- subset(routes, routes$ClsRte != "")

#Faire la somme des types de routes identiques pour les mêmes sites
routes <- aggregate(longueur ~ ClsRte + Enregistre, data = routes, sum)

#Pivoter le tableau
routes <- spread(routes, ClsRte, longueur)
routes <- replace(routes, is.na(routes), 0)

# Enlever colonnes V1 qui se crée de je sais pas où..? C'est quand je fais pas la ligne qui enleve clsrte vide
routes <- subset(routes, select = -V1)

#Total du nombre de km de routes
routes$Total <- routes$`Collectrice municipale`+routes$`Locale`+routes$`Nationale`+routes$`Régionale`

#------------------------------
# données Codes pour les différentes utilisations du territoire
#------------------------------
codes_uti_terr <- read.csv("donnees/Couleur_utilisation_territoire.txt", header = F, sep = ",")

#Enlever colones inutiles
codes_uti_terr <- codes_uti_terr[,-(2:5)]

#Donner des noms aux colones
colnames(codes_uti_terr)<-c("code", "utilisation")

#------------------------------
# données utilisation du territoire
#------------------------------
uti_terr <-read.csv("donnees/donnees_utilisation_territoire.csv", header = T)

#Enlever colones inutiles
uti_terr <- subset(uti_terr, select = c(DN, Enregistre, surface))

#Enlever les lignes vides
#uti_terr <- subset(uti_terr, uti_terr$DN != "NA")

#Faire la somme des types de .utilisation de territoire identiques pour les mêmes sites
uti_terr <- aggregate(surface ~ DN + Enregistre, data = uti_terr, sum)

#Lier le tableau de code et le tableau utilisation territoire
uti_terr <- merge(uti_terr, codes_uti_terr, by.x = "DN", by.y = "code")

#Enlever colones inutiles
uti_terr <- subset(uti_terr, select = c(Enregistre, surface, utilisation))

#Pivoter le tableau
uti_terr<- spread(uti_terr, utilisation, surface)
uti_terr <- replace(uti_terr, is.na(uti_terr), 0)

#Enlever codes territoire parce que pus besoin
rm(codes_uti_terr)

#------------------------------
# données jours julien
#------------------------------
jj <-read.csv("donnees/JJ_21_21h.csv", header = T)

#------------------------------
# Création tableaux pour analyse
#------------------------------
library(tidyr)

#Création tableau par année
anoure_2021 <- subset(anoure, anoure$Year == "2021")
anoure_2022 <- subset(anoure, anoure$Year == "2022")

#Ajouter les jours julien
anoure_2021$jour_julien <- julian(as.Date(paste(anoure_2021$Year, anoure_2021$Month, anoure_2021$Day), format = "%Y %m %d"), origin = as.Date("2021-01-01"))
anoure_2022$jour_julien <- julian(as.Date(paste(anoure_2022$Year, anoure_2022$Month, anoure_2022$Day), format = "%Y %m %d"), origin = as.Date("2022-01-01"))

# une seule observation à 15h
#Seulement 3 pour 21h donc garder le 1h seulement
### CRÉÉER TABLEAU LICAT 2021 à 21H ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_21_21h <- subset(anoure_2021, anoure_2021$Time24H == "2100", select = c(Site, jour_julien, LICAT))

#Pivoter le tableau de licat en 2021 à 21h
licat_21_21h_piv <- spread(licat_21_21h, jour_julien, LICAT, fill = NA, convert = TRUE)


colnames(licat_21_21h_piv)<- c("Site","V144","V145","V150","V151","V153","V154","V158","V160","V165","V172","V179","V180","V186","V192","V193","V1")
library(dplyr)
install.packages("hablar")
library(hablar)
licat_21_21h_piv %>% rowwise() %>% mutate(V1 = sum_("V144",V145,V150,na.rm=TRUE))
licat_21_21h_piv$V1<-licat_21_21h_piv$V144+licat_21_21h_piv$V145+licat_21_21h_piv$V150
licat_21_21h_piv$V2<- licat_21_21h_piv$V150+licat_21_21h_piv$V151+licat_21_21h_piv$V153+licat_21_21h_piv$V154
licat_21_21h_piv$V3<- licat_21_21h_piv$V158
licat_21_21h_piv$V4<- licat_21_21h_piv$V160+licat_21_21h_piv$V165
licat_21_21h_piv$V5<- licat_21_21h_piv$V172
licat_21_21h_piv$V6<- licat_21_21h_piv$V179+licat_21_21h_piv$V180
licat_21_21h_piv$V7<- licat_21_21h_piv$V186
licat_21_21h_piv$V8<- licat_21_21h_piv$V192+licat_21_21h_piv$V193 



names<-c("V1","V2","V3","V4","V5","V6","V7","V8")
for (j in 1:length(names)) {
  for (i in 1:(ncol(licat_21_15h_piv))-1) {
    if (colnames(licat_21_21h_piv[i]==names[j])){
      
    }

    }
}




for (i in 1:26) {
  if( licat_21_21h_piv[i,2]=NA
  )
  ifelse( licat_21_21h_piv[i,3]=NA
  )
  ifelse( licat_21_21h_piv[i,4]=NA
  )
  else( licat_21_21h_piv[i,18]<-NA
  )
}

licat_21_21h_piv$V1<- licat_21_21h_piv$V144 +licat_21_21h_piv$V145+licat_21_21h_piv$V147

##Joindre visites


### CRÉÉER TABLEAU LICAT 2021 à 1H00 et minuit ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_21_1h <- subset(anoure_2021, anoure_2021$Time24H == "100" | anoure_2021$Time24H == "0", select = c(Site, jour_julien, LICAT))

#Pivoter le tableau de licat en 2021 à 1h
licat_21_1h <- spread(licat_21_1h, jour_julien, LICAT)


#une seule observation à 15h en 2022
#Seulement 4 obervations en 2022 à 21h
### CRÉÉER TABLEAU LICAT 2022 à 21H ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
#licat_22_21h <- subset(anoure_2022, anoure_2022$Time24H == "2100", select = c(Site, jour_julien, LICAT))

#Pivoter le tableau de licat en 2022 à 21h
#licat_22_21h <- spread(licat_22_21h, jour_julien, LICAT)

### CRÉÉER TABLEAU LICAT 2022 à 1H00 et minuit ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_22_1h <- subset(anoure_2022, anoure_2022$Time24H == "100" | anoure_2022$Time24H == "0", select = c(Site, jour_julien, LICAT))

#Pivoter le tableau de licat en 2021 à 1h
licat_22_1h <- spread(licat_22_1h, jour_julien, LICAT)

### CRÉÉER TABLEAU LSYL 2022 à 21H00 ###
#Il y en a quelques unes aussi à 1h mais vraiment plus à 21h
lisyl_22_21h <- subset(anoure_2022, anoure_2022$Time24H == "2100", select = c(Site, jour_julien, LISYL))
lisyl_22_21h <- spread(lisyl_22_21h, jour_julien, LISYL)

#------------------------------
# Tableau Détection
#------------------------------
qualite_2021_15 <- subset(anoure_2021, anoure_2021$Time24H == "1500", select = c(Site, jour_julien, RecordingQuality))
qualite_2021_15 <- spread(qualite_2021_15, jour_julien, RecordingQuality)
qualite_2021_15 <- replace(qualite_2021_15, is.na(qualite_2021_15), 0)

qualite_2021_21 <- subset(anoure_2021, anoure_2021$Time24H == "2100", select = c(Site, jour_julien, RecordingQuality))
qualite_2021_21 <- spread(qualite_2021_21, jour_julien, RecordingQuality)
qualite_2021_21 <- replace(qualite_2021_21, is.na(qualite_2021_21), 0)

qualite_2021_1 <- subset(anoure_2021, anoure_2021$Time24H == "100" | anoure_2021$Time24H == "0", select = c(Site, jour_julien, RecordingQuality))
qualite_2021_1 <- spread(qualite_2021_1, jour_julien, RecordingQuality)
qualite_2021_1 <- replace(qualite_2021_1, is.na(qualite_2021_1), 0)

##2022##
qualite_2022_15 <- subset(anoure_2022, anoure_2022$Time24H == "1500", select = c(Site, jour_julien, RecordingQuality))
qualite_2022_15 <- spread(qualite_2022_15, jour_julien, RecordingQuality)
qualite_2022_15 <- replace(qualite_2022_15, is.na(qualite_2022_15), 0)

qualite_2022_21 <- subset(anoure_2022, anoure_2022$Time24H == "2100", select = c(Site, jour_julien, RecordingQuality))
qualite_2022_21 <- spread(qualite_2022_21, jour_julien, RecordingQuality)
qualite_2022_21 <- replace(qualite_2022_21, is.na(qualite_2022_21), 0)

qualite_2022_1 <- subset(anoure_2022, anoure_2022$Time24H == "100" | anoure_2022$Time24H == "0", select = c(Site, jour_julien, RecordingQuality))
qualite_2022_1 <- spread(qualite_2022_1, jour_julien, RecordingQuality)
qualite_2022_1 <- replace(qualite_2022_1, is.na(qualite_2022_1), 0)

#Type de disturbance
disturb_2021_15 <- subset(anoure_2021, anoure_2021$Time24H == "1500", select = c(Site, jour_julien, DisturbanceType))
disturb_2021_15 <- spread(disturb_2021_15, jour_julien, DisturbanceType)
disturb_2021_15 <- replace(disturb_2021_15, is.na(disturb_2021_15), 0)

disturb_2021_21 <- subset(anoure_2021, anoure_2021$Time24H == "2100", select = c(Site, jour_julien, DisturbanceType))
disturb_2021_21 <- spread(disturb_2021_21, jour_julien, DisturbanceType)
disturb_2021_21 <- replace(disturb_2021_21, is.na(disturb_2021_21), 0)

disturb_2021_1 <- subset(anoure_2021, anoure_2021$Time24H == "100" | anoure_2021$Time24H == "0", select = c(Site, jour_julien, DisturbanceType))
disturb_2021_1 <- spread(disturb_2021_1, jour_julien, DisturbanceType)
disturb_2021_1 <- replace(disturb_2021_1, is.na(disturb_2021_1), 0)

#2022

disturb_2022_15 <- subset(anoure_2022, anoure_2022$Time24H == "1500", select = c(Site, jour_julien, DisturbanceType))
disturb_2022_15 <- spread(disturb_2022_15, jour_julien, DisturbanceType)
disturb_2022_15 <- replace(disturb_2022_15, is.na(disturb_2022_15), 0)

disturb_2022_21 <- subset(anoure_2022, anoure_2022$Time24H == "2100", select = c(Site, jour_julien, DisturbanceType))
disturb_2022_21 <- spread(disturb_2022_21, jour_julien, DisturbanceType)
disturb_2022_21 <- replace(disturb_2022_21, is.na(disturb_2022_21), 0)

disturb_2022_1 <- subset(anoure_2022, anoure_2022$Time24H == "100" | anoure_2022$Time24H == "0", select = c(Site, jour_julien, DisturbanceType))
disturb_2022_1 <- spread(disturb_2022_1, jour_julien, DisturbanceType)
disturb_2022_1 <- replace(disturb_2022_1, is.na(disturb_2022_1), 0)

#------------------------------
# Enregistrement données nettoyées
#------------------------------
write.csv(uti_terr_piv, 'donnees/utilisation_territoire_nett.csv', row.names = FALSE)
write.csv(MH_piv, 'donnees/MH_nett.csv', row.names = FALSE)
write.csv(anoure, 'donnees/anoure_nett.csv', row.names = FALSE)
write.csv(routes_piv, 'donnees/routes_nett.csv', row.names = FALSE)

write.csv(licat_21_15h_piv, 'donnees/licat_21_15h.csv', row.names = FALSE)
write.csv(licat_21_21h_piv, 'donnees/licat_21_21h.csv', row.names = FALSE)
write.csv(licat_21_1h_piv, 'donnees/licat_21_1h.csv', row.names = FALSE)
write.csv(licat_22_15h_piv, 'donnees/licat_22_15h.csv', row.names = FALSE)
write.csv(licat_22_21h_piv, 'donnees/licat_22_21h.csv', row.names = FALSE)
write.csv(licat_22_1h_piv, 'donnees/licat_22_1h.csv', row.names = FALSE)

write.csv(qualite_2021_15, 'donnees/qualite_2021_15h.csv', row.names = FALSE)
write.csv(qualite_2021_21, 'donnees/qualite_2021_21h.csv', row.names = FALSE)
write.csv(qualite_2021_1, 'donnees/qualite_2021_1h.csv', row.names = FALSE)
write.csv(qualite_2022_15, 'donnees/qualite_2022_15h.csv', row.names = FALSE)
write.csv(qualite_2022_21, 'donnees/qualite_2022_21h.csv', row.names = FALSE)
write.csv(qualite_2022_1, 'donnees/qualite_2022_1h.csv', row.names = FALSE)

write.csv(disturb_2021_15, 'donnees/disturb_2021_15h.csv', row.names = FALSE)
write.csv(disturb_2021_21, 'donnees/disturb_2021_21h.csv', row.names = FALSE)
write.csv(disturb_2021_1, 'donnees/disturb_2021_1h.csv', row.names = FALSE)
write.csv(disturb_2022_15, 'donnees/disturb_2022_15h.csv', row.names = FALSE)
write.csv(disturb_2022_21, 'donnees/disturb_2022_21h.csv', row.names = FALSE)
write.csv(disturb_2022_1, 'donnees/disturb_2022_1h.csv', row.names = FALSE)
