setwd("~/Desktop/Credits_recherche/CR_anoure")

library(tidyr)

#------------------------------
# données anoures
#------------------------------
anoure<-read.csv("donnees/donnees_anoures.csv", header = T)

# Corriger erreurs dans DisturbanceType
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

anoure <- data.frame(lapply(anoure, function(x){
  gsub("rain", "meteo", x) }))
anoure <- data.frame(lapply(anoure, function(x){
  gsub("wind", "meteo", x) }))

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

#Création tableau par année
anoure_2021 <- subset(anoure, anoure$Year == "2021")
anoure_2022 <- subset(anoure, anoure$Year == "2022")

#Ajouter les jours julien
anoure_2021$jour_julien <- julian(as.Date(paste(anoure_2021$Year, anoure_2021$Month, anoure_2021$Day), format = "%Y %m %d"), origin = as.Date("2021-01-01"))
anoure_2022$jour_julien <- julian(as.Date(paste(anoure_2022$Year, anoure_2022$Month, anoure_2022$Day), format = "%Y %m %d"), origin = as.Date("2022-01-01"))

#------------------------------
# données MH
#------------------------------
MH <-read.csv("donnees/donnees_MH.csv", header = T)

# Enlever colonnes inutiles
MH <- subset(MH, select = c(Enregistre, CLASSE, surface))

#Enlever les lignes vides
MH <- subset(MH, MH$surface != 0)
MH <- subset(MH, MH$Enregistre != "")

#Faire la somme des types de MH identiques pour les mêmes sites
MH <- aggregate(surface ~ CLASSE + Enregistre, data = MH, sum)

#Pivoter le tableau de MH
MH <- spread(MH, CLASSE, surface)
MH <- replace(MH, is.na(MH), 0)

#Enlever la colonne V1 (voir routes)
MH <- subset(MH, select = -V1)

#Mettre les tourbières ensemble
MH$Tourbiere <- MH$`Tourbière boisée indifférenciée`+MH$`Tourbière boisée minérotrophe`+MH$`Tourbière ouverte minérotrophe`
MH$Marecage <- MH$`Marécage`+MH$`Marécage arborescent`
MH <- subset(MH, select = c(Enregistre, `Eau peu profonde`, Marais, Marecage, `Milieu humide`, Tourbiere))

#------------------------------
# données routes
#------------------------------
routes <- read.csv("donnees/donnees_routes.csv", header = T)

# Enlever colonnes inutiles
routes <- subset(routes, select = c(ClsRte, Enregistre, longueur))

#Enlever les lignes vides
routes <- subset(routes, routes$Enregistre != "")

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

#Faire la somme des types de .utilisation de territoire identiques pour les mêmes sites
uti_terr <- aggregate(surface ~ DN + Enregistre, data = uti_terr, sum)

#Lier le tableau de code et le tableau utilisation territoire
uti_terr <- merge(uti_terr, codes_uti_terr, by.x = "DN", by.y = "code")

#Enlever colones inutiles
uti_terr <- subset(uti_terr, select = c(Enregistre, surface, utilisation))

#Pivoter le tableau
uti_terr <- spread(uti_terr, utilisation, surface)
uti_terr <- replace(uti_terr, is.na(uti_terr), 0)

#Joindre colonnes
uti_terr$Agriculture <- uti_terr$`Agriculture indifférenciée`+uti_terr$`Agriculture indifférenciée / Marais`+uti_terr$`Agriculture indifférenciée / Marais`+uti_terr$`Agriculture indifférenciée / Marécage`+uti_terr$`Agriculture indifférenciée / Prairie humide`+uti_terr$Blé+uti_terr$`Blé / Marécage`+uti_terr$`Blé / Tourbière`+uti_terr$`Blé / Tourbière minérotrophe`+uti_terr$Maïs+uti_terr$`Maïs / Marais`+uti_terr$`Maïs / Marécage`+uti_terr$`Maïs / Prairie humide`+uti_terr$`Maïs / Tourbière`+uti_terr$`Maïs / Tourbière minérotrophe`+uti_terr$Soya+uti_terr$`Soya / Marécage`+uti_terr$`Soya / Tourbière minérotrophe`+uti_terr$`Culture pérenne et pâturage`+uti_terr$`Culture pérenne et pâturage / Marais`+uti_terr$`Culture pérenne et pâturage / Marécage`+uti_terr$`Culture pérenne et pâturage / Prairie humide`+uti_terr$`Culture pérenne et pâturage / Tourbière minérotrophe`

uti_terr$Coupe <- uti_terr$`Coupe forestière / Marais`+uti_terr$`Coupe forestière / Marécage`+uti_terr$`Coupe forestière / Prairie humide`+uti_terr$`Coupe forestière / Tourbière minérotrophe`

uti_terr$Friche <- uti_terr$Friche+uti_terr$`Friche / Marécage`

uti_terr$Conifere <- uti_terr$`Thuyas occidental associé à des feuillus tolérants`+uti_terr$`Peuplement issu d'une plantation de résineux indigènes`+uti_terr$`Peuplement issu d'une plantation de résineux indigènes / Marécage`+uti_terr$`Pruche du Canada`+uti_terr$`Pruche du Canada / Marais`+uti_terr$`Pruche du Canada associée à des érables`

uti_terr$Feuillu <- uti_terr$`Chênes / Marécage`+uti_terr$`Chênes / Tourbière`+uti_terr$`Feuillus indéterminés / Marécage`+uti_terr$`Feuillus indéterminés / Tourbière`+uti_terr$`Feuillus indéterminés / Tourbière minérotrophe`+uti_terr$`Feuillus tolérants`+uti_terr$`Feuillus tolérants / Marais`+uti_terr$`Feuillus tolérants / Tourbière`+uti_terr$`Érable à sucre`+uti_terr$`Érable argenté / Marais`+uti_terr$`Érable argenté / Marécage`+uti_terr$`Érable argenté / Prairie humide`+uti_terr$`Érable argenté / Tourbière`+uti_terr$`Érable argenté / Tourbière minérotrophe`+uti_terr$`Érable rouge`+uti_terr$`Érable rouge / Marécage`+uti_terr$`Érable rouge / Tourbière minérotrophe`+uti_terr$Érables+uti_terr$`Érables / Marais`+uti_terr$`Érables / Marécage`+uti_terr$`Érables / Tourbière`+uti_terr$`Érables associés à de la pruche du Canada`

uti_terr$Riviere <- uti_terr$Lac+uti_terr$`Lac / Marais`+uti_terr$`Lac / Marécage`+uti_terr$`Lac / Tourbière`+uti_terr$`Lac / Tourbière minérotrophe`+uti_terr$Eau+uti_terr$`Eau / Marais`+uti_terr$`Eau / Tourbière`+uti_terr$`Eau / Tourbière minérotrophe`

#enlever zone dev pcq seulement à une place
uti_terr <- subset(uti_terr, select = c(Enregistre, Agriculture, Coupe, Friche, Conifere, Feuillu, Riviere))

#Enlever codes territoire parce que pus besoin
rm(codes_uti_terr)
#------------------------------
# Création tableaux pour analyse
#------------------------------

#------------------------------
# Tableaux occupation
#------------------------------
type_site <-read.csv("donnees/type_site.csv", header = T)

occupation_anoure <- merge(MH, routes, by ="Enregistre")
occupation_anoure <- merge(occupation_anoure, uti_terr, by = "Enregistre")
occupation_anoure <- merge(occupation_anoure, type_site, by = "Enregistre")
occupation_anoure$Eau <- occupation_anoure$`Eau peu profonde` + occupation_anoure$Marais + occupation_anoure$Riviere
occupation_anoure$Humide <- occupation_anoure$Marecage + occupation_anoure$`Milieu humide` + occupation_anoure$Tourbiere
occupation_anoure <- subset(occupation_anoure, select = c(Enregistre, Total, Agriculture, Eau, Humide, Type)) 
colnames(occupation_anoure) <- c("Enregistre", "Route", "Agriculture", "Eau", "Humide", "Site")
#------------------------------
# tableaux variables réponses
#------------------------------
# une seule observation à 15h
#Seulement 3 pour 21h donc garder le 1h seulement
### CRÉÉER TABLEAU LICAT 2021 à 1H00 et minuit ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_21_1h <- subset(anoure_2021, anoure_2021$Time24H == "100" | anoure_2021$Time24H == "0", select = c(Site, jour_julien, LICAT))
licat_21_1h <- spread(licat_21_1h, jour_julien, LICAT)

##Joindre visites
colnames(licat_21_1h)<- c("Site","V145","V146","V148","V151","V152","V154","V155","V159","V161","V4","V5","V6","V7","V193","V194")
for (i in 1:26) {
  if( !is.na(licat_21_1h$V145[i])
  ) {licat_21_1h$V1[i]<-licat_21_1h$V145[i]}
  else if( !is.na(licat_21_1h$V146[i])
  ) {licat_21_1h$V1[i]<-licat_21_1h$V146[i]}
  else if(!is.na(licat_21_1h$V148[i])
  ) {licat_21_1h$V1[i]<-licat_21_1h$V148[i]}
  else {licat_21_1h$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(licat_21_1h$V151[i])
  ) {licat_21_1h$V2[i]<-licat_21_1h$V151[i]}
  else if( !is.na(licat_21_1h$V152[i])
  ) {licat_21_1h$V2[i]<-licat_21_1h$V152[i]}
  else if(!is.na(licat_21_1h$V154[i])
  ) {licat_21_1h$V2[i]<-licat_21_1h$V154[i]}
  else if(!is.na(licat_21_1h$V155[i])
  ) {licat_21_1h$V2[i]<-licat_21_1h$V155[i]}
  else {licat_21_1h$V2[i]<-NA}
}
#V3
for (i in 1:26) {
  if( !is.na(licat_21_1h$V159[i])
  ) {licat_21_1h$V3[i]<-licat_21_1h$V159[i]}
  else if( !is.na(licat_21_1h$V161[i])
  ) {licat_21_1h$V3[i]<-licat_21_1h$V161[i]}
  else {licat_21_1h$V3[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(licat_21_1h$V193[i])
  ) {licat_21_1h$V8[i]<-licat_21_1h$V193[i]}
  else if( !is.na(licat_21_1h$V194[i])
  ) {licat_21_1h$V8[i]<-licat_21_1h$V194[i]}
  else {licat_21_1h$V8[i]<-NA}
}

licat_21_1h <- subset(licat_21_1h, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8))

#une seule observation à 15h en 2022
#Seulement 4 obervations en 2022 à 21h
### CRÉÉER TABLEAU LICAT 2022 à 1H00 et minuit ###
#Ajouter le  $LICAT!="0" pour avoir seulement site où présent
licat_22_1h <- subset(anoure_2022, anoure_2022$Time24H == "100" | anoure_2022$Time24H == "0", select = c(Site, jour_julien, LICAT))
licat_22_1h <- spread(licat_22_1h, jour_julien, LICAT)

##Joindre visites
colnames(licat_22_1h)<- c("Site","V109","V110","V112","V113","V116","V117","V3","V4","V5","V6","V7","V157","V158","V9","V10","V11","V12","V13","V14","V15","V16")
#V1
for (i in 1:26) {
  if( !is.na(licat_22_1h$V109[i])
  ) {licat_22_1h$V1[i]<-licat_22_1h$V109[i]}
  else if( !is.na(licat_22_1h$V110[i])
  ) {licat_22_1h$V1[i]<-licat_22_1h$V110[i]}
  else if(!is.na(licat_22_1h$V112[i])
  ) {licat_22_1h$V1[i]<-licat_22_1h$V112[i]}
  else if(!is.na(licat_22_1h$V113[i])
  ) {licat_22_1h$V1[i]<-licat_22_1h$V113[i]}
  else {licat_22_1h$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(licat_22_1h$V116[i])
  ) {licat_22_1h$V2[i]<-licat_22_1h$V116[i]}
  else if( !is.na(licat_22_1h$V117[i])
  ) {licat_22_1h$V2[i]<-licat_22_1h$V117[i]}
  else {licat_22_1h$V2[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(licat_22_1h$V157[i])
  ) {licat_22_1h$V8[i]<-licat_22_1h$V157[i]}
  else if( !is.na(licat_22_1h$V158[i])
  ) {licat_22_1h$V8[i]<-licat_22_1h$V158[i]}
  else {licat_22_1h$V8[i]<-NA}
}
licat_22_1h <- subset(licat_22_1h, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16))


### CRÉÉER TABLEAU LSYL 2022 ###
lisyl_22_21h <- subset(anoure_2022, anoure_2022$Time24H == "2100", select = c(Site, jour_julien, LISYL))
lisyl_22_21h <- spread(lisyl_22_21h, jour_julien, LISYL)

##Joindre visites
colnames(lisyl_22_21h)<- c("Site","V108","V109","V111","V112","V115","V116","V3","V4","V5","V6","V7","V156","V157","V9","V10","V11","V12","V13","V14","V15","V16")
#V1
for (i in 1:26) {
  if( !is.na(lisyl_22_21h$V108[i])
  ) {lisyl_22_21h$V1[i]<-lisyl_22_21h$V108[i]}
  else if( !is.na(lisyl_22_21h$V109[i])
  ) {lisyl_22_21h$V1[i]<-lisyl_22_21h$V109[i]}
  else if(!is.na(lisyl_22_21h$V111[i])
  ) {lisyl_22_21h$V1[i]<-lisyl_22_21h$V111[i]}
  else if(!is.na(lisyl_22_21h$V112[i])
  ) {lisyl_22_21h$V1[i]<-lisyl_22_21h$V112[i]}
  else {lisyl_22_21h$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(lisyl_22_21h$V115[i])
  ) {lisyl_22_21h$V2[i]<-lisyl_22_21h$V115[i]}
  else if( !is.na(lisyl_22_21h$V116[i])
  ) {lisyl_22_21h$V2[i]<-lisyl_22_21h$V116[i]}
  else {lisyl_22_21h$V2[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(lisyl_22_21h$V156[i])
  ) {lisyl_22_21h$V8[i]<-lisyl_22_21h$V156[i]}
  else if( !is.na(lisyl_22_21h$V157[i])
  ) {lisyl_22_21h$V8[i]<-lisyl_22_21h$V157[i]}
  else {lisyl_22_21h$V8[i]<-NA}
}
lisyl_22_21h <- subset(lisyl_22_21h, select = c(Site, V1, V2))
colnames(lisyl_22_21h) <- c("Site", "V1", "V3")

#Tableau à 1h
lisyl_22_1h <- subset(anoure_2022, anoure_2022$Time24H == "100" | anoure_2022$Time24H == "0", select = c(Site, jour_julien, LISYL))
lisyl_22_1h <- spread(lisyl_22_1h, jour_julien, LISYL)
colnames(lisyl_22_1h)<- c("Site","V109","V110","V112","V113","V2","V117","V3","V4","V5","V6","V7","V157","V158","V9","V10","V11","V12","V13","V14","V15","V16")
#V1
for (i in 1:26) {
  if( !is.na(lisyl_22_1h$V109[i])
  ) {lisyl_22_1h$V1[i]<-lisyl_22_1h$V109[i]}
  else if( !is.na(lisyl_22_1h$V110[i])
  ) {lisyl_22_1h$V1[i]<-lisyl_22_1h$V110[i]}
  else if(!is.na(lisyl_22_1h$V112[i])
  ) {lisyl_22_1h$V1[i]<-lisyl_22_1h$V112[i]}
  else if(!is.na(lisyl_22_1h$V113[i])
  ) {lisyl_22_1h$V1[i]<-lisyl_22_1h$V113[i]}
  else {lisyl_22_1h$V1[i]<-NA}
}
lisyl_22_1h <- subset(lisyl_22_1h, select = c(Site, V1, V2))
colnames(lisyl_22_1h) <- c("Site", "V2", "V4")

#Merge les deux
lisyl <- merge(lisyl_22_1h, lisyl_22_21h, by = "Site")
lisyl <- subset(lisyl, select = c(V1, V2, V3, V4))

#------------------------------
# Tableau Détection
#------------------------------
qualite_2021_15 <- subset(anoure_2021, anoure_2021$Time24H == "1500", select = c(Site, jour_julien, RecordingQuality))
qualite_2021_15 <- spread(qualite_2021_15, jour_julien, RecordingQuality)

qualite_2021_21 <- subset(anoure_2021, anoure_2021$Time24H == "2100", select = c(Site, jour_julien, RecordingQuality))
qualite_2021_21 <- spread(qualite_2021_21, jour_julien, RecordingQuality)

qualite_2021_1 <- subset(anoure_2021, anoure_2021$Time24H == "100" | anoure_2021$Time24H == "0", select = c(Site, jour_julien, RecordingQuality))
qualite_2021_1 <- spread(qualite_2021_1, jour_julien, RecordingQuality)
colnames(qualite_2021_1)<- c("Site","V145","V146","V148","V151","V152","V154","V155","V159","V161","V4","V5","V6","V7","V193","V194")
for (i in 1:26) {
  if( !is.na(qualite_2021_1$V145[i])
  ) {qualite_2021_1$V1[i]<-qualite_2021_1$V145[i]}
  else if( !is.na(qualite_2021_1$V146[i])
  ) {qualite_2021_1$V1[i]<-qualite_2021_1$V146[i]}
  else if(!is.na(qualite_2021_1$V148[i])
  ) {qualite_2021_1$V1[i]<-qualite_2021_1$V148[i]}
  else {qualite_2021_1$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(qualite_2021_1$V151[i])
  ) {qualite_2021_1$V2[i]<-qualite_2021_1$V151[i]}
  else if( !is.na(qualite_2021_1$V152[i])
  ) {qualite_2021_1$V2[i]<-qualite_2021_1$V152[i]}
  else if(!is.na(qualite_2021_1$V154[i])
  ) {qualite_2021_1$V2[i]<-qualite_2021_1$V154[i]}
  else if(!is.na(qualite_2021_1$V155[i])
  ) {qualite_2021_1$V2[i]<-qualite_2021_1$V155[i]}
  else {qualite_2021_1$V2[i]<-NA}
}
#V3
for (i in 1:26) {
  if( !is.na(qualite_2021_1$V159[i])
  ) {qualite_2021_1$V3[i]<-qualite_2021_1$V159[i]}
  else if( !is.na(qualite_2021_1$V161[i])
  ) {qualite_2021_1$V3[i]<-qualite_2021_1$V161[i]}
  else {qualite_2021_1$V3[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(qualite_2021_1$V193[i])
  ) {qualite_2021_1$V8[i]<-qualite_2021_1$V193[i]}
  else if( !is.na(qualite_2021_1$V194[i])
  ) {qualite_2021_1$V8[i]<-qualite_2021_1$V194[i]}
  else {qualite_2021_1$V8[i]<-NA}
}

qualite_2021_1 <- subset(qualite_2021_1, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8))

##2022##
qualite_2022_15 <- subset(anoure_2022, anoure_2022$Time24H == "1500", select = c(Site, jour_julien, RecordingQuality))
qualite_2022_15 <- spread(qualite_2022_15, jour_julien, RecordingQuality)

qualite_2022_21 <- subset(anoure_2022, anoure_2022$Time24H == "2100", select = c(Site, jour_julien, RecordingQuality))
qualite_2022_21 <- spread(qualite_2022_21, jour_julien, RecordingQuality)
colnames(qualite_2022_21)<- c("Site","V108","V109","V111","V112","V115","V116","V3","V4","V5","V6","V7","V156","V157","V9","V10","V11","V12","V13","V14","V15","V16")
#V1
for (i in 1:26) {
  if( !is.na(qualite_2022_21$V108[i])
  ) {qualite_2022_21$V1[i]<-qualite_2022_21$V108[i]}
  else if( !is.na(qualite_2022_21$V109[i])
  ) {qualite_2022_21$V1[i]<-qualite_2022_21$V109[i]}
  else if(!is.na(qualite_2022_21$V111[i])
  ) {qualite_2022_21$V1[i]<-qualite_2022_21$V111[i]}
  else if(!is.na(qualite_2022_21$V112[i])
  ) {qualite_2022_21$V1[i]<-qualite_2022_21$V112[i]}
  else {qualite_2022_21$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(qualite_2022_21$V115[i])
  ) {qualite_2022_21$V2[i]<-qualite_2022_21$V115[i]}
  else if( !is.na(qualite_2022_21$V116[i])
  ) {qualite_2022_21$V2[i]<-qualite_2022_21$V116[i]}
  else {qualite_2022_21$V2[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(qualite_2022_21$V156[i])
  ) {qualite_2022_21$V8[i]<-qualite_2022_21$V156[i]}
  else if( !is.na(qualite_2022_21$V157[i])
  ) {qualite_2022_21$V8[i]<-qualite_2022_21$V157[i]}
  else {qualite_2022_21$V8[i]<-NA}
}
qualite_2022_21 <- subset(qualite_2022_21, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16))

qualite_2022_1 <- subset(anoure_2022, anoure_2022$Time24H == "100" | anoure_2022$Time24H == "0", select = c(Site, jour_julien, RecordingQuality))
qualite_2022_1 <- spread(qualite_2022_1, jour_julien, RecordingQuality)
colnames(qualite_2022_1)<- c("Site","V109","V110","V112","V113","V116","V117","V3","V4","V5","V6","V7","V157","V158","V9","V10","V11","V12","V13","V14","V15","V16")
#V1
for (i in 1:26) {
  if( !is.na(qualite_2022_1$V109[i])
  ) {qualite_2022_1$V1[i]<-qualite_2022_1$V109[i]}
  else if( !is.na(qualite_2022_1$V110[i])
  ) {qualite_2022_1$V1[i]<-qualite_2022_1$V110[i]}
  else if(!is.na(qualite_2022_1$V112[i])
  ) {qualite_2022_1$V1[i]<-qualite_2022_1$V112[i]}
  else if(!is.na(qualite_2022_1$V113[i])
  ) {qualite_2022_1$V1[i]<-qualite_2022_1$V113[i]}
  else {qualite_2022_1$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(qualite_2022_1$V116[i])
  ) {qualite_2022_1$V2[i]<-qualite_2022_1$V116[i]}
  else if( !is.na(qualite_2022_1$V117[i])
  ) {qualite_2022_1$V2[i]<-qualite_2022_1$V117[i]}
  else {qualite_2022_1$V2[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(qualite_2022_1$V157[i])
  ) {qualite_2022_1$V8[i]<-qualite_2022_1$V157[i]}
  else if( !is.na(qualite_2022_1$V158[i])
  ) {qualite_2022_1$V8[i]<-qualite_2022_1$V158[i]}
  else {qualite_2022_1$V8[i]<-NA}
}
qualite_2022_1 <- subset(qualite_2022_1, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16))

#Tableau qualité pour lisyl
qualite_lisyl_21 <- subset(qualite_2022_21, select = c(Site, V1, V2))
qualite_lisyl_1 <- subset(qualite_2022_1, select = c(Site, V1, V2))

colnames(qualite_lisyl_21) <- c("Site", "V1", "V3")
colnames(qualite_lisyl_1) <- c("Site", "V2", "V4")
#Merge les deux
qualite_lisyl <- merge(qualite_lisyl_21, qualite_lisyl_1, by = "Site")
qualite_lisyl <- subset(qualite_lisyl, select = c(V1, V2, V3, V4))


#Type de disturbance
disturb_2021_15 <- subset(anoure_2021, anoure_2021$Time24H == "1500", select = c(Site, jour_julien, DisturbanceType))
disturb_2021_15 <- spread(disturb_2021_15, jour_julien, DisturbanceType)

disturb_2021_21 <- subset(anoure_2021, anoure_2021$Time24H == "2100", select = c(Site, jour_julien, DisturbanceType))
disturb_2021_21 <- spread(disturb_2021_21, jour_julien, DisturbanceType)

disturb_2021_1 <- subset(anoure_2021, anoure_2021$Time24H == "100" | anoure_2021$Time24H == "0", select = c(Site, jour_julien, DisturbanceType))
disturb_2021_1 <- spread(disturb_2021_1, jour_julien, DisturbanceType)
colnames(disturb_2021_1)<- c("Site","V145","V146","V148","V151","V152","V154","V155","V159","V161","V4","V5","V6","V7","V193","V194")
for (i in 1:26) {
  if( !is.na(disturb_2021_1$V145[i])
  ) {disturb_2021_1$V1[i]<-disturb_2021_1$V145[i]}
  else if( !is.na(disturb_2021_1$V146[i])
  ) {disturb_2021_1$V1[i]<-disturb_2021_1$V146[i]}
  else if(!is.na(disturb_2021_1$V148[i])
  ) {disturb_2021_1$V1[i]<-disturb_2021_1$V148[i]}
  else {disturb_2021_1$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(disturb_2021_1$V151[i])
  ) {disturb_2021_1$V2[i]<-disturb_2021_1$V151[i]}
  else if( !is.na(disturb_2021_1$V152[i])
  ) {disturb_2021_1$V2[i]<-disturb_2021_1$V152[i]}
  else if(!is.na(disturb_2021_1$V154[i])
  ) {disturb_2021_1$V2[i]<-disturb_2021_1$V154[i]}
  else if(!is.na(disturb_2021_1$V155[i])
  ) {disturb_2021_1$V2[i]<-disturb_2021_1$V155[i]}
  else {disturb_2021_1$V2[i]<-NA}
}
#V3
for (i in 1:26) {
  if( !is.na(disturb_2021_1$V159[i])
  ) {disturb_2021_1$V3[i]<-disturb_2021_1$V159[i]}
  else if( !is.na(disturb_2021_1$V161[i])
  ) {disturb_2021_1$V3[i]<-disturb_2021_1$V161[i]}
  else {disturb_2021_1$V3[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(disturb_2021_1$V193[i])
  ) {disturb_2021_1$V8[i]<-disturb_2021_1$V193[i]}
  else if( !is.na(disturb_2021_1$V194[i])
  ) {disturb_2021_1$V8[i]<-disturb_2021_1$V194[i]}
  else {disturb_2021_1$V8[i]<-NA}
}
disturb_2021_1 <- subset(disturb_2021_1, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8))


#2022

disturb_2022_15 <- subset(anoure_2022, anoure_2022$Time24H == "1500", select = c(Site, jour_julien, DisturbanceType))
disturb_2022_15 <- spread(disturb_2022_15, jour_julien, DisturbanceType)

disturb_2022_21 <- subset(anoure_2022, anoure_2022$Time24H == "2100", select = c(Site, jour_julien, DisturbanceType))
disturb_2022_21 <- spread(disturb_2022_21, jour_julien, DisturbanceType)
colnames(disturb_2022_21)<- c("Site","V108","V109","V111","V112","V115","V116","V3","V4","V5","V6","V7","V156","V157","V9","V10","V11","V12","V13","V14","V15","V16")
#V1
for (i in 1:26) {
  if( !is.na(disturb_2022_21$V108[i])
  ) {disturb_2022_21$V1[i]<-disturb_2022_21$V108[i]}
  else if( !is.na(disturb_2022_21$V109[i])
  ) {disturb_2022_21$V1[i]<-disturb_2022_21$V109[i]}
  else if(!is.na(disturb_2022_21$V111[i])
  ) {disturb_2022_21$V1[i]<-disturb_2022_21$V111[i]}
  else if(!is.na(disturb_2022_21$V112[i])
  ) {disturb_2022_21$V1[i]<-disturb_2022_21$V112[i]}
  else {disturb_2022_21$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(disturb_2022_21$V115[i])
  ) {disturb_2022_21$V2[i]<-disturb_2022_21$V115[i]}
  else if( !is.na(disturb_2022_21$V116[i])
  ) {disturb_2022_21$V2[i]<-disturb_2022_21$V116[i]}
  else {disturb_2022_21$V2[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(disturb_2022_21$V156[i])
  ) {disturb_2022_21$V8[i]<-disturb_2022_21$V156[i]}
  else if( !is.na(disturb_2022_21$V157[i])
  ) {disturb_2022_21$V8[i]<-disturb_2022_21$V157[i]}
  else {disturb_2022_21$V8[i]<-NA}
}
disturb_2022_21 <- subset(disturb_2022_21, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16))


disturb_2022_1 <- subset(anoure_2022, anoure_2022$Time24H == "100" | anoure_2022$Time24H == "0", select = c(Site, jour_julien, DisturbanceType))
disturb_2022_1 <- spread(disturb_2022_1, jour_julien, DisturbanceType)
colnames(disturb_2022_1)<- c("Site","V109","V110","V112","V113","V116","V117","V3","V4","V5","V6","V7","V157","V158","V9","V10","V11","V12","V13","V14","V15","V16")
#V1
for (i in 1:26) {
  if( !is.na(disturb_2022_1$V109[i])
  ) {disturb_2022_1$V1[i]<-disturb_2022_1$V109[i]}
  else if( !is.na(disturb_2022_1$V110[i])
  ) {disturb_2022_1$V1[i]<-disturb_2022_1$V110[i]}
  else if(!is.na(disturb_2022_1$V112[i])
  ) {disturb_2022_1$V1[i]<-disturb_2022_1$V112[i]}
  else if(!is.na(disturb_2022_1$V113[i])
  ) {disturb_2022_1$V1[i]<-disturb_2022_1$V113[i]}
  else {disturb_2022_1$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(disturb_2022_1$V116[i])
  ) {disturb_2022_1$V2[i]<-disturb_2022_1$V116[i]}
  else if( !is.na(disturb_2022_1$V117[i])
  ) {disturb_2022_1$V2[i]<-disturb_2022_1$V117[i]}
  else {disturb_2022_1$V2[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(disturb_2022_1$V157[i])
  ) {disturb_2022_1$V8[i]<-disturb_2022_1$V157[i]}
  else if( !is.na(disturb_2022_1$V158[i])
  ) {disturb_2022_1$V8[i]<-disturb_2022_1$V158[i]}
  else {disturb_2022_1$V8[i]<-NA}
}
disturb_2022_1 <- subset(disturb_2022_1, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16))

#Tableau perturbation pour lisyl
disturb_lisyl_21 <- subset(disturb_2022_21, select = c(Site, V1, V2))
disturb_lisyl_1 <- subset(disturb_2022_1, select = c(Site, V1, V2))

colnames(disturb_lisyl_21) <- c("Site", "V1", "V3")
colnames(disturb_lisyl_1) <- c("Site", "V2", "V4")
#Merge les deux
disturb_lisyl <- merge(disturb_lisyl_21, disturb_lisyl_1, by = "Site")
disturb_lisyl <- subset(disturb_lisyl, select = c(V1, V2, V3, V4))

##Jours juliens lisyl
jj_22_21h<-read.csv("donnees/JJ_22_21h.csv", header = T)
jj_22_1h<-read.csv("donnees/JJ_22_1h.csv", header = T)

jj_22_21h <- subset(jj_22_21h, select = c(Site, V1, V2))
jj_22_1h <- subset(jj_22_1h, select = c(Site, V1, V2))

colnames(jj_22_21h) <- c("Site", "V1", "V3")
colnames(jj_22_1h) <- c("Site", "V2", "V4")

jj_lisyl <- merge(jj_22_21h, jj_22_1h, by = "Site")
jj_lisyl <- subset(jj_lisyl, select = c(V1, V2, V3, V4))
#------------------------------
# Enregistrement données nettoyées
#------------------------------
#write.csv(uti_terr, 'donnees/utilisation_territoire_nett.csv', row.names = FALSE)
#write.csv(MH, 'donnees/MH_nett.csv', row.names = FALSE)
write.csv(anoure, 'donnees/anoure_nett.csv', row.names = FALSE)
#write.csv(routes, 'donnees/routes_nett.csv', row.names = FALSE)
write.csv(occupation_anoure, 'donnees/donnees_occupation.csv', row.names = FALSE)

write.csv(licat_21_1h, 'donnees/licat_21_1h.csv', row.names = FALSE)
write.csv(licat_22_1h, 'donnees/licat_22_1h.csv', row.names = FALSE)
write.csv(lisyl, 'donnees/lisyl.csv', row.names = FALSE)

#write.csv(qualite_2021_15, 'donnees/qualite_2021_15h.csv', row.names = FALSE)
#write.csv(qualite_2021_21, 'donnees/qualite_2021_21h.csv', row.names = FALSE)
#write.csv(qualite_2021_1, 'donnees/qualite_2021_1h.csv', row.names = FALSE)
#write.csv(qualite_2022_15, 'donnees/qualite_2022_15h.csv', row.names = FALSE)
#write.csv(qualite_2022_21, 'donnees/qualite_2022_21h.csv', row.names = FALSE)
#write.csv(qualite_2022_1, 'donnees/qualite_2022_1h.csv', row.names = FALSE)
write.csv(qualite_lisyl, 'donnees/qualite_lisyl.csv', row.names = FALSE)

#write.csv(disturb_2021_15, 'donnees/disturb_2021_15h.csv', row.names = FALSE)
#write.csv(disturb_2021_21, 'donnees/disturb_2021_21h.csv', row.names = FALSE)
#write.csv(disturb_2021_1, 'donnees/disturb_2021_1h.csv', row.names = FALSE)
#write.csv(disturb_2022_15, 'donnees/disturb_2022_15h.csv', row.names = FALSE)
#write.csv(disturb_2022_21, 'donnees/disturb_2022_21h.csv', row.names = FALSE)
#write.csv(disturb_2022_1, 'donnees/disturb_2022_1h.csv', row.names = FALSE)
write.csv(disturb_lisyl, 'donnees/disturb_lisyl.csv', row.names = FALSE)

write.csv(jj_lisyl, 'donnees/jj_lisyl.csv', row.names = F)
