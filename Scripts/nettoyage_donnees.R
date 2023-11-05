setwd("~/Desktop/Credits_recherche/CR_anoure")

library(tidyr)

#------------------------------
# données anoures
#------------------------------
anoure<-read.csv("donnees/donnees_anoures.csv", header = T)

# Corriger erreurs dans DisturbanceType
anoure <- data.frame(lapply(anoure, function(x){
  gsub("human et rain", "1", x) }))
anoure <- data.frame(lapply(anoure, function(x){
  gsub("human et wind", "1", x) }))
anoure <- data.frame(lapply(anoure, function(x){
  gsub("human", "1", x) }))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("rain et human", "2", x) }))
anoure <- data.frame(lapply(anoure, function(x){
  gsub("rain et wind", "2", x) }))
anoure <- data.frame(lapply(anoure, function(x){
  gsub("rain", "2", x) }))
  
anoure <- data.frame(lapply(anoure, function(x){
  gsub("wind et human", "3", x) }))
anoure <- data.frame(lapply(anoure, function(x){
  gsub("wind et rain", "3", x) }))
anoure <- data.frame(lapply(anoure, function(x){
  gsub("wind", "3", x) }))

###CODE POUR RECORDING QUALITY###
anoure <- data.frame(lapply(anoure, function(x){
  gsub("good", "1", x) }))

anoure<- data.frame(lapply(anoure, function(x){
  gsub("moderate", "2", x) }))

anoure <- data.frame(lapply(anoure, function(x){
  gsub("bad", "3", x) }))

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
# Création tableaux pour analyse
#------------------------------

#Création tableau par année
anoure_2021 <- subset(anoure, anoure$Year == "2021")
anoure_2022 <- subset(anoure, anoure$Year == "2022")

#Ajouter les jours julien
anoure_2021$jour_julien <- julian(as.Date(paste(anoure_2021$Year, anoure_2021$Month, anoure_2021$Day), format = "%Y %m %d"), origin = as.Date("2021-01-01"))
anoure_2022$jour_julien <- julian(as.Date(paste(anoure_2022$Year, anoure_2022$Month, anoure_2022$Day), format = "%Y %m %d"), origin = as.Date("2022-01-01"))

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


### CRÉÉER TABLEAU LSYL 2022 à 21H00 ###
#Il y en a quelques unes aussi à 1h mais vraiment plus à 21h
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
lisyl_22_21h <- subset(lisyl_22_21h, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16))

### TABLEAU HYVER ###
#Hyver 2021 21h
hyver_21_21h <- subset(anoure_2021, anoure_2021$Time24H == "2100", select = c(Site, jour_julien, HYVER))
hyver_21_21h <- spread(hyver_21_21h, jour_julien, HYVER)
##Joindre visites
colnames(hyver_21_21h)<- c("Site","V144","V145","V147","V150","V151","V153","V154","V3","V160","V165","V5","V179","V180","V7","V192", "V193")
for (i in 1:26) {
  if( !is.na(hyver_21_21h$V144[i])
  ) {hyver_21_21h$V1[i]<-hyver_21_21h$V144[i]}
  else if( !is.na(hyver_21_21h$V145[i])
  ) {hyver_21_21h$V1[i]<-hyver_21_21h$V145[i]}
  else if(!is.na(hyver_21_21h$V147[i])
  ) {hyver_21_21h$V1[i]<-hyver_21_21h$V147[i]}
  else {hyver_21_21h$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(hyver_21_21h$V150[i])
  ) {hyver_21_21h$V2[i]<-hyver_21_21h$V150[i]}
  else if( !is.na(hyver_21_21h$V151[i])
  ) {hyver_21_21h$V2[i]<-hyver_21_21h$V151[i]}
  else if(!is.na(hyver_21_21h$V153[i])
  ) {hyver_21_21h$V2[i]<-hyver_21_21h$V153[i]}
  else if(!is.na(hyver_21_21h$V154[i])
  ) {hyver_21_21h$V2[i]<-hyver_21_21h$V154[i]}
  else {hyver_21_21h$V2[i]<-NA}
}
#V4
for (i in 1:26) {
  if( !is.na(hyver_21_21h$V160[i])
  ) {hyver_21_21h$V4[i]<-hyver_21_21h$V160[i]}
  else if( !is.na(hyver_21_21h$V165[i])
  ) {hyver_21_21h$V4[i]<-hyver_21_21h$V165[i]}
  else {hyver_21_21h$V4[i]<-NA}
}
#V6
for (i in 1:26) {
  if( !is.na(hyver_21_21h$V179[i])
  ) {hyver_21_21h$V6[i]<-hyver_21_21h$V179[i]}
  else if( !is.na(hyver_21_21h$V180[i])
  ) {hyver_21_21h$V6[i]<-hyver_21_21h$V180[i]}
  else {hyver_21_21h$V6[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(hyver_21_21h$V192[i])
  ) {hyver_21_21h$V8[i]<-hyver_21_21h$V192[i]}
  else if( !is.na(hyver_21_21h$V193[i])
  ) {hyver_21_21h$V8[i]<-hyver_21_21h$V193[i]}
  else {hyver_21_21h$V8[i]<-NA}
}
hyver_21_21h <- subset(hyver_21_21h, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8))

#Hyver 2022 21h
hyver_22_21h <- subset(anoure_2022, anoure_2022$Time24H == "2100", select = c(Site, jour_julien, HYVER))
hyver_22_21h <- spread(hyver_22_21h, jour_julien, HYVER)
#Joindre visites
colnames(hyver_22_21h)<- c("Site","V108","V109","V111","V112","V115","V116","V3","V4","V5","V6","V7","V156","V157","V9","V10","V11","V12","V13","V14","V15","V16")
#V1
for (i in 1:26) {
  if( !is.na(hyver_22_21h$V108[i])
  ) {hyver_22_21h$V1[i]<-hyver_22_21h$V108[i]}
  else if( !is.na(hyver_22_21h$V109[i])
  ) {hyver_22_21h$V1[i]<-hyver_22_21h$V109[i]}
  else if(!is.na(hyver_22_21h$V111[i])
  ) {hyver_22_21h$V1[i]<-hyver_22_21h$V111[i]}
  else if(!is.na(hyver_22_21h$V112[i])
  ) {hyver_22_21h$V1[i]<-hyver_22_21h$V112[i]}
  else {hyver_22_21h$V1[i]<-NA}
}
#V2
for (i in 1:26) {
  if( !is.na(hyver_22_21h$V115[i])
  ) {hyver_22_21h$V2[i]<-hyver_22_21h$V115[i]}
  else if( !is.na(hyver_22_21h$V116[i])
  ) {hyver_22_21h$V2[i]<-hyver_22_21h$V116[i]}
  else {hyver_22_21h$V2[i]<-NA}
}
#V8
for (i in 1:26) {
  if( !is.na(hyver_22_21h$V156[i])
  ) {hyver_22_21h$V8[i]<-hyver_22_21h$V156[i]}
  else if( !is.na(hyver_22_21h$V157[i])
  ) {hyver_22_21h$V8[i]<-hyver_22_21h$V157[i]}
  else {hyver_22_21h$V8[i]<-NA}
}
hyver_22_21h <- subset(hyver_22_21h, select = c(Site, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16))

hyver_22_21h <- data.frame(lapply(hyver_22_21h, function(x){
  gsub("1", "0", x) }))
hyver_22_21h <- data.frame(lapply(hyver_22_21h, function(x){
  gsub("2", "1", x) }))
hyver_22_21h <- data.frame(lapply(hyver_22_21h, function(x){
  gsub("3", "1", x) }))

hyver_21_21h <- data.frame(lapply(hyver_21_21h, function(x){
  gsub("1", "0", x) }))
hyver_21_21h <- data.frame(lapply(hyver_21_21h, function(x){
  gsub("2", "1", x) }))
hyver_21_21h <- data.frame(lapply(hyver_21_21h, function(x){
  gsub("3", "1", x) }))

hyver_21_21h <- subset(hyver_21_21h, hyver_21_21h$HYVER != "0")

### TABLEAU PSCRU
pscru_21_21h <- subset(anoure_2021, anoure_2021$Time24H == "2100", select = c(Site, jour_julien, PSCRU))
pscru_21_1h <- subset(anoure_2021, anoure_2021$Time24H == "100" | anoure_2021$Time24H == "0", select = c(Site, jour_julien, PSCRU))
pscru_21_21h <- subset(pscru_21_21h, pscru_21_21h$PSCRU != "0")
pscru_21_1h <- subset(pscru_21_1h, pscru_21_1h$PSCRU != "0")
pscru_21_15h <- subset(anoure_2021, anoure_2021$Time24H == "1500", select = c(Site, jour_julien, PSCRU))
pscru_21_15h <- subset(pscru_21_15h, pscru_21_15h$PSCRU != "0")

pscru_22_21h <- subset(anoure_2022, anoure_2022$Time24H == "2100", select = c(Site, jour_julien, PSCRU))
pscru_22_1h <- subset(anoure_2022, anoure_2022$Time24H == "100" | anoure_2022$Time24H == "0", select = c(Site, jour_julien, PSCRU))
pscru_22_21h <- subset(pscru_22_21h, pscru_22_21h$PSCRU != "0")
pscru_22_1h <- subset(pscru_22_1h, pscru_22_1h$PSCRU != "0")
pscru_22_15h <- subset(anoure_2022, anoure_2022$Time24H == "1500", select = c(Site, jour_julien, PSCRU))
pscru_22_15h <- subset(pscru_22_15h, pscru_22_15h$PSCRU != "0")

pscru_22_1h <- data.frame(lapply(pscru_22_1h, function(x){
  gsub("1", "0", x) }))
pscru_22_1h <- data.frame(lapply(pscru_22_1h, function(x){
  gsub("2", "1", x) }))
pscru_22_1h <- data.frame(lapply(pscru_22_1h, function(x){
  gsub("3", "1", x) }))

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

#------------------------------
# Enregistrement données nettoyées
#------------------------------
write.csv(uti_terr, 'donnees/utilisation_territoire_nett.csv', row.names = FALSE)
write.csv(MH, 'donnees/MH_nett.csv', row.names = FALSE)
write.csv(anoure, 'donnees/anoure_nett.csv', row.names = FALSE)
write.csv(routes, 'donnees/routes_nett.csv', row.names = FALSE)

write.csv(licat_21_1h, 'donnees/licat_21_1h.csv', row.names = FALSE)
write.csv(licat_22_1h, 'donnees/licat_22_1h.csv', row.names = FALSE)
write.csv(lisyl_22_21h, 'donnees/lisyl_22_21h.csv', row.names = FALSE)

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

