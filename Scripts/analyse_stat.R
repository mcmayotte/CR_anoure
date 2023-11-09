#------------------------------
# Ouvrir données
#------------------------------

anoure <- read.csv('donnees/anoure_nett.csv', header = T)

#Données réponse
licat_21_1h <- read.csv('donnees/licat_21_1h.csv', header = T)
licat_22_1h <- read.csv('donnees/licat_21_1h.csv', header = T)
lisyl_22_21h <- read.csv('donnees/lisyl_22_21h.csv', header = T)

#Données occupation
MH <- read.csv("donnees/MH_nett.csv", header = T)
route <- read.csv("donnees/routes_nett.csv", header = T)
territoire <- read.csv("donnees/utilisation_territoire_nett.csv", header = T)

#Données détection
#qualite_2021_15h <- read.csv('donnees/qualite_2021_15h.csv', header = T)
#qualite_2021_21h <- read.csv('donnees/qualite_2021_21h.csv', header = T)
qualite_2021_1h <- read.csv('donnees/qualite_2021_1h.csv', header = T)
#qualite_2022_15h <- read.csv('donnees/qualite_2022_15h.csv', header = T)
qualite_2022_21h <- read.csv('donnees/qualite_2022_21h.csv', header = T)
qualite_2022_1h <- read.csv('donnees/qualite_2022_1h.csv', header = T)

#disturb_2021_15h <- read.csv('donnees/disturb_2021_15h.csv', header = T)
#disturb_2021_21h <- read.csv('donnees/disturb_2021_21h.csv', header = T)
disturb_2021_1h <- read.csv('donnees/disturb_2021_1h.csv', header = T)
#disturb_2022_15h <- read.csv('donnees/disturb_2022_15h.csv', header = T)
disturb_2022_21h <- read.csv('donnees/disturb_2022_21h.csv', header = T)
disturb_2022_1h <- read.csv('donnees/disturb_2022_1h.csv', header = T)

jj_21_1h <- read.csv('donnees/JJ_21_1h.csv', header =T)
jj_22_1h <- read.csv('donnees/JJ_22_1h.csv', header =T)
jj_22_21h <- read.csv('donnees/JJ_22_21h.csv', header =T)

#------------------------------
# Corrélation des variables
#------------------------------

cor(anoure$RecordingQuality, anoure$DisturbanceType)
#0.5707699
detection_anoure <- subset(anoure, select = c(RecordingQuality, DisturbanceType))
plot(detection_anoure)

occupation_anoure <- merge(MH, route, by ="Enregistre")
occ_anoure <- merge(occupation_anoure, territoire, by = "Enregistre")
occ_anoure$Eau <- occ_anoure$Eau.peu.profonde + occ_anoure$Marais + occ_anoure$Riviere
occ_anoure$Humide <- occ_anoure$Marecage + occ_anoure$Milieu.humide + occ_anoure$Tourbiere
occ_anoure <- subset(occ_anoure, select = c(Enregistre, Total, Agriculture, Eau, Humide)) 

plot(occ_anoure)

#------------------------------
# Analyse
#------------------------------
library(unmarked)
library(AICcmodavg)
#######
#LICAT#
#######
licat_21_1h <- subset(licat_21_1h, select = -c(Site))
disturb_2021_1h <- subset(disturb_2021_1h, select = -c(Site))
qualite_2021_1h <- subset(qualite_2021_1h, select = -c(Site))

#Création données unmarked
licat_21_1h.data <- unmarkedFrameOccu(y = licat_21_1h, siteCovs = occ_anoure, obsCovs = list(disturbance = disturb_2021_1h, qualite = qualite_2021_1h))
licat_21_1h.data
summary(licat_21_1h.data)

#Modèles LICAT 2021 1h
#Détectabilité et occupation constante
m0 <- occu(~ 1 ~ 1, data = licat_21_1h.data)
m0
##Détectabilité varie, mais occupation est constante
mDetection <- occu(~ disturbance + qualite ~ 1, data = licat_21_1h.data)
mDetection

##Détectabilité varie et l'occupation varie
mHumide <- occu(~ disturbance + qualite ~ Humide, data = licat_21_1h.data)
mHumide
##Détectabilité constante et l'occupation varie
mHumide1 <- occu(~ 1 ~ Humide, data = licat_21_1h.data)
mHumide1

##Détectabilité varie et l'occupation varie
mEaulibre <- occu(~ disturbance + qualite ~ Eau, data = licat_21_1h.data)
mEaulibre
##Détectabilité constante et l'occupation varie
mEaulibre1 <- occu(~ 1 ~ Eau, data = licat_21_1h.data)
mEaulibre1

##Détectabilité varie et l'occupation varie
mAgriculture <- occu(~ disturbance + qualite ~ Agriculture, data = licat_21_1h.data)
##Détectabilité constante et l'occupation varie
mAgriculture1 <- occu(~ 1 ~ Agriculture, data = licat_21_1h.data)

##Détectabilité varie et l'occupation varie
mRoute <- occu(~ disturbance + qualite ~ Total, data = licat_21_1h.data)
##Détectabilité constante et l'occupation varie
mRoute1 <- occu(~ 1 ~ Total, data = licat_21_1h.data)

##Détectabilité varie et l'occupation varie
mAgri_Route <- occu(~ disturbance + qualite ~ Agriculture + Total, data = licat_21_1h.data)
##Détectabilité constante et l'occupation varie
mAgri_Route1 <- occu(~ 1 ~ Agriculture + Total, data = licat_21_1h.data)

Cands <- list(m0, mDetection, mHumide, mEaulibre, mRoute, mAgriculture)
##assign meaningful names to each model 
Model.names <- c("nulle", "psi(.)p(Qualité + Perturbation)","psi(Milieux humides)p(Qualité + Perturbation)", "psi(Eau libre)p(Qualité + Perturbation)", "psi(Route)p(Qualité + Perturbation)", "psi(Agriculture)p(Qualité + Perturbation)") 
##do model selection based on AICc 
aictab(cand.set = Cands,  modnames = Model.names)

#######
#LISYL#
#######

lisyl_22_21h <- subset(lisyl_22_21h, select = -c(Site))
disturb_2022_21h <- subset(disturb_2022_21h, select = -c(Site))
qualite_2022_21h <- subset(qualite_2022_21h, select = -c(Site))

#Création données unmarked
lisyl_22_21h.data <- unmarkedFrameOccu(y = lisyl_22_21h, siteCovs = occ_anoure, obsCovs = list(disturbance = disturb_2022_21h, qualite = qualite_2022_21h))
lisyl_22_21h.data
summary(lisyl_22_21h.data)

#Modèles
#Détectabilité et occupation constante
m0 <- occu(~ 1 ~ 1, data = lisyl_22_21h.data)
m0
##Détectabilité varie, mais occupation est constante
mDetection <- occu(~ disturbance + qualite ~ 1, data = lisyl_22_21h.data)
mDetection

##Détectabilité varie et l'occupation varie
mHumide <- occu(~ disturbance + qualite ~ Humide, data = lisyl_22_21h.data)
mHumide
##Détectabilité constante et l'occupation varie
mHumide1 <- occu(~ 1 ~ Humide, data = lisyl_22_21h.data)
mHumide1

##Détectabilité varie et l'occupation varie
mEaulibre <- occu(~ disturbance + qualite ~ Eau, data = lisyl_22_21h.data)
mEaulibre
##Détectabilité constante et l'occupation varie
mEaulibre1 <- occu(~ 1 ~ Eau, data = lisyl_22_21h.data)
mEaulibre1

##Détectabilité varie et l'occupation varie
mAgriculture <- occu(~ disturbance + qualite ~ Agriculture, data = lisyl_22_21h.data)
mAgriculture
##Détectabilité constante et l'occupation varie
mAgriculture1 <- occu(~ 1 ~ Agriculture, data = lisyl_22_21h.data)
mAgriculture1

##Détectabilité varie et l'occupation varie !!!
mRoute <- occu(~ disturbance + qualite ~ Total, data = lisyl_22_21h.data)
mRoute
##Détectabilité constante et l'occupation varie
mRoute1 <- occu(~ 1 ~ Total, data = lisyl_22_21h.data)
mRoute1

##Détectabilité varie et l'occupation varie
mAgri_Route <- occu(~ disturbance + qualite ~ Agriculture + Total, data = lisyl_22_21h.data)
mAgri_Route
##Détectabilité constante et l'occupation varie
mAgri_Route1 <- occu(~ 1 ~ Agriculture + Total, data = lisyl_22_21h.data)
mAgri_Route1

#fit du modele Route
system.time(gof <- mb.gof.test(mRoute,nsim = 1000, plot.hist = TRUE))


Cands <- list(m0, mDetection, mHumide, mEaulibre, mRoute, mAgriculture)
##assign meaningful names to each model 
Model.names <- c("nulle", "psi(.)p(Qualité + Perturbation)","psi(Milieux humides)p(Qualité + Perturbation)", "psi(Eau libre)p(Qualité + Perturbation)", "psi(Route)p(Qualité + Perturbation)", "psi(Agriculture)p(Qualité + Perturbation)") 
##do model selection based on AICc 
aictab(cand.set = Cands,  modnames = Model.names)

table(qualite_2021_1h, disturb_2021_1h)
