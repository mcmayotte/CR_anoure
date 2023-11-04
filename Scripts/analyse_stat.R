#------------------------------
# Ouvrir données
#------------------------------
#Données réponse
licat_21_1h <- read.csv('donnees/licat_21_1h.csv', header = T)
licat_22_1h <- read.csv('donnees/licat_21_1h.csv', header = T)
lisyl_22_21h <- read.csv('donnees/lisyl_21_21h.csv', header = T)

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
# Analyse
#------------------------------
library(unmarked)

licat_21_1h <- subset(licat_21_1h, select = -c(Site))
disturb_2021_1h <- subset(disturb_2021_1h, select = -c(Site))
qualite_2021_1h <- subset(qualite_2021_1h, select = -c(Site))
MH <- subset(MH, select = c(Enregistre, Tourbiere))

#Création données unmarked
licat_21_1h.data <- unmarkedFrameOccu(y = licat_21_1h, siteCovs = route, obsCovs = list(disturbance = disturb_2021_1h, qualite = qualite_2021_1h))
licat_21_1h.data
summary(licat_21_1h.data)

#Modèles LICAT 2021 1h
#Détectabilité et occupation constante
m0 <- occu(~ 1 ~ 1, data = licat_21_1h.data)
m0
##Détectabilité varie, mais occupation est constante
m1 <- occu(~ disturbance + qualite ~ 1, data = licat_21_1h.data)
m1
##Détectabilité varie et l'occupation varie
m2 <- occu(~ disturbance + qualite ~ Locale, data = licat_21_1h.data)
m2
##Détectabilité constante et l'occupation varie
m3 <- occu(~ 1 ~ Locale, data = licat_21_1h.data)
m3
