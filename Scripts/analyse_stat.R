library(unmarked)

#------------------------------
# Ouvrir données
#------------------------------
#Données réponse
#licat_21_15h <- read.csv('donnees/licat_21_15h.csv', header = T)
licat_21_21h <- read.csv('donnees/licat_21_21h.csv', header = T)
licat_21_1h <- read.csv('donnees/licat_21_1h.csv', header = T)
#licat_22_15h <- read.csv('donnees/licat_21_15h.csv', header = T)
licat_22_21h <- read.csv('donnees/licat_21_21h.csv', header = T)
licat_22_1h <- read.csv('donnees/licat_21_1h.csv', header = T)

#Données occupation
MH <- read.csv("donnees/MH_nett.csv", header = T)
route <- read.csv("donnees/routes_nett.csv", header = T)
territoire <- read.csv("donnees/utilisation_territoire_nett.csv", header = T)

#Données détection
qualite_2021_15h <- read.csv('donnees/qualite_2021_15h.csv', header = T)
qualite_2021_15h <- read.csv('donnees/qualite_2021_21h.csv', header = T)
qualite_2021_15h <- read.csv('donnees/qualite_2021_1h.csv', header = T)
qualite_2021_15h <- read.csv('donnees/qualite_2022_15h.csv', header = T)
qualite_2021_15h <- read.csv('donnees/qualite_2022_21h.csv', header = T)
qualite_2021_15h <- read.csv('donnees/qualite_2022_1h.csv', header = T)

disturb_2021_15h <- read.csv('donnees/disturb_2021_15h.csv', header = T)
disturb_2021_15h <- read.csv('donnees/disturb_2021_21h.csv', header = T)
disturb_2021_15h <- read.csv('donnees/disturb_2021_1h.csv', header = T)
disturb_2021_15h <- read.csv('donnees/disturb_2022_15h.csv', header = T)
disturb_2021_15h <- read.csv('donnees/disturb_2022_21h.csv', header = T)
disturb_2021_15h <- read.csv('donnees/disturb_2022_1h.csv', header = T)

#------------------------------
# Analyse
#------------------------------