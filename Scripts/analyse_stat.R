library(unmarked)

#------------------------------
# Ouvrir données
#------------------------------
#Données réponse

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

