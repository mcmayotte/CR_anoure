library (RSQLite)
con <- dbConnect(SQLite(), dbname = "db_anoure")

# Aller chercher les données
anoure<-read.csv("donnees/anoure_nett.csv", header = T)
MH<-read.csv("donnees/MH_nett.csv", header = T)
uti_terr<-read.csv("donnees/utilisation_territoire_nett.csv", header = T)
codes_uti_terr<-read.csv("donnees/codes_utilisation_terr_nett.csv", header = T)

#######Cle 1 - anoure#####
tbl_anoure <-"
CREATE TABLE anoure (
  Site VARCHAR(10),
  Year INTEGER,
  Month INTEGER,
  Day INTEGER,
  Time24H INTEGER,
  RecordingQuality VARCHAR(8),
  DisturbanceType VARCHAR(5),
  ANAME INTEGER,
  HYVER INTEGER,
  LICLA INTEGER,
  LICAT INTEGER,
  LIPAL INTEGER,
  LIPIP INTEGER,
  LISEP INTEGER,
  LISYL INTEGER,
  PSMACTRI INTEGER,
  PSCRU INTEGER,
  fileName VARCHAR(50),
  PRIMARY KEY (fileName)
);"
dbSendQuery(con,tbl_anoure)

#######Cle 2 - code#####
tbl_codes <-"
CREATE TABLE codes (
  code INTEGER,
  utilisation VARCHAR(100),
  PRIMARY KEY (code)
);"
dbSendQuery(con,tbl_codes)

#######Cle 4 - MH#####
tbl_MH <-"
CREATE TABLE MH (
  fid INTEGER,
  CLASSE VARCHAR(50),
  Enregistre VARCHAR(10),
  Surface REAL,
  Surface_pourc REAL,
  PRIMARY KEY (fid)
);"
dbSendQuery(con,tbl_MH)

#######Cle Primaire - utilisation territoire#####
tbl_utilisation_terr <-"
CREATE TABLE utilisation_terr (
  fid INTEGER,
  utilisatio INTEGER,
  Enregistre VARCHAR(10),
  Surface REAL,
  surf_pourc REAL,
  PRIMARY KEY (fid),
  FOREIGN KEY (utilisatio) REFERENCES codes(code),
  FOREIGN KEY (Enregistre) REFERENCES anoure(Site)
);"
dbSendQuery(con,tbl_utilisation_terr)

#Mettre les données dans la bd
dbWriteTable(con, append = TRUE, name= "anoure", value = anoure, row.names = FALSE)
dbWriteTable(con, append = TRUE, name= "codes", value = codes_uti_terr, row.names = FALSE)
dbWriteTable(con, append = TRUE, name= "MH", value = MH, row.names = FALSE)
dbWriteTable(con, append = TRUE, name= "utilisation_terr", value = uti_terr, row.names = FALSE)

# REQUETE 1 - Lier table utilisation territoire et les codes
#1- join anoure et MH pour que pour chaque ligne d'enregistrement il y ait le pourc de surface de chaque type de milieu humide
#2- join util terr et code pour savir quelle code correspond a quoi
#3- join util terri a anoure pour faire comme avec MH



#Requetes nb collaboration par r.a.
#sql_requete <- "
#SELECT collaborations.etudiant1, collaborations.etudiant2, collaborations.sigle, collaborations.session, etudiants.region_administrative AS region_administrative_et1
#FROM collaborations
#JOIN etudiants ON collaborations.etudiant1 = etudiants.prenom_nom
#;"

#rae1 <- dbGetQuery(con, sql_requete)
#head(rae1)


dbDisconnect(con)
