#####################################################
# Projet : Analyse COVID-19 au S�n�gal: partie 1    #
# Auteur : Ousmane Sy Bodian,                       #
# Profil : Ing�nieur statisticien, Data scientist   # 
# Date d�but : 13/06/2020                           #
# Date fin :   20/07/2020                           #
#####################################################


#----------------------- Libreries requises dans ce projet ----------------------
library(fpp2)        # pour la pr�vision
library(tidyverse)   # pour faire des requ�te sur la data
library(plotly)      # Graphe interractive (visualisation)
library(lubridate)  # gestion format date
library(scales)    # pour changer l'echelle 'ymd' en 'jj mm'


#---------------------- Importation de la base de donn�es -----------------------
# La base de donn�es est mis � jours tous jours
# avec de nouvelles observations. A t�l�charger (Source GitHub).
# Source GitHub: https://github.com/senegalouvert/COVID-19
# Donn�es recueillies � la date : 06-13-2020

covid19_SN <- read.csv("confirmes.csv", sep = ",", header = T)

#--------------------- Inventaire sur la classe (type) des variables -------------
# Liste nomminative des variables en colonne
names(covid19_SN)
# Type de variable
str(covid19_SN)

#------------------------------ Gestion du format date --------------------
# consulter le format de date de la base 'tail()'
tail(covid19_SN$date)

# A priori, R ne reconnait pas la structure des dates (la variable temporelle)
# pour une s�rie chronologique. Donc,les lignes suivantes vont nous
# permettre de lui faire comprendre que la variable 'date' est ici notre
# variable temporelle.

# Mettre le format classe 'date' avec 'as.POSIXct()' = classe 'date'
# puis ordonner la base de donn�es dans l'ordre croissant 
covid19_SN <- covid19_SN %>%
  mutate(date = as.POSIXct(date)) %>%
  arrange(date) 

# Viz new format date
str(covid19_SN)
head(covid19_SN)



#-------------------------------- Statistiques descriptives ---------------------------
# Stat.desc
summary(covid19_SN)

# Frequence des cas positifs
tab <- table(as.factor(covid19_SN$cas))
print(tab)
freq_cas <- round(prop.table(tab), 4)*100
print(freq_cas)

# D�coupage en classes
classe <- cut(covid19_SN$cas, breaks = seq(0, max(covid19_SN$cas) + 25, by = 25), 
              include.lowest = T)
print(classe)

# Frequence des classes
freq_classe <- round(prop.table(table(classe)), 4)*100
print(freq_classe)

#--------------------- Etude de Corr�lation entre les variables ----------------------

# La commande suivante du package 'GGally' permet de tracer 
# la matrice de corr�lation des variables avec une subtilit�
# qui est ici d'afficher les nuages de points en m�me temps.
GGally::ggpairs(covid19_SN[,c(2, 3, 6, 8, 9, 11)]) +
  theme_minimal()

# Nuages de points: 'Nbre de morts' en fonction des 'gu�ris' 
# (avec 'geom-smooth(method = "lm")')
ggplot(covid19_SN, aes(x = gueri, y = mort)) +
  geom_smooth(method = "lm") +
  geom_point() +
  ggtitle("Evolution du nbre de personnes d�c�d�es du COVID-19\n en fonction du nbre de celles gu�ries") +
  xlab("Nbre cummul� de personnes gu�ries par jour") +
  ylab("Nbre de personnes d�c�d�es") +
  theme_minimal()




#-------------------------- Situation de la pand�mie dans le pays -----------------------

# Dans cette partie, nous allons visualiser deux graphes


# Graphe 1 : Evolution quotidienne du nombre de cas (contacts, communautaires et graves)

# Par d�faut, le pas de temps utilis� est le mois
# Pour passer � un pas de temps plus fin, par semaine, 
# il est n�cessaire de cr�er la s�quence de date correspondante, comme ceci :
datebreaks <- seq(as.Date("2020-03-02"), as.Date("2020-06-13"), by = "1 week")

# Nous pouvons alors pr�ciser ce pas de temps, � l'aide de l'argument 'breaks' 
# de la fonction 'scale_x_date()', comme ceci :

# Graphe
# Faisable grace au package 'lubridate'
ggplot(covid19 <- covid19_SN %>% 
  mutate(date = date(date))) +  # Obligatoire pour la fonction 'scale_x_date(breaks = datebreaks)'
  geom_line(aes(x = date, y = cas, col = "cas contacts")) +
  geom_line(aes(x = date, y = communautaire, col = "cas communautaires")) +
  geom_line(aes(x = date, y = grave, col = "cas graves")) +
  geom_line(aes(x = date, y = tests /100, col = "Tests r�alis�s (en centaine)")) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-03-23")],
             col = "red", size = 1, linetype = 2) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-05-11")],
             col = "orange", size = .8, linetype = 2) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-06-05")],
             col = "cornflowerblue", size = .8, linetype = 2) +
  scale_x_date(breaks = datebreaks) +
  guides(colour = guide_legend("")) +
  ggtitle("Nombre quotidien de cas du COVID-19 au S�n�gal\n du 02 Mars au 13 Juin 2020") +
  xlab("") +
  ylab("Nbre de cas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1), 
        legend.position = "bottom")

# En effet,le graphe ci-dessus prend en compte des dates importantes correspondant 
# aux diff�rentes mesures prises par les autorit�s dans le cadre de la lutte contre
# pand�mie.

# Dates des mesures prises par le PR Macky Sall

#***** Lundi 16 Mars 2020 : arret des enseignements sur tout le territoire
# plus de p�lerinage et renforcement des controles sur toutes les fronti�res 
# 18 Mars 2020 : suspension de l'exploitation des vols et navigations maritimes
# 19 Mars : Fermeture des mosqu�s 
#**** 23 Mars 2020 : D�claration de l'�tat d'urgence 'COUVRE FEU' de 20h � 06h.

#***** 11 Mai 2020 : Plus de deux mois apr�s le 23 Mars 'COUVRE FEU'
#***** Assouplissement des conditions de l'�tat d'urgence d�sormais 21h � 06h
# R�ouverture des march�s et des lieux de cultes.
# Ces mesures ont �t� prises suites � plusieurs revendications du peuple et 
# de la pression des chefs religieux

#***** 5 Juin : relachement des transports interurbains 
# apr�s plusieurs manifestations dans le pays


# Graphe 2 : Distribution du nombre cumul� des cas (total, gueri, traitement)  
ggplot(covid19 <- covid19_SN %>%
         mutate(date = date(date)) %>%
         mutate(traitement = total - gueri)) +
  geom_line(aes(x = date, y = total, col = "Nbre total atteint")) +
  geom_line(aes(x = date, y = gueri, col = "Nbre total gu�ri")) +
  geom_line(aes(x = date, y = traitement, col = "Nbre total sous traitement")) +
  geom_line(aes(x = date, y = mort, col = "Nbre total d�c�d�")) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-04-10")],
             col = "brown", size = .8, linetype = 2) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-04-24")],
             col = "orange", size = .8, linetype = 2) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-06-01")],
             col = "skyblue", size = .8, linetype = 2) +
  scale_x_date(breaks = datebreaks) +
  guides(colour = guide_legend("")) +
  ggtitle("Nombre cumul� des cas du COVID-19 au S�n�gal\n du 02 Mars au 13 Juin 2020") +
  xlab("") +
  ylab("Nbre cumul�") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1), 
        legend.position = "bottom")





#------------------------------- Cr�ation des s�ries chronologiques -----------------------------

# Notons qu'entre temps, la base de donn�es a �t� mis � jour d�sormais 
# elle part de 2020-03-02 au 2020-06-28

# Importons la nouvelle base de donn�es
covid19_SN <- read.csv("confirmes28juin.csv", sep = ",", header = T)

# Mettre la variable date au format : classe 'date' avec 'as.POSIXct()' = classe 'date'
# puis ordonner la base de donn�es dans l'ordre croissant 
covid19_SN <- covid19_SN %>%
  mutate(date = as.POSIXct(date)) %>%
  arrange(date)

# V�rification de la classe des variables
str(covid19_SN)


#**** Constat :
# Notons que notre s�rie pr�sente des donn�es quotidiennnes du 2020-03-02 au 2020-06-02
# Donc on choisit de cr�er deux s�ries de fr�quence 7 jours (par semaine)
# Dans le but de mieux voir les tendances et saisonnalit�s

#--- Avant la s�rie chronologique,

# Cr�ation des indices de dates � frequence weekly (hebdomadaire) 
inds <- seq(as.Date("2020-03-02"), as.Date("2020-06-28"), by = "week")

# D�but des date: Cr�ation du d�but des dates 
# (il s'agit de notre premi�re semaine d'observation) 
begin <- as.numeric(format(inds[1], "%W"))


#************** Cr�ation propre de la S�rie chronologique ('Nbre de cas confirm�s')
#************************* de frequence hebdomadaire (weekly) *********************
ts_covid19 <- ts(covid19_SN[ ,3], start = begin, frequency = 7)
head(force(ts_covid19), 10)





#----------------------------------- Etude tendancielle et saisonni�re ---------------------------

# Visualisation des chronogrammes avec 'autoplot()'

#----------------- Etude des tendances
autoplot(ts_covid19, col = "brown") +
  ggtitle("COVID-19 : Nombre de cas confirm�s au S�n�gal\n du 02 Mars au 28 Juin 2020") +
  xlab("Semaines") +
  ylab("Nbre de cas confirm�s") +
  theme_minimal()

# Le code suivant permet de rendre interractive le graphe ci dessus.
g <- autoplot(ts_covid19, col = "brown") +
  ggtitle("COVID-19 : Nombre de cas confirm�s au S�n�gal\n du 02 Mars au 28 Juin 2020") +
  xlab("Semaines") +
  ylab("Nbre de cas confirm�s") +
  theme_minimal()
# Visulisation Graphique interractif
ggplotly(g)



#------------- Etude de la saisonnalit�

# Le premier diagramme saisonnier 
ggseasonplot(ts_covid19, year.labels = T, year.labels.left = T) +
  ggtitle("Diagramme saisonnier") +
  theme_minimal()


# Diagramme saisonnier en coordonn�es polaires 
ggseasonplot(ts_covid19, polar = T) +
  guides(colour = guide_legend(title="Weeks")) +
  ggtitle("Diagramme saisonnier en coordonn�es polaires") +
  theme_minimal()


# Diagramme de la sous-s�rie saisonni�re
ggsubseriesplot(ts_covid19) +
  theme_minimal() 



# Deuxi�me diagramme de la sous-s�rie saisonni�re
gglagplot(ts_covid19) +
  theme_minimal()


# Corr�logramme ou ACF (interractif)
g2 <- ggAcf(ts_covid19, lag = 35) +
  xlab("Les d�calages") +
  ggtitle("Corr�logramme") +
  theme_minimal()

ggplotly(g2)




#---------------------------------- Les m�thodes de pr�vision de r�f�rence ----------------------------- 

# Actualisons la base de donn�es qui part de 02-03-2020 � 14-07-2020

# Importons la nouvelle base de donn�es
covid19_SN <- read.csv("confirmes_14Juillet.csv", sep = ",", header = T)

# Mettre la variable date au format : classe 'date' avec 'as.POSIXct()' = classe 'date'
# puis ordonner la base de donn�es dans l'ordre croissant 
covid19_SN <- covid19_SN %>%
  mutate(date = as.POSIXct(date)) %>%
  arrange(date)

# Il faut recr�er � nouveau la s�rie chronologique avec la nouvelle base de donn�es

# Notons que notre s�rie pr�sente des donn�es quotidiennnes du 2020-03-02 au 2020-07-14
# Donc on choisit de cr�er deux s�ries de fr�quence 7 jours (par semaine)
# Dans le but de mieux voir les tendances et saisonnalit�s

#*** S�rie chronologique de frequence hebdomadaire (weekly) ***
# Cr�ation du d�but des dates (il s'agit de notre premi�re semaine d'observation) 
# frequence weekly
inds <- seq(as.Date("2020-03-02"), as.Date("2020-07-14"), by = "week")
# d�but des date
begin <- as.numeric(format(inds[1], "%W"))

# s�ries chronologiques
ts_covid19 <- ts(covid19_SN[ ,3], start = begin, frequency = 7)
head(force(ts_covid19), 10)


#---------- Visualisation du chronogramme
# Avec 'autoplot()'
g <- autoplot(ts_covid19, col = "blue") +
  ggtitle("COVID-19 : Nombre de cas confirm�s au S�n�gal\n du 02 Mars au 28 Juin 2020") +
  xlab("Semaines") +
  ylab("Nbre de cas confirm�s") +
  theme_minimal()
# Visulisation Graphique interractif
ggplotly(g)


#--------------- Appliquons les trois premi�res m�thodes de pr�vision ------------
#-------------------------- sur nos donn�es hebdomadaires -----------------------

# Donn�es d'entrainement de la 11e semaine � la 25e semaine 
# (On a �liminer les valuers nulles de la s�rie)
ts_covid2 <- window(ts_covid19, start = c(11, 1), end = c(25, 1))
# Apper�u de la nouvelle s�ries
head(ts_covid2)

# Visualisation des m�thodes de pr�vision
autoplot(ts_covid2) +
  autolayer(meanf(ts_covid2, h=14),
            series="Mean", PI=FALSE) +
  autolayer(naive(ts_covid2, h=14),
            series="Na�ve", PI=FALSE) +
  autolayer(snaive(ts_covid2, h=14),
            series="Seasonal na�ve", PI=FALSE) +
  ggtitle("Les m�thodes de pr�vision de r�f�rence") +
  xlab("semaines") + ylab("Nbre de cas confirm�s") +
  guides(colour=guide_legend(title="Pr�vision :")) +
  theme_minimal() +
  theme(legend.position = "bottom")




#------------------------------- Transformations et ajustements des donn�es ---------------------------

#---------------------------------------- Transformation Box-Cox -------------------------------------

# Choix de la valeur de lambda par R
ts_covid19 <- window(ts_covid19, start = c(11, 1))
(lambda <- BoxCox.lambda(ts_covid19))
#> [1] 0.2424546

# Visualisation du chronogramme des donn�es ajust�es
autoplot(BoxCox(ts_covid19, lambda)) +
  xlab("") +
  ylab("") +
  ggtitle("Donn�es ajust�es selon la transformation Box-Cox\n de param�tre lambda") +
  theme_minimal()

# Visualisons la s�rie ajust�e et la s�rie non ajust�e
# Regroupons les deux s�ries sur un m�me graphe
ts_combined <- cbind(ajusted_ts = BoxCox(ts_covid19, lambda), 
                     no_ajusted_ts = ts_covid19)

# V�rifions la classe de l'objet cr��
class(ts_combined) # calss : 'ts' 

# Les chronogrammes des deux s�ries
autoplot(ts_combined[, 1:2], facets = T) +
  xlab("semaines") +
  ylab("Nbre de cas confirm�s") +
  ggtitle("S�rie chronologique ajust�e (en haut) et \n s�rie chronologique non ajust�e (en bas)") +
  theme_minimal()




#--------------------------------- Transformation avec ajustement du biais ----------------------------

# Donn�es Ajust�es en fonction du biais 
# avec la m�thode saisonni�re naive
fc <- snaive(ts_covid2, lambda=0, h=14, level=80) # ajustement simple
fc2 <- snaive(ts_covid2, lambda=0, h=14, level=80, # ajustement en fonction du biais
              biasadj=TRUE)

# Comparaison des pr�visions entre les donn�es ajust�es simplement
# et celles ajust�es en fonction du biais.
autoplot(ts_covid2) +
  autolayer(fc, series="Simple  r�trotransformation") +
  autolayer(fc2, series="Biais adjust�", PI=FALSE) +
  xlab("") +
  ylab("") +
  ggtitle("Pr�visions des donn�es ajust�es simplement \n et celles ajust�es en fonction du biais") +
  guides(colour=guide_legend(title="Pr�vision")) +
  theme_minimal() +
  theme(legend.position = "bottom")




#------------------------------------- Diagnostics r�siduels ---------------------------------

# La commande suivante permet de g�n�rer plusieurs diagrammes :
# 1. Le diagramme temporel des r�sidus
# 2. L'Histogramme des r�sidus 
# 3. Le diagramme d'Autocorr�lation (ACF) des r�sidus
checkresiduals(fc2) + # r�sidu de la m�thode saisonni�re naive
  theme_minimal()     # avec ajustement du biais


#---------------------------------- Tests de Portmanteau pour l'autocorr�lation -------------------------
#------------------------------- Voir l'article 'SNCovi19' pour plus d'explication ----------------------
# R�sidus
res <- residuals(fc2)
#---------- Test de Box-Pierce
# lag=h and fitdf=K
Box.test(res, lag=14, fitdf=0)
#> 
#> 	Box-Pierce test
#> 
#> data:  res
#> X-squared (Q) = 60.488, df = 14, p-value = 9.632e-08

# ---------- Test de Ljung-Box
Box.test(res,lag=14, fitdf=0, type="Lj")
#> 
#> 	Box-Ljung test
#> 
#> data:  res
#> X-squared (Q*) = 65.564, df = 14, p-value = 1.213e-08






#----------------------------------- �valuation de l'exactitude des pr�visions --------------------------

#--------- D�coupage des donn�es en donn�es d'Entrainement ('train') et
#---------------------------------- Donn�es de Test

# Etalage du nombre de semaines
# etendu = 28.1 - 11 = 17.1
# 70% * etendu = 17.1 * 0.8 = 16.68 
# Donc, nos donn�es de 'train' irrons jusqu'� 
# debut = 11 + 70% * etendu = 26e semaine, 1e jour.
tsTrain <- window(ts_covid19, end = c(26, 1))   
tsTest <- window(ts_covid19, start = c(27, 1)) 

# Cr�ation des mod�les de r�f�rence
# A partir du jeux d'entrainement "tsTrain"
ts_covidFit1 <- meanf(tsTrain, h = 14)
ts_covidFit2 <- rwf(tsTrain, h = 14)
ts_covidFit3 <- snaive(tsTrain, h = 14)


# Visualisation de la pr�vision
autoplot(ts_covid19) +
  autolayer(ts_covidFit1, series="Mean", PI=FALSE) +
  autolayer(ts_covidFit2, series="Na�ve", PI=FALSE) +
  autolayer(ts_covidFit3, series="Seasonal na�ve", PI=FALSE) +
  xlab("semaines") + ylab("Nbre de cas confirm�s") +
  ggtitle("Pr�vision de nombre de cas confirm�s du Covid19 \n en utilisant les donn�es hebdomadaires") +
  guides(colour=guide_legend(title="Pr�vision")) +
  theme_minimal() +
  theme(legend.position = "bottom")





#----------------------------------- Evaluation de l'exactitude des pr�visions -----------------------------
accuracy(ts_covidFit1, tsTest)    # Evaluation m�thode moyenne
accuracy(ts_covidFit2, tsTest)    # Evaluation m�thode naive
accuracy(ts_covidFit3, tsTest)    # Evaluation m�thode naive saisonni�re



#-------------------------------------- Intervalle de pr�diction ------------------------------------------
# Pour voir les intervalles de pr�diction
# Il suffit de faire la cmd suivante
print(fc)
print(fc2)



#******************************************* FIN DE LA PHASE 1 DE L'ETUDE *****************************************