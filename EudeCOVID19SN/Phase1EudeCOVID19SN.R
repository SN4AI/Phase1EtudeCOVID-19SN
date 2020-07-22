#####################################################
# Projet : Analyse COVID-19 au Sénégal: partie 1    #
# Auteur : Ousmane Sy Bodian,                       #
# Profil : Ingénieur statisticien, Data scientist   # 
# Date début : 13/06/2020                           #
# Date fin :   20/07/2020                           #
#####################################################


#----------------------- Libreries requises dans ce projet ----------------------
library(fpp2)        # pour la prévision
library(tidyverse)   # pour faire des requête sur la data
library(plotly)      # Graphe interractive (visualisation)
library(lubridate)  # gestion format date
library(scales)    # pour changer l'echelle 'ymd' en 'jj mm'


#---------------------- Importation de la base de données -----------------------
# La base de données est mis à jours tous jours
# avec de nouvelles observations. A télécharger (Source GitHub).
# Source GitHub: https://github.com/senegalouvert/COVID-19
# Données recueillies à la date : 06-13-2020

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
# pour une série chronologique. Donc,les lignes suivantes vont nous
# permettre de lui faire comprendre que la variable 'date' est ici notre
# variable temporelle.

# Mettre le format classe 'date' avec 'as.POSIXct()' = classe 'date'
# puis ordonner la base de données dans l'ordre croissant 
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

# Découpage en classes
classe <- cut(covid19_SN$cas, breaks = seq(0, max(covid19_SN$cas) + 25, by = 25), 
              include.lowest = T)
print(classe)

# Frequence des classes
freq_classe <- round(prop.table(table(classe)), 4)*100
print(freq_classe)

#--------------------- Etude de Corrélation entre les variables ----------------------

# La commande suivante du package 'GGally' permet de tracer 
# la matrice de corrélation des variables avec une subtilité
# qui est ici d'afficher les nuages de points en même temps.
GGally::ggpairs(covid19_SN[,c(2, 3, 6, 8, 9, 11)]) +
  theme_minimal()

# Nuages de points: 'Nbre de morts' en fonction des 'guéris' 
# (avec 'geom-smooth(method = "lm")')
ggplot(covid19_SN, aes(x = gueri, y = mort)) +
  geom_smooth(method = "lm") +
  geom_point() +
  ggtitle("Evolution du nbre de personnes décédées du COVID-19\n en fonction du nbre de celles guéries") +
  xlab("Nbre cummulé de personnes guéries par jour") +
  ylab("Nbre de personnes décédées") +
  theme_minimal()




#-------------------------- Situation de la pandémie dans le pays -----------------------

# Dans cette partie, nous allons visualiser deux graphes


# Graphe 1 : Evolution quotidienne du nombre de cas (contacts, communautaires et graves)

# Par défaut, le pas de temps utilisé est le mois
# Pour passer à un pas de temps plus fin, par semaine, 
# il est nécessaire de créer la séquence de date correspondante, comme ceci :
datebreaks <- seq(as.Date("2020-03-02"), as.Date("2020-06-13"), by = "1 week")

# Nous pouvons alors préciser ce pas de temps, à l'aide de l'argument 'breaks' 
# de la fonction 'scale_x_date()', comme ceci :

# Graphe
# Faisable grace au package 'lubridate'
ggplot(covid19 <- covid19_SN %>% 
  mutate(date = date(date))) +  # Obligatoire pour la fonction 'scale_x_date(breaks = datebreaks)'
  geom_line(aes(x = date, y = cas, col = "cas contacts")) +
  geom_line(aes(x = date, y = communautaire, col = "cas communautaires")) +
  geom_line(aes(x = date, y = grave, col = "cas graves")) +
  geom_line(aes(x = date, y = tests /100, col = "Tests réalisés (en centaine)")) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-03-23")],
             col = "red", size = 1, linetype = 2) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-05-11")],
             col = "orange", size = .8, linetype = 2) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-06-05")],
             col = "cornflowerblue", size = .8, linetype = 2) +
  scale_x_date(breaks = datebreaks) +
  guides(colour = guide_legend("")) +
  ggtitle("Nombre quotidien de cas du COVID-19 au Sénégal\n du 02 Mars au 13 Juin 2020") +
  xlab("") +
  ylab("Nbre de cas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1), 
        legend.position = "bottom")

# En effet,le graphe ci-dessus prend en compte des dates importantes correspondant 
# aux différentes mesures prises par les autorités dans le cadre de la lutte contre
# pandémie.

# Dates des mesures prises par le PR Macky Sall

#***** Lundi 16 Mars 2020 : arret des enseignements sur tout le territoire
# plus de pèlerinage et renforcement des controles sur toutes les frontières 
# 18 Mars 2020 : suspension de l'exploitation des vols et navigations maritimes
# 19 Mars : Fermeture des mosqués 
#**** 23 Mars 2020 : Déclaration de l'état d'urgence 'COUVRE FEU' de 20h à 06h.

#***** 11 Mai 2020 : Plus de deux mois après le 23 Mars 'COUVRE FEU'
#***** Assouplissement des conditions de l'état d'urgence désormais 21h à 06h
# Réouverture des marchés et des lieux de cultes.
# Ces mesures ont été prises suites à plusieurs revendications du peuple et 
# de la pression des chefs religieux

#***** 5 Juin : relachement des transports interurbains 
# après plusieurs manifestations dans le pays


# Graphe 2 : Distribution du nombre cumulé des cas (total, gueri, traitement)  
ggplot(covid19 <- covid19_SN %>%
         mutate(date = date(date)) %>%
         mutate(traitement = total - gueri)) +
  geom_line(aes(x = date, y = total, col = "Nbre total atteint")) +
  geom_line(aes(x = date, y = gueri, col = "Nbre total guéri")) +
  geom_line(aes(x = date, y = traitement, col = "Nbre total sous traitement")) +
  geom_line(aes(x = date, y = mort, col = "Nbre total décédé")) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-04-10")],
             col = "brown", size = .8, linetype = 2) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-04-24")],
             col = "orange", size = .8, linetype = 2) +
  geom_vline(xintercept = covid19$date[which(covid19$date == "2020-06-01")],
             col = "skyblue", size = .8, linetype = 2) +
  scale_x_date(breaks = datebreaks) +
  guides(colour = guide_legend("")) +
  ggtitle("Nombre cumulé des cas du COVID-19 au Sénégal\n du 02 Mars au 13 Juin 2020") +
  xlab("") +
  ylab("Nbre cumulé") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1), 
        legend.position = "bottom")





#------------------------------- Création de la série chronologique -----------------------------

# Notons qu'entre temps, la base de données a été mis à jour désormais 
# elle part de 2020-03-02 au 2020-06-28

# Importons la nouvelle base de données
covid19_SN <- read.csv("confirmes28juin.csv", sep = ",", header = T)

# Mettre la variable date au format : classe 'date' avec 'as.POSIXct()' = classe 'date'
# puis ordonner la base de données dans l'ordre croissant 
covid19_SN <- covid19_SN %>%
  mutate(date = as.POSIXct(date)) %>%
  arrange(date)

# Vérification de la classe des variables
str(covid19_SN)


#**** Constat :
# Notons que notre série présente des données quotidiennnes du 2020-03-02 au 2020-06-02
# Donc on choisit de créer deux séries de fréquence 7 jours (par semaine)
# Dans le but de mieux voir les tendances et saisonnalités

#--- Avant la série chronologique,

# Création des indices de dates à frequence weekly (hebdomadaire) 
inds <- seq(as.Date("2020-03-02"), as.Date("2020-06-28"), by = "week")

# Début des date: Création du début des dates 
# (il s'agit de notre première semaine d'observation) 
begin <- as.numeric(format(inds[1], "%W"))


#************** Création propre de la Série chronologique ('Nbre de cas confirmés')
#************************* de frequence hebdomadaire (weekly) *********************
ts_covid19 <- ts(covid19_SN[ ,3], start = begin, frequency = 7)
head(force(ts_covid19), 10)





#----------------------------------- Etude tendancielle et saisonnière ---------------------------

# Visualisation des chronogrammes avec 'autoplot()'

#----------------- Etude des tendances
autoplot(ts_covid19, col = "brown") +
  ggtitle("COVID-19 : Nombre de cas confirmés au Sénégal\n du 02 Mars au 28 Juin 2020") +
  xlab("Semaines") +
  ylab("Nbre de cas confirmés") +
  theme_minimal()

# Le code suivant permet de rendre interractive le graphe ci dessus.
g <- autoplot(ts_covid19, col = "brown") +
  ggtitle("COVID-19 : Nombre de cas confirmés au Sénégal\n du 02 Mars au 28 Juin 2020") +
  xlab("Semaines") +
  ylab("Nbre de cas confirmés") +
  theme_minimal()
# Visulisation Graphique interractif
ggplotly(g)



#------------- Etude de la saisonnalité

# Le premier diagramme saisonnier 
ggseasonplot(ts_covid19, year.labels = T, year.labels.left = T) +
  ggtitle("Diagramme saisonnier") +
  theme_minimal()


# Diagramme saisonnier en coordonnées polaires 
ggseasonplot(ts_covid19, polar = T) +
  guides(colour = guide_legend(title="Weeks")) +
  ggtitle("Diagramme saisonnier en coordonnées polaires") +
  theme_minimal()


# Diagramme de la sous-série saisonnière
ggsubseriesplot(ts_covid19) +
  theme_minimal() 



# Deuxième diagramme de la sous-série saisonnière
gglagplot(ts_covid19) +
  theme_minimal()


# Corrélogramme ou ACF (interractif)
g2 <- ggAcf(ts_covid19, lag = 35) +
  xlab("Les décalages") +
  ggtitle("Corrélogramme") +
  theme_minimal()

ggplotly(g2)




#---------------------------------- Les méthodes de prévision de référence ----------------------------- 

# Actualisons la base de données qui part de 02-03-2020 à 14-07-2020

# Importons la nouvelle base de données
covid19_SN <- read.csv("confirmes_14Juillet.csv", sep = ",", header = T)

# Mettre la variable date au format : classe 'date' avec 'as.POSIXct()' = classe 'date'
# puis ordonner la base de données dans l'ordre croissant 
covid19_SN <- covid19_SN %>%
  mutate(date = as.POSIXct(date)) %>%
  arrange(date)

# Il faut recréer à nouveau la série chronologique avec la nouvelle base de données

# Notons que notre série présente des données quotidiennnes du 2020-03-02 au 2020-07-14
# Donc on choisit de créer deux séries de fréquence 7 jours (par semaine)
# Dans le but de mieux voir les tendances et saisonnalités

#*** Série chronologique de frequence hebdomadaire (weekly) ***
# Création du début des dates (il s'agit de notre première semaine d'observation) 
# frequence weekly
inds <- seq(as.Date("2020-03-02"), as.Date("2020-07-14"), by = "week")
# début des date
begin <- as.numeric(format(inds[1], "%W"))

# séries chronologiques
ts_covid19 <- ts(covid19_SN[ ,3], start = begin, frequency = 7)
head(force(ts_covid19), 10)


#---------- Visualisation du chronogramme
# Avec 'autoplot()'
g <- autoplot(ts_covid19, col = "blue") +
  ggtitle("COVID-19 : Nombre de cas confirmés au Sénégal\n du 02 Mars au 28 Juin 2020") +
  xlab("Semaines") +
  ylab("Nbre de cas confirmés") +
  theme_minimal()
# Visulisation Graphique interractif
ggplotly(g)


#--------------- Appliquons les trois premières méthodes de prévision ------------
#-------------------------- sur nos données hebdomadaires -----------------------

# Données d'entrainement de la 11e semaine à la 25e semaine 
# (On a éliminer les valuers nulles de la série)
ts_covid2 <- window(ts_covid19, start = c(11, 1), end = c(25, 1))
# Apperçu de la nouvelle séries
head(ts_covid2)

# Visualisation des méthodes de prévision
autoplot(ts_covid2) +
  autolayer(meanf(ts_covid2, h=14),
            series="Mean", PI=FALSE) +
  autolayer(naive(ts_covid2, h=14),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(ts_covid2, h=14),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Les méthodes de prévision de référence") +
  xlab("semaines") + ylab("Nbre de cas confirmés") +
  guides(colour=guide_legend(title="Prévision :")) +
  theme_minimal() +
  theme(legend.position = "bottom")




#------------------------------- Transformations et ajustements des données ---------------------------

#---------------------------------------- Transformation Box-Cox -------------------------------------

# Choix de la valeur de lambda par R
ts_covid19 <- window(ts_covid19, start = c(11, 1))
(lambda <- BoxCox.lambda(ts_covid19))
#> [1] 0.2424546

# Visualisation du chronogramme des données ajustées
autoplot(BoxCox(ts_covid19, lambda)) +
  xlab("") +
  ylab("") +
  ggtitle("Données ajustées selon la transformation Box-Cox\n de paramètre lambda") +
  theme_minimal()

# Visualisons la série ajustée et la série non ajustée
# Regroupons les deux séries sur un même graphe
ts_combined <- cbind(ajusted_ts = BoxCox(ts_covid19, lambda), 
                     no_ajusted_ts = ts_covid19)

# Vérifions la classe de l'objet créé
class(ts_combined) # calss : 'ts' 

# Les chronogrammes des deux séries
autoplot(ts_combined[, 1:2], facets = T) +
  xlab("semaines") +
  ylab("Nbre de cas confirmés") +
  ggtitle("Série chronologique ajustée (en haut) et \n série chronologique non ajustée (en bas)") +
  theme_minimal()




#--------------------------------- Transformation avec ajustement du biais ----------------------------

# Données Ajustées en fonction du biais 
# avec la méthode saisonnière naive
fc <- snaive(ts_covid2, lambda=0, h=14, level=80) # ajustement simple
fc2 <- snaive(ts_covid2, lambda=0, h=14, level=80, # ajustement en fonction du biais
              biasadj=TRUE)

# Comparaison des prévisions entre les données ajustées simplement
# et celles ajustées en fonction du biais.
autoplot(ts_covid2) +
  autolayer(fc, series="Simple  rétrotransformation") +
  autolayer(fc2, series="Biais adjusté", PI=FALSE) +
  xlab("") +
  ylab("") +
  ggtitle("Prévisions des données ajustées simplement \n et celles ajustées en fonction du biais") +
  guides(colour=guide_legend(title="Prévision")) +
  theme_minimal() +
  theme(legend.position = "bottom")




#------------------------------------- Diagnostics résiduels ---------------------------------

# La commande suivante permet de générer plusieurs diagrammes :
# 1. Le diagramme temporel des résidus
# 2. L'Histogramme des résidus 
# 3. Le diagramme d'Autocorrélation (ACF) des résidus
checkresiduals(fc2) + # résidu de la méthode saisonnière naive
  theme_minimal()     # avec ajustement du biais


#---------------------------------- Tests de Portmanteau pour l'autocorrélation -------------------------
#------------------------------- Voir l'article 'SNCovi19' pour plus d'explication ----------------------
# Résidus
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






#----------------------------------- Évaluation de l'exactitude des prévisions --------------------------

#--------- Découpage des données en données d'Entrainement ('train') et
#---------------------------------- Données de Test

# Etalage du nombre de semaines
# etendu = 28.1 - 11 = 17.1
# 70% * etendu = 17.1 * 0.8 = 16.68 
# Donc, nos données de 'train' irrons jusqu'à 
# debut = 11 + 70% * etendu = 26e semaine, 1e jour.
tsTrain <- window(ts_covid19, end = c(26, 1))   
tsTest <- window(ts_covid19, start = c(27, 1)) 

# Création des modèles de référence
# A partir du jeux d'entrainement "tsTrain"
ts_covidFit1 <- meanf(tsTrain, h = 14)
ts_covidFit2 <- rwf(tsTrain, h = 14)
ts_covidFit3 <- snaive(tsTrain, h = 14)


# Visualisation de la prévision
autoplot(ts_covid19) +
  autolayer(ts_covidFit1, series="Mean", PI=FALSE) +
  autolayer(ts_covidFit2, series="Naïve", PI=FALSE) +
  autolayer(ts_covidFit3, series="Seasonal naïve", PI=FALSE) +
  xlab("semaines") + ylab("Nbre de cas confirmés") +
  ggtitle("Prévision de nombre de cas confirmés du Covid19 \n en utilisant les données hebdomadaires") +
  guides(colour=guide_legend(title="Prévision")) +
  theme_minimal() +
  theme(legend.position = "bottom")





#----------------------------------- Evaluation de l'exactitude des prévisions -----------------------------
accuracy(ts_covidFit1, tsTest)    # Evaluation méthode moyenne
accuracy(ts_covidFit2, tsTest)    # Evaluation méthode naive
accuracy(ts_covidFit3, tsTest)    # Evaluation méthode naive saisonnière



#-------------------------------------- Intervalle de prédiction ------------------------------------------
# Pour voir les intervalles de prédiction
# Il suffit de faire la cmd suivante
print(fc)
print(fc2)



#******************************************* FIN DE LA PHASE 1 DE L'ETUDE *****************************************
