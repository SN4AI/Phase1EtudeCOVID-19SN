#********************************** Cartographie sur R *******************************

#####################################################
# Projet : Analyse COVID-19 au S�n�gal: partie 1    #
# Auteur : Ousmane Sy Bodian,                       #
# Profil : Ing�nieur statisticien, Data scientist   # 
# Date d�but : 13/06/2020                           #
# Date fin :   20/07/2020                           #
#####################################################



#----------------------- Libreries requises dans ce projet ----------------------

# Chargement des packages pour les donn�es geographiques
library(sf)
library(raster)
library(tidyverse)
# Les packages de visualisation
library(tmap) # for static and interactive maps
library(viridis)
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(shiny)   # for web applications



#---------------------- Importation de la base de donn�es -----------------------

# La base de donn�es est mis � jours tous jours
# Celle que nous affichons ici a �t� t�l�charger le 12-06-2020

# Jeux de donn�es : Nbre de cas confirm�s du covid selon la r�gion
regions <- read.csv("regions_cas.csv", header = T, sep = ",")




#----------------------- Reconstitution de la base de donn�es--------------------

# Recodage des noms de r�gions en format 'nom de famille'
# C�d la premi�re lettre en Majuscule et le reste en minuscule
names(regions) <- str_to_title(names(regions))

# Recodage des noms de r�gions mals �dit�s
names(regions)[7] <- "K�dougou" 
names(regions)[11] <- "Saint-Louis"
names(regions)[12] <- "S�dhiou"
names(regions)[14] <- "Thi�s"

# Objectif dans cette partie est de faire correspondre notre base de donn�es
# et le fchier 'shape.file' ci-dessous. En effet, le fichier 'shape.file'
# est constitu� par les 11 anciennes r�gions administratives du S�n�gal
# Donc, on transforme la base de donn�es en 11 r�gions au lieux des 14 actuelles.
regions <- regions %>%
  mutate(Date = as.POSIXct(Date),
         Kolda = Kolda + S�dhiou, 
         Kaolack = Kaolack + Kaffrine,
         Tambacounda = Tambacounda + K�dougou) %>%
  select(-c("S�dhiou", "Kaffrine", "K�dougou")) %>%
  arrange(Date)


# Recueillons de la base de donn�es 
# le Nre cumul�s de cas 'confirm�s' du covid
confirmes <- regions[regions$Date == max(regions$Date),][-1]
# Convertissons ce vecteur en valeur enti�res
confirmes2 <- as.integer(confirmes)

# Regroupons le vecteur 'Regions' et ' confirmes' dans une seule base en colonnes
covid19_region <- data.frame(Regions = as.character(names(confirmes)), confirmes = confirmes2)






#--------------------------- Importation du 'fichier shapefile -----------------------

senegal <- st_read("SEN-level_1.shp", layer = "SEN-level_1", stringsAsFactors = F)




#--------------------------- Reconstitution du 'fichier shapefile' ----------------------

# Recodage des noms de r�gion mal �crits
# Thi�s
# Attention !!! : la r�gion de Thi�s contient des caract�res sp�ciaux
# Utilisons la fonction suivante pour l'�liminer
# En effet, mymap contient des caract�res accentu�s 
# Note : gsub permet de substituer des caract�res sp�ciaux avec d'autres

# Role de la fonction 'Unaccent': 
# Par d�faut, les caract�res sont encod�s en "UTF8"
# gsub permet de substituer les caract�res sp�ciaux en " " et
# la fonction 'iconv()' permet de convertir les caract�res en codage
# ASCII

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}


# Appliquons la fonction 'Unaccent' sur les vecteurs colonnes
# ID et CAPTION
senegal$ID <- sapply(senegal$ID, Unaccent)
senegal$CAPTION <- sapply(senegal$CAPTION, Unaccent)


# A pr�sent que les caract�res sp�ciaux ont �t� �cart�s, 
# nous pouvons Recodage proprement la variable 'thi�s'
senegal$ID[senegal$ID=="ThiSs"] ="Thi�s"
senegal$CAPTION[senegal$CAPTION=="ThiSs"] ="Thi�s"


#********** Fusion entre les donn�es G�ographiques 'senegal' 
# et le jeux de donn�es 'covid19_region'
MapDataCovid <- inner_join(covid19_region, senegal, by = c("Regions" = "CAPTION")) %>%
  select(Regions, ID, confirmes, geometry)

# Attention!!! car les commandes qui g�n�rent la carte
# ne prennent que les objets 'sf'
# La commande suivante, nous permet de convertir la base compl�te
# 'mapdata_covid' en objet de calsse 'sf'
MapDataCovid <- st_as_sf(MapDataCovid)





# A pr�sent tout est OK pour Visualiser les carte



#----------------------------------- Visualisation des Cartes --------------------------------

#------------------************* Les cartes � ronds proportionnels ************--------------

#------- Ajoutons le Nbre de cas confirm�s au nom des r�gions
# R�gion + Nbre de cas
MapDataCovid <- MapDataCovid %>%
  mutate(char1 = as.character(ID),
         char2 = as.character(confirmes),
         ID2 = paste(char1, char2, sep = "\n"))


#*****------ Premi�re Catre avec 'ggplot' -------

ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes)) +
  stat_sf_coordinates(aes(size = confirmes, fill = confirmes), color = "red", shape = 20, alpha = 0.6) +
  scale_fill_gradient2(name = "Confirm�s",
                       low = "lightcyan", mid = "slategray1", high = "darkred") +
  scale_size_area(name = "Confirm�s", max_size = 25) +
  ggtitle("Nombre de cas Confirm�s au S�n�gal\n jusqu'� ce jour du 12 Juin 2020") +
  theme_minimal() +
  geom_sf_text(aes(label = ID2),vjust=-0.5,
               check_overlap=TRUE,fontface="italic",colour="black") +
  theme(axis.title.x = element_blank(),   # Supprimer l'�tiquette de l'axe des X
        axis.title.y = element_blank(),   # Supprimer l'�tiquette de l'axe des Y
        axis.text = element_blank(),      # Supprimer les axes des X et Y
        legend.position = "bottom")       # Position de la l�gende en bas







#*****-------------------------- Deuxi�me Catre avec 'tm_shape()' --------------------------------
 
#  au S�n�gal\n jusqu'� ce jour du 12 Juin 2020

#*******------- Carte Interractive 
breaks = c(0, 1, 2, 4, 5, 10, 20, 80) * 50
tm_shape(MapDataCovid) + 
  tm_polygons("confirmes", id = "ID2",
              title="Nombre de cas Confirm�s", 
              breaks = breaks) +
  tm_text("ID2", scale = 1.3, shadow = T) +
  tm_bubbles(size = "confirmes", col = "red", alpha = .5, scale = 5, shape = 20)

# Rendre interractive la Carte
tmap_mode("view")
tmap_last()

#******--------- Carte interractive
g <- tm_shape(MapDataCovid) + 
  tm_polygons("confirmes", id = "ID2",
              title="Nombre de cas Confirm�s \n au S�n�gal jusqu'� ce jour\n du 12 Juin 2020", 
              breaks = breaks) +
  tm_text("ID2", scale = 1.3, shadow = T) +
  tm_bubbles(size = "confirmes", col = "red", alpha = .5, scale = 5, shape = 20)

# Rendre interractive la Carte
tmap_mode("view")
g 


#******--------- position de la l�gende en bas 
tm_shape(MapDataCovid) + 
  tm_polygons("confirmes", id = "ID2",  title="Nombre de cas \n Confirm�s", breaks = breaks) +
  tm_text("ID2", scale = 1.3, shadow = T) +
  tm_bubbles(size = "confirmes", col = "red", alpha = .5, scale = 5, shape = 20) +
  tm_layout(legend.position = c("left", "bottom")) 


















