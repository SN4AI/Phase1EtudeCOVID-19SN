#********************************** Cartographie sur R *******************************

#####################################################
# Projet : Analyse COVID-19 au Sénégal: partie 1    #
# Auteur : Ousmane Sy Bodian,                       #
# Profil : Ingénieur statisticien, Data scientist   # 
# Date début : 13/06/2020                           #
# Date fin :   20/07/2020                           #
#####################################################



#----------------------- Libreries requises dans ce projet ----------------------

# Chargement des packages pour les données geographiques
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



#---------------------- Importation de la base de données -----------------------

# La base de données est mis à jours tous jours
# Celle que nous affichons ici a été télécharger le 12-06-2020

# Jeux de données : Nbre de cas confirmés du covid selon la région
regions <- read.csv("regions_cas.csv", header = T, sep = ",")




#----------------------- Reconstitution de la base de données--------------------

# Recodage des noms de régions en format 'nom de famille'
# Càd la première lettre en Majuscule et le reste en minuscule
names(regions) <- str_to_title(names(regions))

# Recodage des noms de régions mals édités
names(regions)[7] <- "Kédougou" 
names(regions)[11] <- "Saint-Louis"
names(regions)[12] <- "Sédhiou"
names(regions)[14] <- "Thiès"

# Objectif dans cette partie est de faire correspondre notre base de données
# et le fchier 'shape.file' ci-dessous. En effet, le fichier 'shape.file'
# est constitué par les 11 anciennes régions administratives du Sénégal
# Donc, on transforme la base de données en 11 régions au lieux des 14 actuelles.
regions <- regions %>%
  mutate(Date = as.POSIXct(Date),
         Kolda = Kolda + Sédhiou, 
         Kaolack = Kaolack + Kaffrine,
         Tambacounda = Tambacounda + Kédougou) %>%
  select(-c("Sédhiou", "Kaffrine", "Kédougou")) %>%
  arrange(Date)


# Recueillons de la base de données 
# le Nre cumulés de cas 'confirmés' du covid
confirmes <- regions[regions$Date == max(regions$Date),][-1]
# Convertissons ce vecteur en valeur entières
confirmes2 <- as.integer(confirmes)

# Regroupons le vecteur 'Regions' et ' confirmes' dans une seule base en colonnes
covid19_region <- data.frame(Regions = as.character(names(confirmes)), confirmes = confirmes2)






#--------------------------- Importation du 'fichier shapefile -----------------------

senegal <- st_read("SEN-level_1.shp", layer = "SEN-level_1", stringsAsFactors = F)




#--------------------------- Reconstitution du 'fichier shapefile' ----------------------

# Recodage des noms de région mal écrits
# Thiès
# Attention !!! : la région de Thiès contient des caractères spéciaux
# Utilisons la fonction suivante pour l'éliminer
# En effet, mymap contient des caractères accentués 
# Note : gsub permet de substituer des caractères spéciaux avec d'autres

# Role de la fonction 'Unaccent': 
# Par défaut, les caractères sont encodés en "UTF8"
# gsub permet de substituer les caractères spéciaux en " " et
# la fonction 'iconv()' permet de convertir les caractères en codage
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


# A présent que les caractères spéciaux ont été écartés, 
# nous pouvons Recodage proprement la variable 'thiès'
senegal$ID[senegal$ID=="ThiSs"] ="Thiès"
senegal$CAPTION[senegal$CAPTION=="ThiSs"] ="Thiès"


#********** Fusion entre les données Géographiques 'senegal' 
# et le jeux de données 'covid19_region'
MapDataCovid <- inner_join(covid19_region, senegal, by = c("Regions" = "CAPTION")) %>%
  select(Regions, ID, confirmes, geometry)

# Attention!!! car les commandes qui générent la carte
# ne prennent que les objets 'sf'
# La commande suivante, nous permet de convertir la base complète
# 'mapdata_covid' en objet de calsse 'sf'
MapDataCovid <- st_as_sf(MapDataCovid)





# A présent tout est OK pour Visualiser les carte



#----------------------------------- Visualisation des Cartes --------------------------------

#------------------************* Les cartes à ronds proportionnels ************--------------

#------- Ajoutons le Nbre de cas confirmés au nom des régions
# Région + Nbre de cas
MapDataCovid <- MapDataCovid %>%
  mutate(char1 = as.character(ID),
         char2 = as.character(confirmes),
         ID2 = paste(char1, char2, sep = "\n"))


#*****------ Première Catre avec 'ggplot' -------

ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes)) +
  stat_sf_coordinates(aes(size = confirmes, fill = confirmes), color = "red", shape = 20, alpha = 0.6) +
  scale_fill_gradient2(name = "Confirmés",
                       low = "lightcyan", mid = "slategray1", high = "darkred") +
  scale_size_area(name = "Confirmés", max_size = 25) +
  ggtitle("Nombre de cas Confirmés au Sénégal\n jusqu'à ce jour du 12 Juin 2020") +
  theme_minimal() +
  geom_sf_text(aes(label = ID2),vjust=-0.5,
               check_overlap=TRUE,fontface="italic",colour="black") +
  theme(axis.title.x = element_blank(),   # Supprimer l'étiquette de l'axe des X
        axis.title.y = element_blank(),   # Supprimer l'étiquette de l'axe des Y
        axis.text = element_blank(),      # Supprimer les axes des X et Y
        legend.position = "bottom")       # Position de la légende en bas







#*****-------------------------- Deuxième Catre avec 'tm_shape()' --------------------------------
 
#  au Sénégal\n jusqu'à ce jour du 12 Juin 2020

#*******------- Carte Interractive 
breaks = c(0, 1, 2, 4, 5, 10, 20, 80) * 50
tm_shape(MapDataCovid) + 
  tm_polygons("confirmes", id = "ID2",
              title="Nombre de cas Confirmés", 
              breaks = breaks) +
  tm_text("ID2", scale = 1.3, shadow = T) +
  tm_bubbles(size = "confirmes", col = "red", alpha = .5, scale = 5, shape = 20)

# Rendre interractive la Carte
tmap_mode("view")
tmap_last()

#******--------- Carte interractive
g <- tm_shape(MapDataCovid) + 
  tm_polygons("confirmes", id = "ID2",
              title="Nombre de cas Confirmés \n au Sénégal jusqu'à ce jour\n du 12 Juin 2020", 
              breaks = breaks) +
  tm_text("ID2", scale = 1.3, shadow = T) +
  tm_bubbles(size = "confirmes", col = "red", alpha = .5, scale = 5, shape = 20)

# Rendre interractive la Carte
tmap_mode("view")
g 


#******--------- position de la légende en bas 
tm_shape(MapDataCovid) + 
  tm_polygons("confirmes", id = "ID2",  title="Nombre de cas \n Confirmés", breaks = breaks) +
  tm_text("ID2", scale = 1.3, shadow = T) +
  tm_bubbles(size = "confirmes", col = "red", alpha = .5, scale = 5, shape = 20) +
  tm_layout(legend.position = c("left", "bottom")) 


















