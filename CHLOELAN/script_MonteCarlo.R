# author: A. Baron, C. Bres, E. Desnyder
#Machine Learning for Biology Project



#PREPARATION DE L'ESPACE DE TRAVAIL----

library (tidyverse)
library(vegan)
set.seed(123)

#IMPORTATION DES DONEES----
##Functionnal groups----
file_fg <- data.frame(read_csv("fg_observed_averyll.csv"))
row.names(file_fg)<-file_fg[,1]#on renomme les indices par la première colonne

##Environmental parameters----
file_env<-data.frame(read_csv("13_variables.csv"))
row.names(file_env)<-file_env[,1]

#SELECTION D'UN SOUS ENSEMBLE POUR L'EVALUATION FINALE----
test_final_fg<-file_fg %>% sample_frac(.25)
sample_final<-test_final_fg[,1]
fg_observed<-setdiff(file_fg,test_final_fg)

##Nettoyage des données----
test_final_fg <- select(test_final_fg,-1)
fg_observed <- select(fg_observed,-1)
file_env<-select(file_env,-1)

##Sélection du sous ensemble pour les paramètres environnementaux----
test_final_env<- file_env %>% filter(row.names(file_env) %in% sample_final)
env_observed<-setdiff(file_env,test_final_env)

#RDA----
RDA <- rda(fg_observed ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, 
                         data = env_observed)
##Récupération des axes de la RDA----

test<-RDA$CCA$u[,1:3]

#display = sites ou  species (functionnal groups)
#choices= axis
#scaling ="none", "sites", "species", or "symmetric", which correspond to the values 0, 1, 2, and 3 respectively
RDA_coordinates<-scores(RDA, display = "sites", choices = 1:3, scaling = 2)

##Merge avec les variables environementales----

RDA_coordinates<-merge(RDA_coordinates,env_observed,by=0)


#MONTE-CARLO----
#Rq: pour l'instant pas de boucle je veux voir si ça fonctionne déjà sans



#création de deux groupes pour l'entrainement(train) et la validation croisée(val)

train_fg<-RDA_coordinates %>% sample_frac(2/3)
val_fg<-setdiff(RDA_coordinates,train_fg)


#modèle linéaire

#Axe 1
linear_model1<-lm( RDA1 ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, data=train_fg)
summary(linear_model1)

#Axe 2
linear_model2<-lm( RDA2 ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, data=train_fg)
summary(linear_model2)

#Axe 3
linear_model3<-lm( RDA3 ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, data=train_fg)
summary(linear_model3)

#Ajout méthodes complémentaires type lasso etc. (optionnel)

