# author: A. Baron, C. Bres, E. Desnyder
#Machine Learning for Biology Project



#PREPARATION DE L'ESPACE DE TRAVAIL----

library (tidyverse)
library(vegan)
set.seed(123)

#IMPORTATION DES DONNEES----
##Functionnal groups----
file_fg <- data.frame(read_csv("fg_observed_averyll.csv"))
row.names(file_fg)<-file_fg[,1]#on renomme les indices par la première colonne

##Environmental parameters----
file_env<-data.frame(read_csv("13_variables_averyll.csv"))
row.names(file_env)<-file_env[,1]

#SELECTION D'UN SOUS ENSEMBLE POUR L'EVALUATION FINALE----
test_final_fg<-file_fg %>% sample_frac(.25)#on prend 25% du jeu de donnees pr le test final
sample_final<-test_final_fg[,1]#nom de l'echantillon = col1 du tab
fg_observed<-setdiff(file_fg,test_final_fg) #trouve toutes les lignes de file_fg (3/4 restants) qui ne sont pas dans le sous-ensemble de test : pour l'entrainement

nrow(test_final_fg)*4#le sous-ensemble de test contient 1/4 des donnees du dataset initial
nrow(test_final_fg)+nrow(fg_observed) == nrow(file_fg)#somme des TRAIN+TEST = nb d'obs du dataset

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

test<-RDA$CCA$u[,1:3] #extrait les coordonnees de la RDA

#display = sites ou  species (functionnal groups)
#choices= axis
#scaling ="none", "sites", "species", or "symmetric", which correspond to the values 0, 1, 2, and 3 respectively
RDA_coordinates<-scores(RDA, display = "sites", choices = 1:3, scaling = 2)#sp?
RDA_coordinates
scores(RDA, display = "sites", choices = 1:3, scaling = "sp")#retrouve la meme chose que ligne precedente : score sur l'axe par sample
scores(RDA, display = "species", choices = 1:3, scaling = 3)#inertie sur l'axe par variable Y : groupe fonctionnel
plot(RDA, display = c("species", "reg"))#inertie sur l'axe par variable Y : groupe fonctionnel
plot(RDA, display = c("reg", "sp"))#biplot
plot(RDA, display = "wa")#points
# plot(RDA, display = "cn")#var X

##Merge avec les variables environementales----

RDA_coordinates<-merge(RDA_coordinates,env_observed,by=0)


#MONTE-CARLO----
#Rq: pour l'instant pas de boucle je veux voir si ça fonctionne déjà sans



#création de deux groupes pour l'entrainement(train) et la validation croisée(val)

train_fg<-RDA_coordinates %>% sample_frac(2/3) #on prend les coordonnees de 2/3 des data d'entrainement (96li)
val_fg<-setdiff(RDA_coordinates,train_fg)#le 1/3 restant est utilise pour la validation(32li)


#modèle linéaire -----

#Axe 1
linear_model1<-lm( RDA1 ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, data=train_fg)
summary(linear_model1)#significativite : RelEM***, mat(.), intercxept(*)
# forest coeff = 0.1
RsquareAdj(linear_model1)#42% de variance explique par le modele en R² ajuste (46% sinon, coherent avec la rda de l'autre script, cf explo-data-fambact_averyll.R)
#Axe 2
linear_model2<-lm( RDA2 ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, data=train_fg)
summary(linear_model2)# sign n: cn (.), estimate (*)
#gd coeff : Ca = 0.35, NPP=0.32, map=-0.31
#variance expliquee par le modele : 8.5% av R² ajuste (20% sinon)

#Axe 3
linear_model3<-lm( RDA3 ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, data=train_fg)
summary(linear_model3)#Ph, Conifer (*), K (.)
#coeff : K=0.55; conifer = 0.37
RsquareAdj(linear_model3)#R² adj = 8.8% (27% variance sinon)

#Ajout méthodes complémentaires type lasso etc. (optionnel)
linear_model_qr <-lm( RDA1 ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, data=train_fg, method = "model.frame")


#Autre ?----
#fonction pour projeter, rdaout est le nom du modèle de rda
fitted_values_predict <- predict(rdaout, newdata=G, type="response")
