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

nrow(test_final_fg)*4#le sous-ensemble de test contient 1/4 des donnees du data n = 19.248
# summary(cv(m.auto, k = "loo", method = "naive"))         
         ##comparer cette prediction avec les obs reelles ----
         
#Autre ?----
#fonction pour projeter, rdaout est le nom du modèle de rda----
fitted_values_predict <- predict(rdaout, newdata=G, type="response")

#test avorted pr ajouter nvelles donnees neon en plus de averyll
fitted_values_predict_2 <- predict (RDA, newdata = fg_champi_neon_RDA1, type = "response")
plot(fitted_values_predict_2)
neon_13var
fc_grp_neon
