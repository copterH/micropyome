"
author: A. Baron, C. Bres, E. Desnyder
Machine Learning for Biology Project

"

#PREPARATION DE L'ESPACE DE TRAVAIL----

# Cleaning environment

rm(list=ls())

# Importing libraries

library(tidyverse) # data manipulation
library(vegan) # RDA
library(Metrics)
library(cv)
library(ape)

set.seed(123)


#DATA IMPORTATION ----

##Functionnal groups----
file_fg <- data.frame(read_csv("fg_observed_averyll.csv", show_col_types = FALSE))
row.names(file_fg)<-file_fg[,1]#on renomme les indices par la première colonne

##Environmental parameters----
file_env<-data.frame(read_csv("13_variables_averyll.csv", show_col_types = FALSE))
row.names(file_env)<-file_env[,1]







#SELECTION D'UN SOUS ENSEMBLE POUR L'EVALUATION FINALE----
test_final_fg<-file_fg %>% sample_frac(.25)#on prend 25% du jeu de donnees pr le test final
sample_final<-test_final_fg[,1]#nom de l'echantillon = col1 du tab
fg_observed<-setdiff(file_fg,test_final_fg) #trouve toutes les lignes de file_fg (3/4 restants) qui ne sont pas dans le sous-ensemble de test : pour l'entrainement

nrow(test_final_fg)*4#le sous-ensemble de test contient 1/4 des donnees du dataset initial n = 19.248
nrow(test_final_fg)+nrow(fg_observed) == nrow(file_fg)#somme des TRAIN+TEST = nb d'obs du dataset
# summary(cv(m.auto, k = "loo", method = "naive"))         
 

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

RDA_coordinates<-data.frame(merge(RDA_coordinates,env_observed,by=0))%>% as.data.frame()









#BOUCLE MONTE CARLO ----
B <- 100

# créer des dataframe qui enregistrent tous les lm

m <- matrix(ncol = 0, nrow = B)
full_lm1 <- data.frame(m)
full_lm2 <- data.frame(m)
full_lm3 <- data.frame(m)

# créer un dataframe qui enregistre les MSE de chaque modèle testé

n <- matrix(ncol = 0, nrow = 3)
scores <- data.frame(n)


for (i in 1:B){
  
  ## Initialisation ----
  
  # création de deux groupes pour l'entrainement(train) et la validation croisée(val)
  
  train_fg <- RDA_coordinates %>% sample_frac(2/3) #on prend les coordonnees de 2/3 des data d'entrainement (96li)
  val_fg<-setdiff(RDA_coordinates,train_fg)#le 1/3 restant est utilise pour la validation(32li)
  
  
  ## Modèle linéaire -----
  
  #Axe 1
  linear_model1<-lm( RDA1 ~ pC + cn + pH + NPP + map + mat + forest + conifer +
                       relEM +P + K + Ca + Mg, data=train_fg)
  #Axe 2
  linear_model2<-lm( RDA2 ~ pC + cn + pH + NPP + map + mat + forest + conifer +
                       relEM +P + K + Ca + Mg, data=train_fg)
  #Axe 3
  linear_model3<-lm( RDA3 ~ pC + cn + pH + NPP + map + mat + forest + conifer +
                       relEM +P + K + Ca + Mg, data=train_fg) 
  
  ##Extraction des coefficients et calcul des Y^ ----
  coef<-data.frame("RDA1_coef"=linear_model1$coefficients, 
                   "RDA2_coef"=linear_model2$coefficients,
                   "RDA3_coef"=linear_model3$coefficients) %>%
    t() %>% as.data.frame()
  
  cols<-intersect(names(coef), names(val_fg)) #select all factors coefficients
  
  ###Y^  Axe 1 ----
  y1_est<-val_fg %>% 
    
    #1. Multiplication of coefficient * values
    mutate(
      across( #across : apply function to multiple columns
        all_of(cols), #were to apply it
        ~ .x * as.numeric(coef[['RDA1_coef',cur_column()]]))) %>% #which function. Here multiply the column by the coefficient factor
    
    #2. Sum of coefficient and add intercept
    mutate(y_est=rowSums(across(all_of(cols)))) %>%
    mutate(y_est, y_est=y_est + as.numeric(coef[['RDA1_coef', '(Intercept)']])) %>%
    
    #3. Select to have only RDA1, y1_est
    select(Row.names,RDA1,y_est)
  
  
  ###Y^  Axe 2 ----
  y2_est<-val_fg %>% 
    
    #1. Multiplication of coefficient * values
    mutate(
      across( #across : apply function to multiple columns
        all_of(cols), #were to apply it
        ~ .x * as.numeric(coef[['RDA2_coef',cur_column()]]))) %>% #which function. Here multiply the column by the coefficient factor
    
    #2. Sum of coefficient and add intercept
    mutate(y_est=rowSums(across(all_of(cols)))) %>%
    mutate(y_est, y_est=y_est + as.numeric(coef[['RDA2_coef', '(Intercept)']])) %>%
    
    #3. Select to have only RDA1, y1_est
    select(Row.names,RDA2,y_est)
  
  
  ###Y^  Axe 3 ----
  y3_est<-val_fg %>% 
    
    #1. Multiplication of coefficient * values
    mutate(
      across( #across : apply function to multiple columns
        all_of(cols), #were to apply it
        ~ .x * as.numeric(coef[['RDA3_coef',cur_column()]]))) %>% #which function. Here multiply the column by the coefficient factor
    
    #2. Sum of coefficient and add intercept
    mutate(y_est=rowSums(across(all_of(cols)))) %>%
    mutate(y_est, y_est=y_est + as.numeric(coef[['RDA3_coef', '(Intercept)']])) %>%
    
    #3. Select to have only RDA1, y1_est
    select(Row.names,RDA3,y_est)
  
  
  
  ## Calcul de la distance (MSE) ----
  
  MSE1<- mean((y1_est$RDA1 - y1_est$y_est)^2)
  MSE2<- mean((y2_est$RDA2 - y2_est$y_est)^2)
  MSE3<- mean((y3_est$RDA3 - y3_est$y_est)^2)

  
  ## Enregistrement des coefficients et des MSE ----
  
  ## Coefficients 
  
  full_lm1 <- rbind(full_lm1, linear_model1$coefficients)
  full_lm2 <- rbind(full_lm2, linear_model2$coefficients)
  full_lm3 <- rbind(full_lm3, linear_model3$coefficients)
  
  ## MSE
  
  scores <- rbind(scores, c(MSE1, MSE2, MSE3))
 
  
   
}

#End of loop

colnames(full_lm1) <- c("pC", "cn", "pH", "NPP", "map", "mat", "forest", "conifer",
                        "relEM", "P", "K", "Ca", "Mg")
colnames(full_lm2) <- c("pC", "cn", "pH", "NPP", "map", "mat", "forest", "conifer",
                        "relEM", "P", "K", "Ca", "Mg")
colnames(full_lm3) <- c("pC", "cn", "pH", "NPP", "map", "mat", "forest", "conifer",
                        "relEM", "P", "K", "Ca", "Mg")

colnames(scores) <- c("score1", "score2", "score3")


head(full_lm1)
head(full_lm2)
head(full_lm3)

head(scores)



#Creation of the final model : Mean of all coefficient.


# Calcul of RMSE ----

RMSE1 <- sqrt(sum(scores$score1)/B)
RMSE2 <- sqrt(sum(scores$score2)/B)
RMSE3 <- sqrt(sum(scores$score3)/B)

print(RMSE1)
print(RMSE2)

print(RMSE3)
##  Compute MAE on the test set inside the training set ------
val_fg #validation set inside training set
### MAE 1 -----
Y_valid_est1<-predict (linear_model1, newdata = val_fg )
Y_valid_est1 #predicted data for validation set
val_fg$RDA1 #observed values for validation set
score_mae_1_valid_fc <- mae(val_fg$RDA1, Y_valid_est1 )
score_mae_1_valid_man <- (1/(nrow(val_fg))) * sum(abs(val_fg$RDA1 - Y_valid_est1))


score_mae_1_valid_fc == score_mae_1_valid_man #the values of MAE computed by the function or the manual formula are equals
### MAE 2, 3 -----
#we repeat the previous operation for the rda axes 2 and 3
Y_valid_est2<-predict (linear_model2, newdata = val_fg )
Y_valid_est3<-predict (linear_model3, newdata = val_fg )
Y_valid_est2 #predicted data for validation set
val_fg$RDA2  #observed values for validation set
#axis RDA2 
score_mae_2_valid_fc <- mae(val_fg$RDA2, Y_valid_est2 )
score_mae_2_valid_man <- (1/(nrow(val_fg))) * sum(abs(val_fg$RDA2 - Y_valid_est2))
#Axis RDA3
score_mae_3_valid_fc <- mae(val_fg$RDA3, Y_valid_est3 )
score_mae_3_valid_man <- (1/(nrow(val_fg))) * sum(abs(val_fg$RDA3 - Y_valid_est3))
#print the MAE values for the 3 RDA axes of Validation Set
score_mae_1_valid_fc ; score_mae_2_valid_fc ; score_mae_3_valid_fc

#EVALUATION FINALE  -----

# predict (linear_model1, newdata = valid )#s$class
#axe 1 
Y_final_est1<-predict (linear_model1, newdata = test_final_env )
#axe 2
Y_final_est2<-predict (linear_model2, newdata = test_final_env )

#axe 3
Y_final_est3<-predict (linear_model3, newdata = test_final_env )
#reunite the 3 axes for final test
Ypredict_lm<- data.frame(Y_final_est1,Y_final_est2,Y_final_est3) # un test pour predict

   

#Projection des valeurs réelles et estimées----
fitted_values_predict <- data.frame(predict(RDA, x=env_observed, y=test_final_env, type="wa"))
head(fitted_values_predict)
head(test_final_fg)
plot(RDA, display = c("species", "reg"))#inertie sur l'axe par variable Y : groupe fonctionnel
plot(RDA, display = c("reg", "sp"))#biplot
plot(RDA, display = "wa")
points(fitted_values_predict[,1:2], col = "red", pch = 19)
points(Ypredict_lm[, 1:2], col = "blue", pch = 19)  

#Evaluation criteria
Eval<-data.frame(row.names = c("Axe1","Axe2","Axe3"))
#RMSE
Eval$RMSE<-c(sqrt(mean((fitted_values_predict$RDA1 - Ypredict_lm$Y_final_est1)^2)),
             sqrt(mean((fitted_values_predict$RDA2 - Ypredict_lm$Y_final_est2)^2)),
             sqrt(mean((fitted_values_predict$RDA3 - Ypredict_lm$Y_final_est3)^2)))



#MAE = Mean Absolute Error

fitted_values_predict$RDA1 # Observed values of axis RDA1, in the Final Validation set
Ypredict_lm$Y_final_est1 # Predicted values of linear model 1, in the Final Validation set, only the values
Y_final_est1 # Predicted values of linear model 1, in the Final Validation set, identifier of sample with values

score_mae_1_final <- mae(fitted_values_predict$RDA1, Ypredict_lm$Y_final_est1) #
score_mae_1_final_bis <- (1/(nrow(fitted_values_predict))) * sum(abs(fitted_values_predict$RDA1 - Ypredict_lm$Y_final_est1))


score_mae_2_final <- mae(fitted_values_predict$RDA2, Ypredict_lm$Y_final_est2)#compute the MAE score using the function mae{Metrics}
score_mae_2_final_bis <- (1/(nrow(fitted_values_predict))) * sum(abs(fitted_values_predict$RDA2 - Ypredict_lm$Y_final_est2))#compute the MAE score manually, using the formula 1/N * sum|y_est - y^_est|
score_mae_3_final <- mae (fitted_values_predict$RDA3, Ypredict_lm$Y_final_est3) #score 3 is higher
score_mae_3_final_bis <- (1/(nrow(fitted_values_predict))) * sum(abs(fitted_values_predict$RDA3 - Ypredict_lm$Y_final_est3))
#print MAE values for 3 RDA axes in the Final Evaluation Set
score_mae_1_final ; score_mae_2_final ; score_mae_3_final

#> score_mae_1_valid_fc ; score_mae_2_valid_fc ; score_mae_3_valid_fc
#distance matrix
dist_1 <- y1_est$RDA1 - y1_est$y_est
dist_2 <- y2_est$RDAD - y2_est$y_est
dist_3 <- y3_est$RDA3 - y3_est$y_est
#r2 = 1-(MSE/MST)


# r2_1 <- 1 - (score_new1^2)/ (mst (dist_1)) #TEST pour calcul du R²
r2_1 <- RsquareAdj(y1_est$RDA1, y1_est$y_est, n=32) #r2 is lower  
r2_2 <- RsquareAdj(y2_est$RDA2, y2_est$y_est, n = 32)
r2_3 <- RsquareAdj(y3_est$RDA3, y3_est$y_est, n=32)

#Compare  r² of  3 axes of the RDA
sum(r2_1)#lower
sum(r2_2)
sum(r2_3)
mean(r2_1)#lower 
mean(r2_2)
mean(r2_3)


# MC WITH ELASTICNET ----

