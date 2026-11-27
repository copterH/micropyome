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
library(glmnet)

set.seed(123)


#IMPORTATION DES DONNEES----

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






# RDA ----
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


#Récupération des noms des variables
cols<-intersect(names(RDA_coordinates), names(env_observed)) #select all factors coefficients







# MONTE CARLO ----

## Initialisation ----
B <- 100 # Number of iterations for the loop

# empty dataframe for saving all the lm inside the loop

m <- matrix(ncol = 0, nrow = B)
full_lm1 <- data.frame(m)
full_lm2 <- data.frame(m)
full_lm3 <- data.frame(m)

# créer un dataframe qui enregistre les MSE de chaque modèle testé

n <- matrix(ncol = 0, nrow = 3)
scores <- data.frame(n)


## ____Loop____ ----

for (i in 1:B){
  
  print(paste0(i,"/100"))
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
  
  ## Validation ----
  
  #Y^  Axe 1
  y1_est<-predict (linear_model1, newdata = val_fg)
  
  #Y^  Axe 2
  y2_est<-predict (linear_model2, newdata = val_fg)
  
  #Y^  Axe 3
  y3_est<-predict (linear_model3, newdata = val_fg)
  
  
  
  ## Calcul de la distance (MSE) ----
  
  MSE1<- mean((val_fg$RDA1 - y1_est)^2)
  MSE2<- mean((val_fg$RDA2 - y2_est)^2)
  MSE3<- mean((val_fg$RDA3 - y3_est)^2)

  
  ## Enregistrement des coefficients et des MSE ----
  
  ## Coefficients 
  
  full_lm1 <- rbind(full_lm1, linear_model1$coefficients)
  full_lm2 <- rbind(full_lm2, linear_model2$coefficients)
  full_lm3 <- rbind(full_lm3, linear_model3$coefficients)
  
  ## MSE
  
  scores <- rbind(scores, c(MSE1, MSE2, MSE3))
 
  
   
}

## ____End of loop____ ----

#Proposition: la liste "cols" correspond exactement à ce vecteur
colnames(full_lm1) <- c("Intercept", "pC", "cn", "pH", "NPP", "map", "mat", "forest", "conifer",
                        "relEM", "P", "K", "Ca", "Mg")
colnames(full_lm2) <- c("Intercept", "pC", "cn", "pH", "NPP", "map", "mat", "forest", "conifer",
                        "relEM", "P", "K", "Ca", "Mg")
colnames(full_lm3) <- c("Intercept", "pC", "cn", "pH", "NPP", "map", "mat", "forest", "conifer",
                        "relEM", "P", "K", "Ca", "Mg")
colnames(scores) <- c("score1", "score2", "score3")


head(full_lm1)
head(full_lm2)
head(full_lm3)

head(scores)

#MODELE LINEAIRE MOYEN FINAL ----
LM1_final<- data.frame('LM1'=rapply (full_lm1, mean)) %>% t()
LM2_final<- data.frame('LM2'=rapply (full_lm2, mean)) %>% t()
LM3_final<- data.frame('LM3'=rapply (full_lm3, mean)) %>% t()

#Creation of the final model : Mean of all coefficient.


## Calcul of RMSE ----

RMSE1 <- sqrt(sum(scores$score1)/B)
RMSE2 <- sqrt(sum(scores$score2)/B)
RMSE3 <- sqrt(sum(scores$score3)/B)

print(RMSE1)
print(RMSE2)
print(RMSE3)


###VALID in training set ???


##  Compute MAE on the test set inside the training set ----

val_fg #validation set inside training set
### MAE 1 -----
Y_valid_est1<-predict (linear_model1, newdata = val_fg)
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
score_mae_1_valid_fc ; score_mae_2_valid_fc ; score_mae_3_valid_fc #using function mae{Metrics}
score_mae_1_valid_man ; score_mae_2_valid_man ; score_mae_3_valid_man #manually writing the formula : check is ok




# FINAL EVALUATION ----

# predict (linear_model1, newdata = valid )#s$class #old version of model prediction
#axe 1 of the RDA
head(test_final_env) #environmental data obs  of final validation dataset : used to predict
head(test_final_fg) #functional group data obs of final validation dataset : not to be touched, only to compare
full_lm1#dataframe containing the values coefficient of all the iteration of the linear model in the loop
dim(full_lm1)# it contain 100 lines, corresponding to 100 iterations in MonteCarlo loop



## Calcul des valeurs estimées Y^ des données finales (25% des données complètes) ----


### LINEAR MODEL ----


####Y^  Axe 1 ----

FINAL_lm1_y_predict<- test_final_env %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(LM1_final['LM1',cur_column()]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(LM1_final['LM1','Intercept'])) %>%
  
  #3. Select to have only y1_est
  select(y_est)

####Y^  Axe 2 ----

FINAL_lm2_y_predict<- test_final_env %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(LM2_final['LM2',cur_column()]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(LM2_final['LM2','Intercept'])) %>%
  
  #3. Select to have only y2_est
  select(y_est)


####Y^  Axe 3 ----

FINAL_lm2_y_predict<- test_final_env %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(LM3_final['LM3',cur_column()]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(LM3_final['LM3','Intercept'])) %>%
  
  #3. Select to have only y2_est
  select(y_est)

## Projection des valeurs réelles et estimées----

fitted_values_proj <- data.frame(predict(RDA, x=test_final_env, y=test_final_fg, type="wa"))
head(fitted_values_proj)
head(test_final_fg)
plot(RDA, display = c("species", "reg"))#inertie sur l'axe par variable Y : groupe fonctionnel
plot(RDA, display = c("reg", "sp"))#biplot
plot(RDA, display = "wa")
points(fitted_values_proj[,1:2], col = "red", pch = 19)

points(Ypredict_lm[, 1:2], col = "blue", pch = 19)  

#Evaluation criteria
Eval<-data.frame(row.names = c("Axe1","Axe2","Axe3"))
#RMSE
Eval$RMSE<-c(sqrt(mean((fitted_values_proj$RDA1 - Ypredict_lm$Y_final_est1)^2)),
             sqrt(mean((fitted_values_proj$RDA2 - Ypredict_lm$Y_final_est2)^2)),
             sqrt(mean((fitted_values_proj$RDA3 - Ypredict_lm$Y_final_est3)^2)))



## MAE = Mean Absolute Error ----

fitted_values_proj$RDA1 # Observed values of axis RDA1, in the Final Validation set
Ypredict_lm$Y_final_est1 # Predicted values of linear model 1, in the Final Validation set, only the values
Y_final_est1 # Predicted values of linear model 1, in the Final Validation set, identifier of sample with values

score_mae_1_final <- mae(fitted_values_proj$RDA1, Ypredict_lm$Y_final_est1) #
score_mae_1_final_bis <- (1/(nrow(fitted_values_proj))) * sum(abs(fitted_values_proj$RDA1 - Ypredict_lm$Y_final_est1))


score_mae_2_final <- mae(fitted_values_proj$RDA2, Ypredict_lm$Y_final_est2)#compute the MAE score using the function mae{Metrics}
score_mae_2_final_bis <- (1/(nrow(fitted_values_proj))) * sum(abs(fitted_values_proj$RDA2 - Ypredict_lm$Y_final_est2))#compute the MAE score manually, using the formula 1/N * sum|y_est - y^_est|
score_mae_3_final <- mae (fitted_values_proj$RDA3, Ypredict_lm$Y_final_est3) #score 3 is higher
score_mae_3_final_bis <- (1/(nrow(fitted_values_proj))) * sum(abs(fitted_values_proj$RDA3 - Ypredict_lm$Y_final_est3))
#print MAE values for 3 RDA axes in the Final Evaluation Set
score_mae_1_final ; score_mae_2_final ; score_mae_3_final

#> score_mae_1_valid_fc ; score_mae_2_valid_fc ; score_mae_3_valid_fc
#distance matrix
dist_1 <- y1_est$RDA1 - y1_est$y_est
dist_2 <- y2_est$RDAD - y2_est$y_est
dist_3 <- y3_est$RDA3 - y3_est$y_est
#r2 = 1-(MSE/MST)


# r2_1 <- 1 - (score_new1^2)/ (mst (dist_1)) #TEST pour calcul du R²
r2_1 <- RsquareAdj(y1_est$RDA1, y1_est$y_est, n = 32) #r2 is lower  
r2_2 <- RsquareAdj(y2_est$RDA2, y2_est$y_est, n = 32)
r2_3 <- RsquareAdj(y3_est$RDA3, y3_est$y_est, n = 32)





#Compare  r² of  3 axes of the RDA
sum(r2_1)#lower
sum(r2_2)
sum(r2_3)
mean(r2_1)#lower 
mean(r2_2)
mean(r2_3)

## SAVING SCORES IN DATAFRAME ----

MC_scores_val <- data.frame(row.names = c("Axe1","Axe2","Axe3"))




# MC WITH ELASTICNET ----




n <- matrix(ncol = 0, nrow = 3)
scores <- data.frame(n)

#Alphas initialisation for elastic net
alphas <- seq(0, 1, by = 0.1)


#Dataframe saving best hyperparameters
df_hp1 <- data.frame(best_alpha = NA, best_lambda = NA)
df_hp2 <- data.frame(best_alpha = NA, best_lambda = NA)
df_hp3 <- data.frame(best_alpha = NA, best_lambda = NA)

B<-10

for (i in 1:B){
  print(paste0(i,"/100"))
  
  ## Initialisation ----
  
  #For choosing best parameters later
  cv_results1 <- data.frame(alpha = alphas, min_cvm = NA, lambda_min = NA)
  cv_results2 <- data.frame(alpha = alphas, min_cvm = NA, lambda_min = NA)
  cv_results3 <- data.frame(alpha = alphas, min_cvm = NA, lambda_min = NA)
  

  # création de deux groupes pour l'entrainement(train) et la validation croisée(val)
  
  train_fg <- RDA_coordinates %>% sample_frac(2/3) #on prend les coordonnees de 2/3 des data d'entrainement (96li)
  val_fg<-setdiff(RDA_coordinates,train_fg)#le 1/3 restant est utilise pour la validation(32li)
  
  ## Search Grid over alpha -----
  for (j in seq_along(alphas)) {
    #Linear model with Elastic net
    #Axe 1
    cv_fit1 <- cv.glmnet(as.matrix(train_fg[ , 5:ncol(train_fg)]), 
                        train_fg[ , "RDA1"], 
                        alpha = alphas[j], nfolds = 10)
    cv_results1$min_cvm[j] <- min(cv_fit1$cvm) #minimum cross validated error for each alpha
    cv_results1$lambda_min[j] <- cv_fit1$lambda.min
    
    
    #Axe 2
    cv_fit2 <- cv.glmnet(as.matrix(train_fg[ , 5:ncol(train_fg)]), 
                         train_fg[ , "RDA2"], 
                         alpha = alphas[j], nfolds = 10)
    cv_results2$min_cvm[j] <- min(cv_fit2$cvm) #minimum cross validated error for each alpha
    cv_results2$lambda_min[j] <- cv_fit2$lambda.min 

    
    #Axe 3
    cv_fit3 <- cv.glmnet(as.matrix(train_fg[ , 5:ncol(train_fg)]), 
                         train_fg[ , "RDA3"], 
                         alpha = alphas[j], nfolds = 10)
    cv_results3$min_cvm[j] <- min(cv_fit3$cvm) #minimum cross validated error for each alpha
    cv_results3$lambda_min[j] <- cv_fit3$lambda.min
    
  }
  
  #Axe 1
  best_idx1 <- which.min(cv_results1$min_cvm)
  best_alpha1 <- cv_results1$alpha[best_idx1]
  best_lambda1 <- cv_results1$lambda_min[best_idx1]
  
  df_hp1[i,"best_alpha"]<-best_alpha1
  df_hp1[i,"best_lambda"]<-best_lambda1
  
  
  #Axe 2
  best_idx2 <- which.min(cv_results2$min_cvm)
  best_alpha2 <- cv_results2$alpha[best_idx2]
  best_lambda2 <- cv_results2$lambda_min[best_idx2]
  
  df_hp2[i,"best_alpha"]<-best_alpha2
  df_hp2[i,"best_lambda"]<-best_lambda2
  
  
  #Axe 3
  best_idx3 <- which.min(cv_results3$min_cvm)
  best_alpha3 <- cv_results3$alpha[best_idx3]
  best_lambda3 <- cv_results3$lambda_min[best_idx3]
  
  df_hp3[i,"best_alpha"]<-best_alpha3
  df_hp3[i,"best_lambda"]<-best_lambda3
}


## End Loop ----

### Calcul of the mean alpha and lambda ----

summary(df_hp1)
final_alpha1 <- mean(df_hp1[,"best_alpha"])
print(final_alpha1)
print(sd(df_hp1[,"best_alpha"]))
final_lambda1 <- mean(df_hp1[,"best_lambda"])


summary(df_hp2)
final_alpha2 <- mean(df_hp2[,"best_alpha"])
print(final_alpha2)
print(sd(df_hp2[,"best_alpha"]))
final_lambda2 <- mean(df_hp2[,"best_lambda"])


summary(df_hp3) #Pb: bcp de valeurs très hautes qui gonflent la moyenne ->  est ce que la médiane serait mieux ?
final_alpha3 <- mean(df_hp3[,"best_alpha"])
print(final_alpha3)
print(sd(df_hp3[,"best_alpha"]))
final_lambda3 <- mean(df_hp3[,"best_lambda"])


#### Return the best models ----

# Axe 1
EN_lm1 <- cv.glmnet(as.matrix(train_fg[,5:ncol(train_fg)]),
                    train_fg[,"RDA1"],
                    alpha = final_alpha1, nfolds = 10)
final_coefs1 <- coef(EN_lm1, s = "lambda.1se")

# Axe 2
EN_lm2 <- cv.glmnet(as.matrix(train_fg[,5:ncol(train_fg)]),
                    train_fg[,"RDA2"],
                    alpha = final_alpha2, nfolds = 10)
final_coefs2 <- coef(EN_lm2, s = "lambda.1se")

# Axe 3
EN_lm3 <- cv.glmnet(as.matrix(train_fg[,5:ncol(train_fg)]),
                    train_fg[,"RDA3"],
                    alpha = final_alpha3, nfolds = 10)
final_coefs3 <- coef(EN_lm3, s = "lambda.1se")


## Calculation of Y^ ----

###Y^  Axe 1 ----
EN_y1_est_train<- RDA_coordinates %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(final_coefs1[cur_column(),]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(final_coefs1['(Intercept)', ])) %>%
  
  #3. Select to have only RDA1, y1_est
  select(Row.names,RDA1,y_est)


###Y^  Axe 2 ----
EN_y2_est_train<- RDA_coordinates %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(final_coefs2[cur_column(),]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(final_coefs2['(Intercept)', ])) %>%
  
  #3. Select to have only RDA2, y2_est
  select(Row.names,RDA2,y_est)


###Y^  Axe 3 ----
EN_y3_est_train<- RDA_coordinates %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(final_coefs3[cur_column(),]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(final_coefs3['(Intercept)', ])) %>%
  
  #3. Select to have only RDA3, y3_est
  select(Row.names,RDA3,y_est)


### scores train ----

EN_scores_train <- data.frame(row.names = c("Axe1","Axe2","Axe3"))

EN_scores_train$RMSE <- c(sqrt(mean((EN_y1_est_train$RDA1 - EN_y1_est_train$y_est)^2)),
                        sqrt(mean((EN_y2_est_train$RDA2 - EN_y2_est_train$y_est)^2)),
                        sqrt(mean((EN_y3_est_train$RDA3 - EN_y3_est_train$y_est)^2)))

EN_scores_train$MAE <- c(mae(EN_y1_est_train$RDA1,EN_y1_est_train$y_est),
                       mae(EN_y2_est_train$RDA2, EN_y2_est_train$y_est),
                       mae(EN_y3_est_train$RDA3, EN_y3_est_train$y_est))

EN_scores_train$r2 <- c(mean(RsquareAdj(EN_y1_est_train$RDA1, EN_y1_est_train$y_est, n = 32)),
                        mean(RsquareAdj(EN_y2_est_train$RDA2, EN_y2_est_train$y_est, n = 32)),
                        mean(RsquareAdj(EN_y3_est_train$RDA3, EN_y3_est_train$y_est, n = 32))
                        )



## FINAL EVALUATION OF EN ----

### Calculation of Y^ eval ----

#### Y^  Axe 1 ----
EN_y1_est_eval<- RDA_coordinates %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(final_coefs1[cur_column(),]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(final_coefs1['(Intercept)', ])) %>%
  
  #3. Select to have only RDA1, y1_est
  select(Row.names,RDA1,y_est)


#### Y^  Axe 2 ----
EN_y2_est_eval<- RDA_coordinates %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(final_coefs2[cur_column(),]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(final_coefs2['(Intercept)', ])) %>%
  
  #3. Select to have only RDA2, y2_est
  select(Row.names,RDA2,y_est)


#### Y^  Axe 3 ----
EN_y3_est_eval<- test_final_env %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(final_coefs3[cur_column(),]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(final_coefs3['(Intercept)', ])) %>%
  
  #3. Select to have only RDA3, y3_est
  select(Row.names,RDA3,y_est)



## Calculation of scores ----

EN_scores_val <- data.frame(row.names = c("Axe1","Axe2","Axe3"))

EN_scores_val$RMSE <- c(sqrt(mean((EN_y1_est_train$RDA1 - EN_y1_est_train$y_est)^2)),
                          sqrt(mean((EN_y2_est_train$RDA2 - EN_y2_est_train$y_est)^2)),
                          sqrt(mean((EN_y3_est_train$RDA3 - EN_y3_est_train$y_est)^2)))

EN_scores_val$MAE <- c(mae(EN_y1_est_train$RDA1,EN_y1_est_train$y_est),
                         mae(EN_y2_est_train$RDA2, EN_y2_est_train$y_est),
                         mae(EN_y3_est_train$RDA3, EN_y3_est_train$y_est))

EN_scores_val$r2 <- c(mean(RsquareAdj(EN_y1_est_train$RDA1, EN_y1_est_train$y_est, n = 32)),
                        mean(RsquareAdj(EN_y2_est_train$RDA2, EN_y2_est_train$y_est, n = 32)),
                        mean(RsquareAdj(EN_y3_est_train$RDA3, EN_y3_est_train$y_est, n = 32))
)




# COMPARING MC AND EN ----

MC_scores_val

EN_scores_val






