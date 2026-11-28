"
author: A. Baron, C. Bres, E. Desnyder
Machine Learning for Biology Project

"





# 0. PREPARING WORKSPACE ----


# Cleaning environment

rm(list=ls())

# Importing libraries

library(tidyverse) # Data manipulation
library(vegan) # RDA
library(Metrics) # RMSE, MAE
library(cv) # Cross validation
library(ape)
library(glmnet) # Linear model

set.seed(12)





# 1. DATA SET UP ----


## 1.1. DATA IMPORTATION ----

### Functionnal groups ----
file_fg <- data.frame(read_csv("fg_observed_averyll.csv", show_col_types = FALSE))
row.names(file_fg)<-file_fg[,1] # We rename the indices by the first column.

### Environmental parameters ----
file_env<-data.frame(read_csv("13_variables_averyll.csv", show_col_types = FALSE))
row.names(file_env)<-file_env[,1]


## 1.2. SELECTION OF A SUBSET FOR THE FINAL EVALUATION ----

### Functionnal groups ----
# We extract 25% of the dataset for the final test.
test_final_fg<-file_fg %>% sample_frac(.25)
# Name of the sample = 1rst column of the table.
sample_final<-test_final_fg[,1]
# Find all the lines from file_fg (the remaining 3/4) which are not in the final
# evaluation subset, will be used in the train.
fg_observed<-setdiff(file_fg,test_final_fg)

# We clean the data by removing the first column repeating the site names.
test_final_fg <- select(test_final_fg,-1)
fg_observed <- select(fg_observed,-1)
file_env<-select(file_env,-1)


### Environmental parameters ----
test_final_env<- file_env %>% filter(row.names(file_env) %in% sample_final)
env_observed<-setdiff(file_env,test_final_env)





# 2. RDA ----
# Redondancy analysis

RDA <- rda(fg_observed ~ pC + cn + pH + NPP + map + mat + forest +
                         conifer + relEM +P + K + Ca + Mg, 
                         data = env_observed)


## 2.1. Extracting the RDA axes ----

test<-RDA$CCA$u[,1:3] # Extract the RDA coordinates.


# ???
#display = sites or  species (functionnal groups)
#choices= axis
#scaling ="none", "sites", "species", or "symmetric", which correspond to the values 0, 1, 2, and 3 respectively

RDA_coordinates<-scores(RDA, display = "sites", choices = 1:3, scaling = 2) # sp? ???
RDA_coordinates

### Visualisation of the RDA ----

scores(RDA, display = "sites", choices = 1:3, scaling = "sp") # find same result as the previous line : score on the sample axis : 
scores(RDA, display = "species", choices = 1:3, scaling = 3) # inertia on the axis by variable Y : functionnal group 
plot(RDA, display = c("species", "reg"))
plot(RDA, display = c("reg", "sp")) # biplot
plot(RDA, display = "wa") # points
# plot(RDA, display = "cn") # var X


## 2.2. Merging with environemental variables ----

RDA_coordinates<-data.frame(merge(RDA_coordinates,env_observed,by=0))%>% as.data.frame()
# ??? as.data.frame() utile ?

# Taking the variables names 

cols<-intersect(names(RDA_coordinates), names(env_observed)) # select all factors coefficients





# 3. LINEAR MODEL ----


## 3.1. Initialization out of loop ----

B <- 100 # Number of iterations for the loop

# Empty dataframe for saving all the LM inside the loop

m <- matrix(ncol = 0, nrow = B)
full_lm1 <- data.frame(m)
full_lm2 <- data.frame(m)
full_lm3 <- data.frame(m)

# Empty dataframe for saving the MSE and MAE for each tested model

n <- matrix(ncol=0, nrow=3)
LM_MSE <- data.frame(n)
LM_MAE <- data.frame(n)


## 3.2. ____Loop____ ----

for (i in 1:B){
  
  print(paste0(i,"/100"))
  
  ### 3.2.1. Initialization ----
  
  # Creation of 2 groups for training (train) and cross validation (val).
  
  # We keep the coordinates of 2/3 of the training data.
  train_fg <- RDA_coordinates %>% sample_frac(2/3)
  # The remaining 1/3 is used for validation.
  val_fg <- setdiff(RDA_coordinates,train_fg)
  
  
  ### 3.2.2. Linear model ----
  
  # Axe 1
  linear_model1<-lm( RDA1 ~ pC + cn + pH + NPP + map + mat + forest + conifer +
                       relEM +P + K + Ca + Mg, data=train_fg)
  # Axe 2
  linear_model2<-lm( RDA2 ~ pC + cn + pH + NPP + map + mat + forest + conifer +
                       relEM +P + K + Ca + Mg, data=train_fg)
  # Axe 3
  linear_model3<-lm( RDA3 ~ pC + cn + pH + NPP + map + mat + forest + conifer +
                       relEM +P + K + Ca + Mg, data=train_fg) 
  
  ### 3.2.3. Validation ----
  
  # Y^  Axe 1
  y1_est<-predict (linear_model1, newdata = val_fg)
  
  # Y^  Axe 2
  y2_est<-predict (linear_model2, newdata = val_fg)
  
  # Y^  Axe 3
  y3_est<-predict (linear_model3, newdata = val_fg)
  
  
  ### 3.2.4. Evaluation criteria ----
  
  #### MSE
  
  MSE1<- mean((val_fg$RDA1 - y1_est)^2)
  MSE2<- mean((val_fg$RDA2 - y2_est)^2)
  MSE3<- mean((val_fg$RDA3 - y3_est)^2)
  
  LM_MSE <- rbind(LM_MSE, c(MSE1, MSE2, MSE3))
  
  #### MAE
  
  LM_MAE <- rbind(LM_MAE, c(mae(val_fg$RDA1, y1_est),
                            mae(val_fg$RDA2, y2_est),
                            mae(val_fg$RDA3, y3_est)))
  
  
  ### 3.2.5. Storing coefficients ----
  
  full_lm1 <- rbind(full_lm1, linear_model1$coefficients)
  full_lm2 <- rbind(full_lm2, linear_model2$coefficients)
  full_lm3 <- rbind(full_lm3, linear_model3$coefficients)
   
}


## 3.3. ____End of loop____ ----

colnames(full_lm1) <- c("Intercept", "pC", "cn", "pH", "NPP", "map", "mat", "forest", "conifer",
                        "relEM", "P", "K", "Ca", "Mg")
colnames(full_lm2) <- c("Intercept", "pC", "cn", "pH", "NPP", "map", "mat", "forest", "conifer",
                        "relEM", "P", "K", "Ca", "Mg")
colnames(full_lm3) <- c("Intercept", "pC", "cn", "pH", "NPP", "map", "mat", "forest", "conifer",
                        "relEM", "P", "K", "Ca", "Mg")

colnames(LM_MSE) <- c("MSE1", "MSE2", "MSE3")
colnames(LM_MAE) <- c("MAE1", "MAE2", "MAE3")

head(full_lm1)
head(full_lm2)
head(full_lm3)

head(LM_MSE)
head(LM_MAE)


### 3.3.1. Final mean Linear Model ----

# Creation of the final model : its coefficients are the mean of coefficients of each model.
LM1_final<- data.frame('LM1'=rapply (full_lm1, mean)) %>% t()
LM2_final<- data.frame('LM2'=rapply (full_lm2, mean)) %>% t()
LM3_final<- data.frame('LM3'=rapply (full_lm3, mean)) %>% t()


### 3.3.2. Evaluation criteria in LM_scores_train ----

LM_scores_train<-data.frame(row.names = c("Axe1","Axe2","Axe3"))

LM_scores_train$RMSE<-c(sqrt(mean(LM_MSE$MSE1)),
                        sqrt(mean(LM_MSE$MSE2)),
                        sqrt(mean(LM_MSE$MSE3)))

LM_scores_train$MAE <- c(mean(LM_MAE$MAE1),
                         mean(LM_MAE$MAE2),
                         mean(LM_MAE$MAE3))





# 4. ELASTICNET ----


## 4.1. Initialization out of loop ----

n <- matrix(ncol = 0, nrow = 3)
scores <- data.frame(n)

#Alphas initialisation for elastic net
alphas <- seq(0, 1, by = 0.1)


#Dataframe saving best hyperparameters
df_hp1 <- data.frame(best_alpha = NA, best_lambda = NA)
df_hp2 <- data.frame(best_alpha = NA, best_lambda = NA)
df_hp3 <- data.frame(best_alpha = NA, best_lambda = NA)

B<-100

## 4.2. ____Loop____ ----

for (i in 1:B){
  print(paste0(i,"/100"))
  
  ### 4.2.1. Initialization ----
  
  # For choosing best parameters later.
  cv_results1 <- data.frame(alpha = alphas, min_cvm = NA, lambda_min = NA)
  cv_results2 <- data.frame(alpha = alphas, min_cvm = NA, lambda_min = NA)
  cv_results3 <- data.frame(alpha = alphas, min_cvm = NA, lambda_min = NA)
  

  # Creation of 2 groups for training (train) and cross validation (val).
  
  # We keep the coordinates of 2/3 of the training data.
  train_fg <- RDA_coordinates %>% sample_frac(2/3)
  # The remaining 1/3 is used for validation.
  val_fg<-setdiff(RDA_coordinates,train_fg)
  
  ### 4.2.2. Search Grid over alpha ----
  
  for (j in seq_along(alphas)) {
    
    # Linear model with Elastic net
    # Axe 1
    cv_fit1 <- cv.glmnet(as.matrix(train_fg[ , 5:ncol(train_fg)]), 
                        train_fg[ , "RDA1"], 
                        alpha = alphas[j], nfolds = 10)
    cv_results1$min_cvm[j] <- min(cv_fit1$cvm) # Minimum cross validated error for each alpha.
    cv_results1$lambda_min[j] <- cv_fit1$lambda.min
    
    
    # Axe 2
    cv_fit2 <- cv.glmnet(as.matrix(train_fg[ , 5:ncol(train_fg)]), 
                         train_fg[ , "RDA2"], 
                         alpha = alphas[j], nfolds = 10)
    cv_results2$min_cvm[j] <- min(cv_fit2$cvm) # Minimum cross validated error for each alpha.
    cv_results2$lambda_min[j] <- cv_fit2$lambda.min 

    
    # Axe 3
    cv_fit3 <- cv.glmnet(as.matrix(train_fg[ , 5:ncol(train_fg)]), 
                         train_fg[ , "RDA3"], 
                         alpha = alphas[j], nfolds = 10)
    cv_results3$min_cvm[j] <- min(cv_fit3$cvm) # Minimum cross validated error for each alpha.
    cv_results3$lambda_min[j] <- cv_fit3$lambda.min
    
  }
  
  
  ### 4.2.3. Saving best hyperparameters ----
  
  # Axe 1
  best_idx1 <- which.min(cv_results1$min_cvm)
  best_alpha1 <- cv_results1$alpha[best_idx1]
  best_lambda1 <- cv_results1$lambda_min[best_idx1]
  
  df_hp1[i,"best_alpha"]<-best_alpha1
  df_hp1[i,"best_lambda"]<-best_lambda1
  
  
  # Axe 2
  best_idx2 <- which.min(cv_results2$min_cvm)
  best_alpha2 <- cv_results2$alpha[best_idx2]
  best_lambda2 <- cv_results2$lambda_min[best_idx2]
  
  df_hp2[i,"best_alpha"]<-best_alpha2
  df_hp2[i,"best_lambda"]<-best_lambda2
  
  
  # Axe 3
  best_idx3 <- which.min(cv_results3$min_cvm)
  best_alpha3 <- cv_results3$alpha[best_idx3]
  best_lambda3 <- cv_results3$lambda_min[best_idx3]
  
  df_hp3[i,"best_alpha"]<-best_alpha3
  df_hp3[i,"best_lambda"]<-best_lambda3
  
}


## 4.3. ____End of loop____ ----


### 4.3.1. Calcul of the mean alpha and lambda ----

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



summary(df_hp3) # ??? keep the commentar ? #Pb: bcp de valeurs très hautes qui gonflent la moyenne ->  est ce que la médiane serait mieux ?
final_alpha3 <- mean(df_hp3[,"best_alpha"])
print(final_alpha3)
print(sd(df_hp3[,"best_alpha"]))
final_lambda3 <- mean(df_hp3[,"best_lambda"])


#### 4.3.2. Return the best models ----

# Axe 1
EN_lm1 <- cv.glmnet(as.matrix(train_fg[,5:ncol(train_fg)]),
                    train_fg[,"RDA1"],
                    alpha = final_alpha1, nfolds = 10)
final_coefs1 <- coef(EN_lm1, s = "lambda.min")

# Axe 2
EN_lm2 <- cv.glmnet(as.matrix(train_fg[,5:ncol(train_fg)]),
                    train_fg[,"RDA2"],
                    alpha = final_alpha2, nfolds = 10)
final_coefs2 <- coef(EN_lm2, s = "lambda.min")

# Axe 3
EN_lm3 <- cv.glmnet(as.matrix(train_fg[,5:ncol(train_fg)]),
                    train_fg[,"RDA3"],
                    alpha = final_alpha3, nfolds = 10)
final_coefs3 <- coef(EN_lm3, s = "lambda.min")


#### 4.3.3. Calculation of Y^ ----

##### Y^  Axe 1
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


##### Y^  Axe 2
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


##### Y^  Axe 3
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


#### 4.3.4. Evaluation criteria in EN_scores_train ----

EN_scores_train <- data.frame(row.names = c("Axe1","Axe2","Axe3"))

EN_scores_train$RMSE <- c(sqrt(mean((EN_y1_est_train$RDA1 - EN_y1_est_train$y_est)^2)),
                          sqrt(mean((EN_y2_est_train$RDA2 - EN_y2_est_train$y_est)^2)),
                          sqrt(mean((EN_y3_est_train$RDA3 - EN_y3_est_train$y_est)^2)))


EN_scores_train$MAE <- c(mae(EN_y1_est_train$RDA1,EN_y1_est_train$y_est),
                        mae(EN_y2_est_train$RDA2, EN_y2_est_train$y_est),
                        mae(EN_y3_est_train$RDA3, EN_y3_est_train$y_est))





# 5. FINAL EVALUATION ----

## 5.1. LINEAR MODEL ----


### 5.1.1. Calculation of Y^ eval ----
# Computing the estimated values Y^ of final data (25% of whole dataset)


#### Y^  Axe 1

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


#### Y^  Axe 2

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


#### Y^  Axe 3

FINAL_lm3_y_predict<- test_final_env %>% 
  
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

Ypredict_lm <- data.frame('Y_final_est1'=FINAL_lm1_y_predict$y_est, 
                          'Y_final_est2'=FINAL_lm2_y_predict$y_est,
                          'Y_final_est3'=FINAL_lm3_y_predict$y_est, row.names = row.names(FINAL_lm1_y_predict))


### 5.1.2. Evaluation criteria in LM_scores_eval ----

LM_scores_eval<-data.frame(row.names = c("Axe1","Axe2","Axe3"))

LM_scores_eval$RMSE<-c(sqrt(mean((fitted_values_proj$RDA1 - Ypredict_lm$Y_final_est1)^2)),
                       sqrt(mean((fitted_values_proj$RDA2 - Ypredict_lm$Y_final_est2)^2)),
                       sqrt(mean((fitted_values_proj$RDA3 - Ypredict_lm$Y_final_est3)^2)))

LM_scores_eval$MAE <- c(mae(fitted_values_proj$RDA1, Ypredict_lm$Y_final_est1),
                        mae(fitted_values_proj$RDA2, Ypredict_lm$Y_final_est2),
                        mae(fitted_values_proj$RDA3, Ypredict_lm$Y_final_est3))


## 5.2. ELASTICNET ----

### Calculation of Y^ eval ----

#### Y^  Axe 1 ----
EN_y1_est_eval<- test_final_env %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(final_coefs1[cur_column(),]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(final_coefs1['(Intercept)', ])) %>%
  
  #3. Select to have only RDA1, y1_est
  select(y_est)


#### Y^  Axe 2 ----
EN_y2_est_eval<- test_final_env %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(final_coefs2[cur_column(),]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y_est=rowSums(across(all_of(cols)))) %>%
  mutate(y_est, y_est=y_est + as.numeric(final_coefs2['(Intercept)', ])) %>%
  
  #3. Select to have only RDA2, y2_est
  select(y_est)


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
  select(y_est)

EN_predict_eval <- data.frame('Y3_final_predict'=EN_y1_est_eval$y_est, 
                              'Y2_final_predict'=EN_y2_est_eval$y_est,
                              'Y3_final_predict'=EN_y3_est_eval$y_est, row.names = row.names(EN_y1_est_eval))

## Calculation of scores ----

EN_scores_eval <- data.frame(row.names = c("Axe1","Axe2","Axe3"))

EN_scores_eval$RMSE <- c(sqrt(mean((fitted_values_proj$RDA1 - EN_y1_est_eval$y_est)^2)),
                         sqrt(mean((fitted_values_proj$RDA2 - EN_y2_est_eval$y_est)^2)),
                         sqrt(mean((fitted_values_proj$RDA3 - EN_y3_est_eval$y_est)^2)))

EN_scores_eval$MAE <- c(mae(fitted_values_proj$RDA1,EN_y1_est_eval$y_est),
                        mae(fitted_values_proj$RDA2, EN_y2_est_eval$y_est),
                        mae(fitted_values_proj$RDA3, EN_y3_est_eval$y_est))



## Projection of real and estimated values ----

fitted_values_proj <- data.frame(predict(RDA, x=test_final_env, y=test_final_fg, type="wa"))
head(fitted_values_proj)
head(test_final_fg)
plot(RDA, display = c("species", "reg"))# inbertia on the axis by variable Y :  fonctionnal group
plot(RDA, display = c("reg", "sp"))#biplot
plot(RDA, display = "wa")
points(fitted_values_proj[,1:2], col = "red", pch = 19)

points(Ypredict_lm[, 1:2], col = "blue", pch = 19)  
points(EN_predict_eval[, 1:2], col = "chartreuse", pch = 19)  




# FINAL EVALUATION ----

##Computing the estimated values Y^ of final data (25% of whole dataset) ----

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

FINAL_lm3_y_predict<- test_final_env %>% 
  
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

Ypredict_lm <- data.frame('Y_final_est1'=FINAL_lm1_y_predict$y_est, 
                          'Y_final_est2'=FINAL_lm2_y_predict$y_est,
                          'Y_final_est3'=FINAL_lm3_y_predict$y_est, row.names = row.names(FINAL_lm1_y_predict))



## Projection of real and estimated values ----

fitted_values_proj <- data.frame(predict(RDA, x=test_final_env, y=test_final_fg, type="wa"))
head(fitted_values_proj)
head(test_final_fg)
plot(RDA, display = c("species", "reg"))# inertia on the axis by variable Y :  fonctionnal group
plot(RDA, display = c("reg", "sp"))#biplot
plot(RDA, display = "wa")
points(fitted_values_proj[,1:2], col = "red", pch = 19)

points(Ypredict_lm[, 1:2], col = "blue", pch = 19)  
points(EN_predict_eval[, 1:2], col = "chartreuse", pch = 19)  

#Evaluation criteria
LM_scores_eval<-data.frame(row.names = c("Axe1","Axe2","Axe3"))
#RMSE
LM_scores_eval$RMSE<-c(sqrt(mean((fitted_values_proj$RDA1 - Ypredict_lm$Y_final_est1)^2)),
                       sqrt(mean((fitted_values_proj$RDA2 - Ypredict_lm$Y_final_est2)^2)),
                       sqrt(mean((fitted_values_proj$RDA3 - Ypredict_lm$Y_final_est3)^2)))



## MAE = Mean Absolute Error ----
LM_scores_eval$MAE <- c(mae(fitted_values_proj$RDA1, Ypredict_lm$Y_final_est1),
                        mae(fitted_values_proj$RDA2, Ypredict_lm$Y_final_est2),
                        mae(fitted_values_proj$RDA3, Ypredict_lm$Y_final_est3))




"Interpretation:
Linear model (train vs eval):


Elastic net (train vs eval): 

Best model between the two:


Biological interpretation:


"
LM_scores_train
LM_scores_eval
EN_scores_train
EN_scores_eval
