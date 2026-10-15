#auteur.ice : BARON Anaelle
#PROJET MLB (M2 MODE V Monbet) CHLOELAN, theme champi
#Date : "2026-09-25 15:49:10 CEST"

#libraries -----
library(tidyverse)
#importer le jeu de donnees : observees a l'echelle des familles de bacteries -----
library(readr)
observed <- read_csv("data/averill/bacteria/family/observed.csv")
View(observed)

head(observed)
#somme des lignes pour i allant de 1 à fin ? ------
vect_sum_i <- 0
for (i in range (1:10)){
  sum_i <- sum(observed[i,2:12])
  vect_sum_i <- c (vect_sum_i, sum_i)
}
#test en dehors de la boucle if------
vect_sum_ibis <- 0
sum_ibis <- sum(observed[10,2:12])
vect_sum_ibis <- c(vect_sum_ibis, sum_ibis)
vect_sum_ibis
#Obs diverses ------
sum(observed[4,2:12])
str(observed)
summary(observed)
dim(observed)

