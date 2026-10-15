# 0. Infos ----



# 1. Importation des librairies ----

library(tidyverse) #PCoA
library(readr)
library(vegan) #PCoA
library(ade4)#ancien package pr rda
library(readr)
library(ggvegan)#plots rda ?
library(ape) # PCoA
library(lubridate)
  
# 2. Importation des données ----

fg_observed <- read.table('fg_observed.csv', header = TRUE, sep = ',',
                          stringsAsFactors = TRUE)
summary(fg_observed)

envt9 <- read.table('09_variables.csv', header = TRUE, sep = ',',
                          stringsAsFactors = TRUE)
envt13 <- read.table('13_variables.csv', header = TRUE, sep = ',',
                    stringsAsFactors = TRUE)

distance <- dist(fg_observed, method='canberra')


# 3. PCoA ----
# Basée sur les similarités entre les objets

PCoA <- wcmdscale(distance, eig = TRUE)

summary(PCoA)

PCoA$points

print(PCoA)

plot(PCoA$points)



# 4. Programme général

n <- length(fg_observed)
ratio <- 0.5
for (i in 1:length(fg_observed)){
  
  # On sépare les les train des val
  train <- sample(1:n,ratio*n)
  val <- setdiff(1:n, train)
  
  D <- dist(fg_observed, method='canberra')
  
}




