#auteur.ice : BARON Anaelle
#PROJET MLB (M2 MODE V Monbet) CHLOELAN, theme champi
#Creation_Date : "2026-09-25 15:49:10 CEST"
#Modification_Date : 2026-09-28 15:09:54 CEST
#1) libraries -----
library(tidyverse)
library(readr)
library(vegan)#rda (le+ utilise mnt)
library(tidyverse)#subset ou select ?
library(ade4)#ancien package pr rda
library(readr)
library(ggvegan)#plots rda ?
# library(ggplot2)
#importer le jeu de donnees : observees a l'echelle des familles de bacteries -----
#chemin d'acces ds repo de base : data/averill/bacteria/family/observed.csv")
observed <- read_csv("observed.csv")
View(observed)

head(observed)
#2)) somme des lignes pour i allant de 1 à fin ? ------
vect_sum_i <- 0
for (i in range (1:10)){
  sum_i <- sum(observed[i,2:12])
  vect_sum_i <- c (vect_sum_i, sum_i)
}
#3) test en dehors de la boucle if------
vect_sum_ibis <- 0
#en excluant la col "autres"
sum_ibis <- sum(observed[11,3:12])
vect_sum_ibis <- c(vect_sum_ibis, sum_ibis)
#verifier en reajoutant la categ autre
 (vect_sum_ibis +  observed$other[1:10])
vect_sum_ibis <- vect_sum_ibis [-1]
## 3.a ceci est un sous-titre -----
## 3.b egalement ------
#4) Obs diverses ------
sum(observed[4,2:12])
str(observed)
summary(observed)
dim(observed)
#5) RDA = Analyse de redondance sur les familles de champignon V1 -----
##import var X et Y
fam_champi <- read.csv("observed.csv") # Y
var9_env <- read.csv("09_variables.csv")#X restraint
var13_env <- read.csv("13_variables.csv") #X tot

#verif que les noms des echantillons sont les memmes dans les diff dataset
fam_champi$X == var9_env$X
sum (fam_champi$X == var13_env$X) == nrow(fam_champi)

fam_champi$X[1:10]
var9_env$X[1:10]

#obs les datavar env X
str(var9_env)
var9_env$forest <- as.numeric(var9_env$forest)
var9_env$conifer <- as.numeric(var9_env$conifer)

#obs les data sp (fam)Y
str(fam_champi)
# (A) creer la RDA avec 9 variables -----
fam_champi_RDA <- rda (fam_champi[2:22] ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM, data = var9_env)
##Significativite : anova et R²----
anova(fam_champi_RDA)
#le modele global est significatif avec les 9 variables
anova(fam_champi_RDA, by = "axis")
#Axes 1 et 2, et dans une moindre mesure : Axe 3
anova (fam_champi_RDA, by = "terms")
# #pH, relEM, CN, pC, temperature annuelle moyenne 
anova(fam_champi_RDA, by = "margin")
#pH, abondance relative d'arbres ectomychrozhiziens, puis presence de foret et ratio C/N  pour les marginaux

RsquareAdj(fam_champi_RDA)
# 23% ajuste de variance expliquee (29% sinon)
##plot simple ---- 
plot(fam_champi_RDA)
#par couche 
plot(fam_champi_RDA, display = "sites")
plot(fam_champi_RDA, display = "species")
plot(fam_champi_RDA, display = "bp")
plot(fam_champi_RDA, display = "reg")
plot(fam_champi_RDA, display = "all")
plot(fam_champi_RDA, display = c("species", "bp"))


##test des plots ----
autoplot(fam_champi_RDA,layers=c("X","biplot"))+
  geom_hline(yintercept=0,linetype=2)+ 
  geom_vline(xintercept=0,linetype=2)+ 
  theme_bw()

#effet des contraintes 
#ds l'ordre de l'ordination rda
autoplot(fam_champi_RDA,layers=c("biplot"), arrows=FALSE)+
  geom_hline(yintercept=0,linetype=2)+ 
  geom_vline(xintercept=0,linetype=2)+ 
  theme_bw()

#(B) RDA max a 13 variables (c° en nutriments) -----
fam_champi_RDA2 <- rda(fam_champi[2:22] ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, data = var13_env)
anova(fam_champi_RDA2)#modele global significatif (un peu + que celui a 9 variables)
anova(fam_champi_RDA2, by = "axis")#axes 1, 2 et un peu 3
anova(fam_champi_RDA2, by = "terms")#NAOVA TYPE I
#pH, relEM, cn, forest (p=0.001)*** puis : pc(p=0.002), K (p=0.008) et mat (p=0.009)** puis connifer (p=0.44)*
anova(fam_champi_RDA2, by = "margin")#anova type II
#relEM, pH, cn, forest

RsquareAdj(fam_champi_RDA2)#25% de la variance expliquee en ajustee (33% sinon)
###plot simple ---- 
plot(fam_champi_RDA2)
#par couche 
plot(fam_champi_RDA2, display = "sites")
plot(fam_champi_RDA2, display = "species")
plot(fam_champi_RDA2, display = "bp")
plot(fam_champi_RDA2, display = "reg")
plot(fam_champi_RDA2, display = "all")
plot(fam_champi_RDA2, display = c("species", "bp", "sites"))

#n esont pas sign dans les mod testes : map, NPP, legerement conifer et P, Ca, Mg

# (C) test RDA a 6 variables (c° en nutri) ----
fam_champi_RDA_nutri <- rda(fam_champi[2:22] ~ pC + cn +P + K + Ca + Mg, data = var13_env)
anova(fam_champi_RDA_nutri, by = "margin")#
#K, Ca, Mg tres sign ; pC et cn moyen; Phosphore pas du tout

# (D) test RDA a 4 variables (c° en nutri) ----
fam_champi_RDA_nutri2 <- rda(fam_champi[2:22] ~ +P + K + Ca + Mg, data = var13_env)
anova(fam_champi_RDA_nutri2, by = "margin")#P n'est tjr pas sign

##test Phosphore  ----
fam_champi_RDA_nutriq <- rda(fam_champi[2:22] ~ P , data = var13_env)
anova(fam_champi_RDA_nutriq, by = "margin")#NON
anova(fam_champi_RDA_nutriq)
RsquareAdj(fam_champi_RDA_nutriq)

#ANOVA DE TYPE I(by terms) : l'effet de la C° en Phosphore n'est significatif dans aucun des modeles testes, avec qq var de C° nutri ou avec plein : meme comme seul var explicatif il n'est pas sign
#ANOVA DE TYPE II (by margins): idem



#(E) RDA sans col other -----

#supprimer la colonne other du dataset, pour ne garder que les colonnes des familles connues de champignon
famconnu_champi <-  select(fam_champi, Mortierellaceae : Helotiales_fam_Incertae_sedis)#prendre ts les noms de famille champi sauf "other"
famconnu_champiS <- cbind (fam_champi$X, famconnu_champi)#rajouter la colonne X
#test dim
dim(fam_champi)
dim(famconnu_champiS)
#creer la RDA 
famconnu_champiS_RDA <- rda(famconnu_champiS[2:21] ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, data = var13_env)
anova(famconnu_champiS_RDA, by = "terms")
anova(famconnu_champiS_RDA, by = "margin")
#plot 
plot(famconnu_champiS_RDA)
#par couche 
plot(famconnu_champiS_RDA, display = "sites")
plot(famconnu_champiS_RDA, display = "species")
plot(famconnu_champiS_RDA, display = "bp")
plot(famconnu_champiS_RDA, display = "reg")
plot(famconnu_champiS_RDA, display = "all")
plot(famconnu_champiS_RDA, display = c("species", "bp"))
#r²
RsquareAdj(famconnu_champiS_RDA)#explique 22% de la variance qd ajuste (30% sinon) 

#(F) SELECTION DE VAR ----
#test correlations -----
cor(var13_env[,2:13])<0.7
#pas de correlation ;) 

#les 4 var significatives dans le modele max (test en anova de type II)
RDA_4var_famconnu <- rda(famconnu_champiS[2:21] ~ cn + pH + map + relEM , data = var13_env)


#6) nom des var env ---- 
# Percentage of carbon (pC)
fam_champi_RDA2 <- rda(fam_champi[2:22] ~ pC + cn + pH + NPP + map + mat + forest + conifer + relEM +P + K + Ca + Mg, data = var13_env)

# 
# Soil ratio of carbon to nitrogen (cn)
# 
# Net primary productivity (NPP)
# 
# Mean annual precipitation (map)
# 
# Mean annual temperature (mat)
# 
# Soil pH (pH)
# 
# Presence of forest vegetation (forest)
# 
# Presence of conifers (conifer)
# 
# Relative abundance of ectomycorrhizal trees (relEM)

