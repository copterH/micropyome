# -*- coding: utf-8 -*-
"""
Created on Fri Sep 25 15:05:41 2026

@author: edes
"""
# %% Importation des librairies

import pandas as pd
import random as rd
import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import Lasso



# %% Importation des données avec pandas

fam_fungi = pd.read_csv('observed.csv', sep=',', index_col=0)
env13 = pd.read_csv('13_variables.csv', sep=',', index_col=0)
env09 = pd.read_csv('09_variables.csv', sep=',', index_col=0)

# %% Retrait de la catégorie 'Others'

fam_fungi2 = fam_fungi.drop("other", axis=1)
# On retire les 'other' car perte de signification
# Les champignons de cette catégorie sont soit non identifiés soit trop rares
# Ainsi il s'agit d'un mix très variable d'environnement, ce qui amène à une confusion

# %% Exploration des données visualisation

fam_fungi2.loc['SRR1502337'].plot.bar()


# %% Séparation des données

# L_calibration est la liste des indices des sites qui se retrouvent
# dans le jeu de données de calibration
n = len(fam_fungi)
ratio = 0.9
L = [ i for i in range(len(fam_fungi2)) ]
L_calibration = rd. sample(L, int(ratio*n))
L_calibration.sort()

# L_test est la liste des indices des sites qui se retrouvent
# dans le jeu de données de test. Elle est construite en opposition
# à ce qui est déjà présent dans L_calibration
L_test = []
for i in range(n):
    if i  not in L_calibration:
        L_test.append(i)

# On construit les DataFrame d'abondance de calibration et de test
# à partir de L_calibration et de L_test
abund_cal = fam_fungi2.iloc[L_calibration]
abund_test = fam_fungi2.iloc[L_test]

# On construit les DataFrame des données environnementales
# (respectivement 9 et 13 variables) de calibration et de test
# à partir de L_calibration et de L_test
env09_cal = env09.iloc[L_calibration]
env09_test = env09.iloc[L_test]

env13_cal = env13.iloc[L_calibration]
env13_test = env13.iloc[L_calibration]

print(abund_cal)
print(abund_test)

# On travaillera par la suite exclusivement avec les données de calibrations.
# Les données de test seront utilisées uniquement à la fin pour valider ou non le modèle


# %% On transforme les données pour que la somme de chaque ligne = 1


# %% Visualisation des valeurs d'abondance de la famille i pour chaque site
abund_cal["Mortierellaceae"].plot.bar()


# %% LM - Modèle linéaire : 1 par famille

Lm = LinearRegression()
Lm.fit(env09_cal, abund_cal)



# %% Régression lasso


lasso = Lasso(alpha=0.0001)
lasso.fit(env09_cal, abund_cal)
# rmse_lasso = np.sqrt(np.mean((yval-lasso3.predict(X3val))**2))
# print("order 3 (lambda = 0.1) = " + str(np.round(rmse3_lasso,3)))
# print("order 3 = "+str(np.round(rmse3_val,3)))






# %% En cours




