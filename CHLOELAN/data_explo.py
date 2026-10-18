# -*- coding: utf-8 -*-
"""
Created on Fri Sep 25 15:05:41 2026

@author: edes
"""
#%% Importation des librairies

import pandas as pd
import random as rd
import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import Lasso

# %% Importation des données par famille avec pandas

fam_fungi = pd.read_csv('observed.csv', sep=',', index_col=0)
env13 = pd.read_csv('13_variables.csv', sep=',', index_col=0)
env09 = pd.read_csv('09_variables.csv', sep=',', index_col=0)

# %% Importation des données par fonction avec pandas

fg_fungi = pd.read_csv('fg_observed.csv', sep=',', index_col=0)


# %% Retrait de la catégorie 'Others'

fam_fungi2 = fam_fungi.drop("other", axis=1)
# On retire les 'other' car perte de signification
# Les champignons de cette catégorie sont soit non identifiés soit trop rares
# Ainsi il s'agit d'un mix très variable d'environnement, ce qui amène à une confusion

# %% Exploration des données visualisation

fam_fungi2.loc['SRR1502337'].plot.bar()

fg_fungi.loc['SRR1502674'].plot.bar()

# %% Séparation des données

# L_calibration est la liste des indices des sites qui se retrouvent
# dans le jeu de données de calibration
N = len(fg_fungi)
ratio = 0.9
L = [ i for i in range(len(fg_fungi)) ]
L_calibration = rd. sample(L, int(ratio*N))
L_calibration.sort()

# L_test est la liste des indices des sites qui se retrouvent
# dans le jeu de données de test. Elle est construite en opposition
# à ce qui est déjà présent dans L_calibration
L_test = []

def setdiff(Lin, Lnew, N):
    for i in range(N):
        if i  not in Lin:
            Lnew.append(i)

setdiff(L_calibration, L_test, N)

# On construit les DataFrame d'abondance de calibration et de test
# à partir de L_calibration et de L_test
abund_cal = fg_fungi.iloc[L_calibration]
abund_test = fg_fungi.iloc[L_test]

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



# %% Visualisation des valeurs d'abondance de la famille i pour chaque site
abund_cal["Mortierellaceae"].plot.bar()

# %% Régression lasso

lasso = Lasso(alpha=0.0001)
lasso.fit(env09_cal, abund_cal)
# rmse_lasso = np.sqrt(np.mean((yval-lasso3.predict(X3val))**2))
# print("order 3 (lambda = 0.1) = " + str(np.round(rmse3_lasso,3)))
# print("order 3 = "+str(np.round(rmse3_val,3)))

# %% En cours

# Définir les paramètres
B = 100 # Var choisie
n = len(abund_cal)
n_train = 2/3 * n

# Fonctions à définir

def distance(data): # fonction qui renvoie une matrice de distance
# distance peut être euclidienne
# pcoa permet d'avoir d'autres types de distance
   print(n)


# PCoA

def pcoa(x): # fonction qui réalise la pcoa à partir d'une matrice de distance
    print(n)

# onrécupère pour chaque 

# Boucle 
for i in range(1,B):
    
    # Séparation des échantillons
    L_train = rd.sample([i for i in range(n+1)], n_train)
    L_validation = setdiff([i for i in range(n+1)], L_train)
    
    # Entrainement du modèle sur les données
    
    
    # Calcul des 
    D = distance(fg_fungi[L_train,])
    PCoA = pcoa(D)
    res = lm(pcoa[,1])




# %%
# Importing necessary libraries
import numpy as np
from sklearn.metrics import pairwise_distances
from sklearn.manifold import MDS
import matplotlib.pyplot as plt

# Sample data: Random data matrix
data = np.random.rand(10, 5)  # 10 samples, 5 variables

# Computing pairwise distances
distances = pairwise_distances(data, metric='euclidean')

# PCoA using MDS
mds = MDS(n_components=2, dissimilarity='precomputed', random_state=42)
coordinates = mds.fit_transform(distances)

# Extracting components from embedding_
components = mds.embedding_.transpose()

# Plotting the biplot
plt.figure(figsize=(10, 8))
plt.scatter(coordinates[:, 0], coordinates[:, 1], color='blue', label='Samples')

# Adding variable loadings as arrows
for i in range(data.shape[1]):
    plt.arrow(0, 0, components[0, i], components[1, i], color='red', alpha=0.5,
              head_width=0.05, head_length=0.05)
    plt.text(components[0, i] * 1.15, components[1, i] * 1.15,
             'Variable {}'.format(i+1), color='red')

# Adding sample labels
for i in range(len(coordinates)):
    plt.text(coordinates[i, 0], coordinates[i, 1], 'Sample {}'.format(i+1), fontsize=9)

# Adding labels and title
plt.xlabel('PCo1')
plt.ylabel('PCo2')
plt.title('Biplot of PCoA')

plt.grid(True)
plt.legend()
plt.show()