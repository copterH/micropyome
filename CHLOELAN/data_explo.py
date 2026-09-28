# -*- coding: utf-8 -*-
"""
Created on Fri Sep 25 15:05:41 2026

@author: edes
"""
# %% Importation des librairies


import pandas as pd
import random as rd
#import matplotlib as plt



# %% Importation des données avec pandas

fam_fungi = pd.read_csv('observed.csv', sep=',', index_col=0)
env13 = pd.read_csv('13_variables.csv', sep=',', index_col=0)
env09 = pd.read_csv('09_variables.csv', sep=',', index_col=0)

# %% Retrait de la catégorie 'Others'

fam_fungi2 = fam_fungi.drop("other", axis=1)

# %% Exploration des données visualisation

fam_fungi2.loc['SRR1502337'].plot.bar()


# %% Séparation des données

n = len(fam_fungi2)
ratio = 0.9
L = [ i for i in range(len(fam_fungi2)) ]
L_calibration = rd. sample(L, int(ratio*n))
L_calibration.sort()

L_test = []
for i in range(n):
    if i  not in L_calibration:
        L_test.append(i)

dta_calibration = fam_fungi2.iloc[L_calibration]
dta_test = fam_fungi2.iloc[L_test]

print(dta_calibration)
print(dta_test)

# %% En cours

