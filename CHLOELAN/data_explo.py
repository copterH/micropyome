# -*- coding: utf-8 -*-
"""
Created on Fri Sep 25 15:05:41 2026

@author: edes
"""
# %% Importation des librairies


import pandas as pd
#import matplotlib as plt



# %% Importation des données avec pandas

fam_fungi = pd.read_csv('observed.csv', sep=',', index_col=0)
env13 = pd.read_csv('13_variables.csv', sep=',', index_col=0)
env09 = pd.read_csv('09_variables.csv', sep=',', index_col=0)

# %% Retrait de la catégorie 'Others'

fam_fungi2 = fam_fungi.drop("other", axis=1)

# %% Exploration des données visualisation

fam_fungi2.loc['SRR1502337'].plot.bar()


