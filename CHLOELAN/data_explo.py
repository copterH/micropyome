# -*- coding: utf-8 -*-
"""
Created on Fri Sep 25 15:05:41 2026

@author: edes
"""
# %% Importation des librairies


import pandas as pd


# %% Importation des données avec pandas

fam_fungi = pd.read_csv('observed.csv', sep=',')
env13 = pd.read_csv('13_variables.csv', sep=',')
env09 = pd.read_csv('09_variables.csv', sep=',')