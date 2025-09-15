# Analisis inicial del dataset de lineas de omnibus de Montevideo
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import geopandas as gpd
import folium
from folium.plugins import MarkerCluster
from shapely.geometry import Point, LineString
import contextily as ctx
import warnings
warnings.filterwarnings('ignore')

# Cargar el dataset
df = pd.read_csv('Lineas_Omnibus_Mvdeo/lineas_omnibus_mvd.csv')