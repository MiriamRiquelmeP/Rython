from itertools import combinations
import numpy as np
import matplotlib.pyplot as plt
from skimage import io, filters, data, morphology, color, measure
from sklearn.neighbors import NearestCentroid
from sklearn import svm
from sklearn.ensemble import RandomForestClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.naive_bayes import GaussianNB
import itertools
import multiprocessing as mp
import cv2
import sys
import pickle
from PIL import Image
from sklearn.neighbors import NearestCentroid
from skimage import measure, util
import matplotlib.patches as patches
from scipy import ndimage as ndi
from skimage import segmentation
from skimage.feature import peak_local_max
from scipy import stats
import math
from joblib import Parallel, delayed

imagen <- Imagen(NULL)
imagen$image <- "./pruebawater.jpg"

imagen.B = imagen$image  
label = measure.label(imagen.B, connectivity=1, background=0)
dropsFiltered = morphology.remove_small_objects(label, 500, 1)
dropsFiltered[dropsFiltered != 0] = 1
label = measure.label(dropsFiltered, connectivity=1, background=0)
#props = measure.regionprops(label)
# print(props[1].area, props[1].label)
imagen.set_label(label)
#watershed(imagen)
distance = ndi.distance_transform_edt(imagen.label)
distance1 = morphology.erosion(distance)
distance1 = morphology.erosion(distance1)
local_maxi = peak_local_max(distance1, labels=imagen.label, min_distance=10, indices=False)
markers = ndi.label(local_maxi)[0]
wtshed = segmentation.watershed(-distance1, markers, mask=imagen.label, watershed_line=True)
imagen.set_watershed(wtshed)



props2 = measure.regionprops(imagen.wtshed)
imagen.set_properties(props2)