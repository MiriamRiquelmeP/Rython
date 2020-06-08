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
from skimage import io, filters, data, morphology, color
from sklearn.neighbors import NearestCentroid
from skimage import measure, util
import matplotlib.patches as patches
from scipy import ndimage as ndi
from skimage import segmentation
from skimage.feature import peak_local_max
from scipy import stats
import math


class Imagen:
    def __init__(self, image):
        self.image = image
        self.NUM = None
        self.imageRGB = None
        self.imageCIE = None
        self.x = None
        self.y = None
        self.fvp = None
        self.clf = None
        self.B = None
        self.label = None
        self.properties = None
        self.wtshed = None
        self.areas = None
        self.stat = None
        self.minArea = None
        self.maxArea = None
        self.totArea = None
        self.media = None
        self.std = None
        self.percarea = None
        self.bigAreas = None
        self.bigMean = None
        self.bigRange = None
        self.ratio = None
        self.bigPercent = None
        self.recoded = None
        self.masked = None
        
    def loadImage(self):
        self.loadimage = io.imread(self.image)
    def set_NUM(self, NUM):
        self.NUM = NUM
    def set_xy(self, x, y):
        self.x = x
        self.y = y
    def set_fvp(self, fvp):
        self.fvp = fvp
    def set_clf(self, clf):
        self.clf = clf
    def set_imageClass(self, B):
        self.B = B
        self.B = np.where(self.B == 1, 255, self.B)
    def set_label(self, label):
        self.label = label
    def set_properties(self, properties):
        self.properties = properties
    def set_watershed(self, watershed):
        self.wtshed = watershed
    def set_areas(self, areas):
        self.areas = areas
    def set_stat(self, stat):
        self.stat = stat
    def set_minArea(self, minArea):
        self.minArea = minArea
    def set_maxArea(self, maxArea):
        self.maxArea = maxArea
    def set_totArea(self, totarea):
        self.totArea = totarea
    def set_media(self, media):
        self.media = media
    def set_std(self, std):
        self.std = std
    def set_percarea(self, percarea):
        self.percarea = percarea
    def set_bigAreas(self, bigAreas):
        self.bigAreas = bigAreas
    def set_bigMean(self, bigMean):
        self.bigMean = bigMean
    def set_bigRange(self, bigRange):
        self.bigRange = bigRange
    def set_ratio(self, ratio):
        self.ratio = ratio
    def set_bigPercent(self, bigPercent):
        self.bigPercent = bigPercent
    def set_recoded(self, recoded):
        self.recoded = recoded
    def set_masked(self, masked):
        self.masked = masked
        
def cargarImagen(imagen, filename):
    imagen.imageRGB = io.imread(filename)
    imagen.imageCIE = color.rgb2lab(imagen.imageRGB)

def showImage(rgbimagen):
    plt.imshow(rgbimagen)
    plt.show()
    return

def computeFeatureVector(imagen):
    # imagen: objeto de la clase Image
    imageRGB = imagen.imageRGB
    imageCIE = imagen.imageCIE
    NUM = imagen.NUM
    x = imagen.x
    y = imagen.y
    fvp = np.zeros(((int(NUM) * 2), 6))  # definir vector de caracteristicas de NUM*2 puntos
    n = 0
    for i, j in zip(x, y):
        xrgb = imageRGB[int(round(i[1])), int(round(i[0])),]
        xcie = imageCIE[int(round(i[1])), int(round(i[0])),]
        brgb = imageRGB[int(round(j[1])), int(round(j[0])),]
        bcie = imageCIE[int(round(j[1])), int(round(j[0])),]
        fvp[n,] = np.concatenate((xrgb,xcie))
        index = int(NUM+n)
        fvp[index, ] = np.concatenate((brgb,bcie))
        n += 1
    imagen.set_fvp(fvp)
    return(fvp)


def trainKnn(imagen):
    # fvp: objeto generado por computeFeatureVector
    # NUM: numero de puntos de entrenamiento, dasignado por objeto Image.NUM
    fvp = imagen.fvp
    NUM = imagen.NUM
    clf = NearestCentroid()
    labels = [1] * int(NUM) + [0] * int(NUM)
    clf.fit(fvp, labels)
    imagen.set_clf(clf)
    return(clf)


def parallelizeFunction(param):
    i = param[0]
    j = param[1]
    rgb = imageng.imageRGB[i, j]
    cie = imageng.imageCIE[i, j]
    predecir = rgb.tolist() + cie.tolist()
    pr = imageng.clf.predict([predecir])
    return pr[0]


def classImage(imagen):
    global imageng  # esto es para que parallelizeFunction pueda acceder a la imagen
    imageng = imagen
    a = range(imagen.imageRGB.shape[0])
    b = range(imagen.imageRGB.shape[1])
    paramlist = list(itertools.product(a, b))
    pool = mp.Pool()
    res = pool.map(parallelizeFunction, paramlist)
    A = np.array(res)
    B = np.reshape(A, (imagen.imageRGB.shape[0], imagen.imageRGB.shape[1]))
    return B


def measureArea(imagen):
    label = measure.label(imagen.B, connectivity=1, background=0)
    dropsFiltered = morphology.remove_small_objects(label, 500, 1)
    dropsFiltered[dropsFiltered != 0] = 1
    label = measure.label(dropsFiltered, connectivity=1, background=0)
    #props = measure.regionprops(label)
    # print(props[1].area, props[1].label)
    imagen.set_label(label)
    watershed(imagen)
    props2 = measure.regionprops(imagen.wtshed)
    imagen.set_properties(props2)
    return

def watershed(imagen):
    distance = ndi.distance_transform_edt(imagen.label)
    distance1 = morphology.erosion(distance)
    distance1 = morphology.erosion(distance1)
    local_maxi = peak_local_max(distance1, labels=imagen.label, min_distance=10, indices=False)
    markers = ndi.label(local_maxi)[0]
    wtshed = segmentation.watershed(-distance1, markers, mask=imagen.label, watershed_line=True)
    imagen.set_watershed(wtshed)
    return

def statistics(imagen):
    measureArea(imagen)
    props2 = imagen.properties
    areas = list()
    for i in props2:
        areas.append(i.area)
    st = stats.describe(areas)
    imagen.set_areas(areas)
    imagen.set_stat(st)
    media = st[2].round(3)
    std = math.sqrt(st[3]).__round__(3)
    totarea=sum(areas)
    percarea=((sum(areas) / imagen.wtshed.size) * 100).round(3)
    minArea=min(areas)
    maxArea=max(areas)
    imagen.set_minArea(minArea)
    imagen.set_maxArea(maxArea)
    imagen.set_totArea(totarea)
    imagen.set_media(media)
    imagen.set_std(std)
    imagen.set_percarea(percarea)
    return

def threshold(imagen, thr):
    threshold = thr
    npareas = np.array(imagen.areas)
    small = npareas[npareas <= threshold]
    big = npareas[npareas > threshold]
    ratio = sum(small) / sum(big)
    ratio = ratio.__round__(3)
    bigAreas = np.nonzero(npareas < threshold)
    recoded = imagen.wtshed
    if bigAreas[0].size != 0:
         for j in np.nditer(bigAreas):
            recoded = np.where(recoded == (j+1), 0, recoded)
    props3 = measure.regionprops(recoded)
    recoAreas = list()
    for k in props3:
        recoAreas.append(k.area)
    stBig = stats.describe(recoAreas)
    bigMean = stBig[2]
    bigRange = stBig[1]
    bigPercent = (sum(recoAreas) / imagen.wtshed.size).round(3)
    imagen.set_ratio(ratio)
    imagen.set_bigAreas(bigAreas)
    imagen.set_bigRange(bigRange)
    imagen.set_bigMean(bigMean)
    imagen.set_bigPercent(bigPercent)
    imagen.set_recoded(recoded)
    return
    
def maskImage(imagen):
    mask = imagen.recoded == 0
    masked = (imagen.imageRGB).copy()
    masked[mask] = 0
    imagen.set_masked(masked)
    return
    
def saveModel(imagen, filename):
    with open(filename, "wb") as fileToSave:
        pickle.dump(imagen.clf, fileToSave)
    return

def loadModel(filename):
    with open(filename, "rb") as fileToOpen:
        clf = pickle.load(fileToOpen)
    return(clf)





