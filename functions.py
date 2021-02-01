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
from skimage.exposure import match_histograms

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
        self.model = None
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
        self.bigAreaSum = None
        
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
    def set_model(self, model):
        self.model = model
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
    def set_bigAreaSum(self, bigAreaSum):
        self.bigAreaSum = bigAreaSum

def _getThreads():
    if sys.platform == 'win32':
        return (int)(os.environ['NUMBER_OF_PROCESSORS'])
    else:
        return (int)(os.popen('grep -c cores /proc/cpuinfo').read())
     
def cargarImagen(imagen, filename):
    rawImage = io.imread(filename)
    reference = io.imread("images/reference.jpg")
    matched = match_histograms(rawImage, reference, multichannel = True)
    imagen.imageRGB = io.imread(filename)
    imagen.imageCIE = color.rgb2lab(imagen.imageRGB)
    del rawImage

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

def trainSVM(imagen):
    fvp = imagen.fvp
    NUM = imagen.NUM
    clf = svm.NuSVC(kernel="linear")
    labels = [1] * int(NUM) + [0] * int(NUM)
    clf.fit(fvp, labels)
    imagen.set_clf(clf)
    return clf

def trainRandomForest(imagen):
    fvp = imagen.fvp
    NUM = imagen.NUM
    clf = RandomForestClassifier(random_state=0, n_estimators=10)
    labels = [1] * int(NUM) + [0] * int(NUM)
    clf.fit(fvp, labels)
    imagen.set_clf(clf)
    return clf

def trainNeuralNetwork(imagen):
    fvp = imagen.fvp
    NUM = imagen.NUM
    clf = MLPClassifier(solver='lbfgs', alpha=1e-5, hidden_layer_sizes=(5, 2), random_state=1)
    labels = [1] * int(NUM) + [0] * int(NUM)
    clf.fit(fvp, labels)
    imagen.set_clf(clf)
    return clf

def trainNaiveBayes(imagen):
    fvp = imagen.fvp
    NUM = imagen.NUM
    clf = GaussianNB()
    labels = [1] * int(NUM) + [0] * int(NUM)
    clf.fit(fvp, labels)
    imagen.set_clf(clf)
    return clf

def parallelizeFunction(param):
    i = param[0]
    j = param[1]
    rgb = imageng.imageRGB[i, j]
    cie = imageng.imageCIE[i, j]
    predecir = rgb.tolist() + cie.tolist()
    pr = imageng.clf.predict([predecir])
    return pr[0]

def classImage(imagen):
    im = np.dstack((imagen.imageRGB, imagen.imageCIE))
    im = np.reshape(im, ((imagen.imageRGB.shape[0] * imagen.imageRGB.shape[1]), 6))
    clf = imagen.clf
    B = clf.predict(im)
    B = np.reshape(B, (imagen.imageRGB.shape[0], imagen.imageRGB.shape[1]) )
    return(B)


def measureArea(imagen):
    label = measure.label(imagen.B, connectivity=1, background=0)
    dropsFiltered = morphology.remove_small_objects(label, 500, 1)
    dropsFiltered[dropsFiltered != 0] = 1
    label = measure.label(dropsFiltered, connectivity=1, background=0)
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
    ratio = sum(big) / sum(small)
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
    imagen.set_bigAreaSum(sum(recoAreas))
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

def classModel(imagen):
    cores = [10,9,8,7,6,5,4,3,2,1]
    clf = imagen.clf
    im = np.dstack((imagen.imageRGB, imagen.imageCIE))
    im = np.reshape(im, ((imagen.imageRGB.shape[0] * imagen.imageRGB.shape[1]), 6))
    for i in cores:
        if( (im.shape[0] % i) == 0):
            core = i
            break
    imag = np.split(im, core)
    parallel = Parallel(n_jobs=core)
    res = parallel(delayed(clf.predict)(imag[i]) for i in range(core))
    res = np.concatenate(res)
    B = np.reshape(res, (imagen.imageRGB.shape[0], imagen.imageRGB.shape[1]) )
    return(B)




