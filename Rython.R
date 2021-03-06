##OJO antes de ejecutar esto hay que activar el entorno virtual
## source ~/env/Rython/bin/activate
if( any(grepl("package:reticulate" ,search()))){detach("package:reticulate")}
Sys.setenv(RETICULATE_PYTHON="/home/fpsanz/Rython/bin/python3")
library(reticulate)
library(tidyverse)
library(EBImage)

#reticulate::use_virtualenv("~/.virtualenvs/Rython/")
#use_python("/usr/bin/python3")
source_python("functions.py", convert = TRUE)
source("utils.R")

#Crear instancia clase imagen
imagenNew <- Imagen(NULL)
imagenNew$image <- "./images/01_X100.jpg"
imagenNew$set_NUM(10)

#leer imagen en R directamente
imagenR <- readImage(imagenNew$image )
EBImage::display(imagenR, method = "raster")

#Imagen from python
cargarImagen(imagenNew, imagenNew$image)

imgR <- py_2_R_imageColor(imagenNew$imageRGB)
display(imgR, method = "raster")

# definir puntos de entrenamiento
va <- locator(n = imagenNew$NUM, type = "p")
nova <- locator(n = imagenNew$NUM, type = "p")
vac <- featureCoord(va)
novac <- featureCoord(nova)
vac <- unname(vac)
vac <- r_to_py(vac, convert = FALSE)
novac <- unname(novac)
novac <- r_to_py(novac)
## asignar puntos a objeto 
imagenNew$set_xy(vac, novac)

#generar vector de caracteristicas
fvp = computeFeatureVector(imagenNew)

#crear clasificador
clf = trainKnn(imagenNew)
clf = trainSVM(imagenNew)


#load model
imagenNew$clf = loadModel("/datos/repos/visionGUI/knnModel")
imagen = imagenNew
im = np.dstack( (imagen.imageRGB, imagen.imageCIE) )
im = np.reshape(im, ((imagen.imageRGB.shape[0] * imagen.imageRGB.shape[1]), 6))
clf = imagen.clf
B = clf.predict(im)
B = np.reshape(B, (imagen.imageRGB.shape[0], imagen.imageRGB.shape[1]) )


#Clasificar imagen
B = classImage(imagenNew)
imagenNew$B = B

#visualizar la clasificacion
BR <- py_2_R_imageBW(B)
display(BR, method = "raster")

#Crear etiquetas/watersheed/medirAreas
statistics(imagenNew)

#Thresholding
thr = 2000 # se pasará por según valor deslizador
threshold(imagenNew, thr)
imagenNew$ratio

#mask image
maskImage(imagenNew)
maskR <- py_2_R_imageColor(imagenNew$masked)
display(maskR, method = "raster")

#guardar modelo en fichero
saveModel(imagenNew, "knnModel")

#leer model from file
imagenNew$clf = loadModel("knnModel")




