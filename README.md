# Instrucciones de instalación de Rython

## Instalar entorno virtual

### Instalar pip3

```bash
sudo apt-get install python3-pip
```

### Instalar virtualenv

```bash
sudo pip3 install virtualenv
```

### Crear entorno virtual

Desde tu home:

```bash
virtualenv Rython -p python3
```

debe crearse una carpeta en la ruta ~/Rython

### Activar entorno virtual

```bash
source ~/Rython/bin/activate
```

### Desactivar entorno virtual

```bash
deactivate
```

## Instalar los paquetes necesarios

Con el entorno virtual activado

```bash
pip3 install matplotlib
pip3 install scikit-learn
pip3 install scikit-image
pip3 install numpy
pip3 install pickle
pip3 install opencv-python
pip3 install PIL
pip3 install itertools
pip3 install multiprocessing
pip3 install scipy
```
