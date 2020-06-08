#system("./activate.sh")


py_2_R_imageColor <- function(x){
    require(EBImage)
    # funcion para convertir array de imagen python a R
    # traspone la matrix para que quede igual
    # porque R va por filas y python por columnas
    x01 <- aperm(x, c(2,1,3))
    x01 <- as.vector(x01)/255
    Rimage <- Image(x01, dim = c(dim(x)[2],dim(x)[1], dim(x)[3]), colormode = "Color" ) 
    return(Rimage)
}

py_2_R_imageBW <- function(x){
    require(EBImage)
    # funcion para convertir array de imagen python a R
    # traspone la matrix para que quede igual
    # porque R va por filas y python por columnas
    x01 <- aperm(x, c(2,1))
    x01 <- as.vector(x01)
    Rimage <- Image(x01, dim = c(dim(x)[2],dim(x)[1]), colormode = "Color" ) 
    return(Rimage)
}

featureCoord <- function(data, type){
    dfVac <- round(as.data.frame.list(data))
    listvac <- as.list(as.data.frame( t(dfVac)))
    return(listvac)
}

