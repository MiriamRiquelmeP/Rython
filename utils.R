#system("source ~/Rython/bin/activate")


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


kk <- "
digraph a_nice_graph {

graph [splines=polyline, bgcolor='#ecf0f5', ratio=1.5]

#node color
node[fillcolor='@@1', fontcolor='@@14', style=filled, label='Load Image', fontsize=9, shape=oval]
a
node[fillcolor='@@2', fontcolor='@@15', style=filled, label='Manual train']
b
node[fillcolor='@@3', fontcolor='@@16', style=filled, label='Train from model']
c
#node[fillcolor='@@4', fontcolor='@@17', style=filled, label='Load feature vector']
#d
node[fillcolor='@@5', fontcolor='@@18', style=filled, label='Load fitted model']
e
node[fillcolor='@@6', fontcolor='@@19', style='filled,dashed', label='Mark points']
f
node[fillcolor='@@7', fontcolor='@@20', style='filled,dashed', label='Generate feature vector']
g
node[fillcolor='@@8', fontcolor='@@21', style=filled, label='Select algorithm']
h
node[fillcolor='@@9', fontcolor='@@22', style='filled,dashed', label='Train model']
i
node[fillcolor='@@10', fontcolor='@@23', style='filled,dashed', label='Classify image']
j
node[fillcolor='@@11', fontcolor='@@24', style=filled, label='View results']
k
#node[fillcolor='@@12', fontcolor='@@25', style=filled, label='Save feature vector', shape=record]
#l
node[fillcolor='@@13', fontcolor='@@26', style=filled, label='Save model', shape=record]
m

# edge definitions with the node IDs
a -> {b,c}
c -> {e}
b -> f
f -> g
g -> h
h -> i
i -> j
j -> k
#g -> l [style=dashed, color=grey]
i -> m [style=dashed, color=grey]
#d -> h
e -> j
{rank=same; b;c}
#{rank=same; d;e}
#{rank=same; l;g} 
#{rank=same; m;i}
}
"
col1 <- "'#ecf0f5'"
col2 <- "'#ecf0f5'"
col3 <- "'#ecf0f5'"
col4 <- "'#ecf0f5'"
col5 <- "'#ecf0f5'"
col6 <- "'#ecf0f5'"
col7 <- "'#ecf0f5'"
col8 <- "'#ecf0f5'"
col9 <- "'#ecf0f5'"
col10 <- "'#ecf0f5'"
col11 <- "'#ecf0f5'"
col12 <- "'#ecf0f5'"
col13 <- "'#ecf0f5'"

fcol1 <- "'#222d32'"
fcol2 <- "'#222d32'"
fcol3 <- "'#222d32'"
fcol4 <- "'#222d32'"
fcol5 <- "'#222d32'"
fcol6 <- "'#222d32'"
fcol7 <- "'#222d32'"
fcol8 <- "'#222d32'"
fcol9 <- "'#222d32'"
fcol10 <- "'#222d32'"
fcol11 <- "'#222d32'"
fcol12 <- "'#222d32'"
fcol13 <- "'#222d32'"

