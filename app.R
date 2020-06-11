library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)
library(shinydashboardPlus)
library(shinyjs)
library(shinythemes)
library(reticulate)
library(tidyverse)
library(ShinyImage)
library(EBImage)
library(DiagrammeR)
library(png)
library(shinyFiles)
source("utils.R")

reticulate::use_virtualenv("~/Rython/")
source_python("functions.py", convert = TRUE)

### HEADER ############ 
header <- dashboardHeader(title = "Image classifier tool", 
                          titleWidth = 300, 
                          dropdownMenuOutput("messageMenu"),
                          tags$li(class = "dropdown", actionButton("aboutButton", "About"),
                                  style="margin-top:8px; margin-right: 5px")
)

### SIDEBAR ##########
sidebar <- dashboardSidebar(useShinyalert(),
                            useShinyjs(),
                            sidebarMenu(
                              menuItem(
                                numericInput( "pixelsize",
                                           label=HTML("Pixel size (&mu;m/pixel)"),
                                           value = 0.2,
                                           step=0.01))
                            ),
                            sidebarMenu(
                              menuItem(
                                uiOutput("Image")
                              )),
                            sidebarMenu(menuItem(uiOutput("pickerOption"))),
                            sidebarMenu(menuItem(uiOutput("Model"))),
                            sidebarMenu(menuItem(uiOutput("algorithm"))),
                            sidebarMenu(menuItem(uiOutput("botonClass"))),
                            sidebarMenu(menuItem(uiOutput("botonResults"))),
                            tags$hr(style="border-color: white; margin-left: 10px; margin-right: 10px"),
                            sidebarMenu(
                              menuItem(
                                fluidRow(column(12, align = "center", offset = 0,
                                                uiOutput("fileName"),
                                                uiOutput("savefitmodel") ) ) ) )
                            )

### BODY ###############
body <- dashboardBody(
      # tags$head(
      #   tags$link(rel = "stylesheet", type = "text/css", href = "customDark.css")
      # ),
      setShadow(class = "shiny-plot-output"),
      setShadow( class = "box"),
      setShadow( class = "svg-container"),
      shiny::tagList(shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "busystyle.css"),
        shiny::tags$script(type = "text/javascript", src = "busy.js")
      )),
      div(
        class = "busy",
        #h4("Loading data, please be patient..."),
        img(src = "dna-svg-small-13.gif", style = "width: 150px"),
        style = "z-index: 99"
      ), 
      bsAlert("alert"),
      fluidPage( fluidRow( 
        column(width = 3,
               uiOutput("Thres"),
               grVizOutput("dg", height = "600px")),
      column(width=9, 
             #plotOutput("imagen", click = "plot_click"),
             uiOutput("image"),
             textOutput("texto"),
             tags$br(),
             htmlOutput("resultxt")
      ))
      )
)
     
  



########################################## UI #################################################

ui <- dashboardPage(title="Image classifier",
                    header,
                    sidebar,
                    body
) # fin del UI


########################################## SERVER #################################################

server <- function(input, output, session) {
  
  #crear instancia de la clase Imagen
  imagenNew <- Imagen(NULL)
  
  observeEvent(input$aboutButton, {
    shinyalert("Rython app 2020", "Authors:
               User interface design: Miriam Riquelme Pérez
            Algorithms implementation: Fernando Pérez Sanz",
               imageUrl = "dna-svg-small-13.gif", 
               imageWidth = 200, imageHeight = 100)})


  # Variables reactivas ##################
  uploadImage <- reactiveValues(ok = FALSE)
  shinyImageFile <- reactiveValues(shiny_img_origin = NULL)
  va <- reactiveValues(x = NULL, y = NULL)
  nova <- reactiveValues(x = NULL, y = NULL)
  counter <- 1
  train <- reactiveValues(ok = FALSE)
  fv <- reactiveValues(ok = FALSE)
  loadimage <- reactiveValues(ok = FALSE)
  manualTrain <- reactiveValues(ok = FALSE)
  selectAlgorithm <- reactiveValues(ok = FALSE)
  stat <- reactiveValues(ok = FALSE)
  tr <- reactiveValues(ok = FALSE)
  clf <- reactiveValues(ok = FALSE)
  cargarmodelo <- reactiveValues(ok=FALSE)
  
# Upload Image #################
output$Image <- renderUI({
  fileInput("imagenFile",
            "Upload image",
            placeholder = "image.png",
            accept = c(".png",".jpg") )
})

observeEvent(input$imagenFile,{
  uploadImage$ok <- TRUE ## valida que se ha cargado la imagen
})

# Elegir Opcion manual train / form model ################
output$pickerOption <- renderUI({
  validate(need(isTRUE(uploadImage$ok), ""))
    pickerInput(
      inputId = "option",
      label = "Choose the option",
      choices = list( "Manual train" = "opt1",
                      "Train from model" = "opt2"),
      options = list(title = "Option ..."),
      selected = NULL )
})

# render Contenedor de imagen ################
output$image <- renderUI({
  validate(need(!is.null(input$option), "" ) )
  if(input$option == "opt1"){
    manualTrain$ok <- TRUE
    cargarmodelo$ok <- FALSE
    stat$ok = FALSE
    tr$ok = FALSE
    shinyalert("Mark points into vacuole object")
    plotOutput("imagen", click = "plot_click", height = "685px")
  } else{
    manualTrain$ok <- FALSE
    fv$ok <- FALSE
    clf$ok <- FALSE
    stat$ok <- FALSE
    tr$ok <- FALSE
    plotOutput("imagen", height = "685px")
  }
})

# renombra imagen para que se llame como original #########
renameUpload <- function(inFile) {
    if(is.null(inFile))
      return(NULL)
    oldNames = inFile$datapath
    newNames = file.path(dirname(inFile$datapath), inFile$name)
    file.rename(from = oldNames, to = newNames)
    inFile$datapath <- newNames
    return(inFile$datapath)
  }

# renderiza Imagen ####################
observeEvent(input$option, {
    validate(need(!is.null(input$imagenFile),""))
    #validate(need(input$option,""))
    imagenNew$image <- renameUpload(input$imagenFile)
    imagenNew$set_NUM(10)
    cargarImagen(imagenNew, imagenNew$image)
    shinyImageFile$shiny_img_origin <- shinyimg$new(renameUpload(input$imagenFile))
    output$imagen <- renderPlot({shinyImageFile$shiny_img_origin$render()})
    loadimage$ok <- TRUE
    clf$ok <- FALSE
  })   

# mostrar flujograma #####
output$dg <- renderGrViz({
  validate(need(isTRUE(uploadImage$ok), ""))
  validate(need(!is.null(input$option), ""))
  if(isTRUE(loadimage$ok)){ col1="'#3c8dbc'"; fcol1="'#ffffff'" }
  if(isTRUE(manualTrain$ok)){col2="'#3c8dbc'"; fcol2="'#ffffff'"}
  if(isTRUE(fv$ok)){col6="'#3c8dbc'"; fcol6="'#ffffff'";col7="'#3c8dbc'"; fcol7="'#ffffff'" }
  if(isTRUE(tr$ok)){col8="'#3c8dbc'"; fcol8="'#ffffff'"; col9="'#3c8dbc'"; fcol9="'#ffffff'"}
  if(isTRUE(clf$ok)){col10="'#3c8dbc'"; fcol10="'#ffffff'"}
  if(isTRUE(stat$ok)){col11="'#3c8dbc'"; fcol11="'#ffffff'"}
  if(input$option=="opt2"){col3="'#3c8dbc'"; fcol3="'#ffffff'"}
  if(isTRUE(cargarmodelo$ok)){col5="'#3c8dbc'"; fcol5="'#ffffff'"}
  qq <- paste0(
    "[1]: ",col1,
    "\n[2]: ",col2,
    "\n[3]: ",col3,
    "\n[4]: ",col4,
    "\n[5]: ",col5,
    "\n[6]: ",col6,
    "\n[7]: ",col7,
    "\n[8]: ",col8,
    "\n[9]: ",col9,
    "\n[10]: ",col10,
    "\n[11]: ",col11,
    "\n[12]: ",col12,
    "\n[13]: ",col13,
    "\n[14]: ",fcol1,
    "\n[15]: ",fcol2,
    "\n[16]: ",fcol3,
    "\n[17]: ",fcol4,
    "\n[18]: ",fcol5,
    "\n[19]: ",fcol6,
    "\n[20]: ",fcol7,
    "\n[21]: ",fcol8,
    "\n[22]: ",fcol9,
    "\n[23]: ",fcol10,
    "\n[24]: ",fcol11,
    "\n[25]: ",fcol12,
    "\n[26]: ",fcol13
  )
  k <- c(kk, qq )  
  grViz(k, engine = "dot")
})

# salvar modelo ############################
output$savefitmodel <- renderUI({
  validate(need(isTRUE(tr$ok),""))
  validate(need(input$filename!="", ""))
  #actionButton("saveModel", label = "Save model")
  shinyDirButton("saveModel", "Folder select", "Select a folder")
})

output$fileName <- renderUI({
  validate(need(isTRUE(tr$ok),""))
  textInput("filename", "Model name to save")
})

roots <- getVolumes()()[1]
shinyDirChoose(input, "saveModel", roots = roots )

observeEvent(input$saveModel,{
  validate(need(input$filename!="","") )
  validate(need(is.list(input$saveModel), ""))
  path <- parseDirPath( roots, input$saveModel)
  file <- paste0(path,"/",input$filename)
  saveModel(imagenNew, file)
})
######    ######       #####

# cargar modelo ###########################
output$Model <- renderUI({
  validate(need(input$option=="opt2", ""))
   fileInput("filemodel",
             "Select model file",
             placeholder = "model")
 })


observeEvent(input$filemodel, {
  filepath <- input$filemodel
  if(is.null(filepath)){return(NULL)}
  imagenNew$clf = loadModel(filepath$datapath)
  cargarmodelo$ok <- TRUE #
  B = classImage(imagenNew)
  imagenNew$B = B
  clf$ok <- TRUE #
  BR <- py_2_R_imageBW(B)
  output$imagen <- renderPlot({display(BR, method = "raster")})
  
})

# marcar puntos de entrenamiento manual ##############
observeEvent(input$plot_click,{
  if(counter <= (imagenNew$NUM*2) ){
    if(counter <= imagenNew$NUM){
      va$x <- append(va$x, input$plot_click$x)
      va$y <- append(va$y, input$plot_click$y) } else{
      nova$x <- append(nova$x, input$plot_click$x)
      nova$y <- append(nova$y, input$plot_click$y)
      }
    if(counter == imagenNew$NUM){
      shinyalert("mark non vacuole points")
    }
    counter <<- counter + 1
  } else{
      shinyalert("Finished mark points. Continue with the analysis")
      train$ok <- TRUE
    }
})

# mostrar datos parte inferior ###############
output$texto <- renderText({
    validate(need(input$plot_click, ""))
        paste0("x: ",round(input$plot_click$x), " , ",
               "y: ",round(input$plot_click$y),
               "; Total points :", counter-1,
               "; vac: ",length(va$x),
               "; novac: ",length(nova$x) )
    })
# feature vector ###################
observeEvent(train$ok,{
  if( isTRUE(train$ok) ){
    vac <- featureCoord(list(x = va$x, y=va$y ) )
    novac <- featureCoord(list(x = nova$x, y=nova$y ))
    vac <- unname(vac)
    vac <- r_to_py(vac, convert = FALSE)
    novac <- unname(novac)
    novac <- r_to_py(novac)
    imagenNew$set_xy(vac, novac)
    fvp = computeFeatureVector(imagenNew)
    fv$ok <- TRUE
  }
})
# select algorithm ######################
output$algorithm <- renderUI({
  validate(need(isTRUE(fv$ok), ""))
    pickerInput(
      inputId = "algoritmo",
      label = "Choose algorithm",
      choices = list("Knn" = "knn",
                     "SVM" = "svm",
                     "Bayes" = "bayes",
                     "ANN" = "ann"),
      options = list(title = "Option ..."),
      selected = NULL
    )
})


# train model ###############################
observeEvent(input$algoritmo,{
  validate(need(input$algoritmo,""))
  if( input$algoritmo == "knn"){
    train = trainKnn(imagenNew)
    tr$ok <- TRUE
  }
})

# render boton class #######################
output$botonClass <- renderUI({
  validate(need(isTRUE(tr$ok),""))
  actionButton(inputId = "buttonClass", label="Classify")
})

# clasifica imagen ######################
observeEvent(input$buttonClass, {
  if(isTRUE(tr$ok)){
    B = classImage(imagenNew)
    imagenNew$B = B
    clf$ok <- TRUE
    BR <- py_2_R_imageBW(B)
    output$imagen <- renderPlot({display(BR, method = "raster")})
  }
})

# render boton resultados ################
output$botonResults <- renderUI({
  validate(need(isTRUE(clf$ok),""))
  actionButton(inputId = "buttonResults", label="View Results")
})


# acciones al ejecutar estadísticas ##########
observeEvent(input$buttonResults,{
  statistics(imagenNew)
  stat$ok <- TRUE
})

output$Thres <- renderUI({
  validate(need(isTRUE(stat$ok), ""))
  pixelSize <- input$pixelsize
  min <- imagenNew$minArea *(pixelSize^2)
  max <- imagenNew$maxArea *(pixelSize^2)
  value <- (max-min)/2
  sliderInput("thres", HTML("Select Macro-area (standard value 144 &mu;m2)"), min =min , max = max, value=144, step=10, width="100%")
})

observeEvent(input$thres,{
  #validate(need(input$thres,""))
  validate(need(isTRUE(stat$ok),"" ) )
  pixelSize <- input$pixelsize
  thr = input$thres / (pixelSize^2) # se pasará por según valor deslizador
  threshold(imagenNew, thr)
  imagenNew$ratio
  maskImage(imagenNew)
  maskR <- py_2_R_imageColor(imagenNew$masked)
  output$imagen <- renderPlot({display(maskR, method = "raster")}) 
})

output$resultxt <- renderUI({
  validate(need(isTRUE(stat$ok),"" ) )
  validate(need(input$thres,"" ) )
  conversion <- (input$pixelsize^2)
  texto <- paste(paste0("Mean big size: ",round( (imagenNew$bigMean * conversion),2) ),
                 paste0("Total area big vacuole: ",round((imagenNew$bigAreaSum * conversion),2) ),
                 paste0("Percent area big: ",round(imagenNew$bigPercent*100,2)," %" ),
                 paste0("Big/small ratio: ", imagenNew$ratio ),
                 paste0("Big vacuole range area: ", round((imagenNew$bigRange[[1]] * conversion),2), " - ",
                        round((imagenNew$bigRange[[2]] * conversion),2 )),sep ="<br/>")
  HTML(texto)
})

}


# Run the application 
shinyApp(ui = ui, server = server)
