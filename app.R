
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
                                uiOutput("Image")
                              )),
                            sidebarMenu(
                              menuItem( 
                                pickerInput(
                                  inputId = "option",
                                  label = "Choose the option",
                                  choices = list( "Manual train" = "opt1",
                                                  "Train from model" = "opt2"),
                                  options = list(title = "Option ..."),
                                  selected = NULL ) )
                            ),
                            sidebarMenu( 
                              menuItem(
                                fluidRow( column(12, align = "center", offset=0,
                                                 uiOutput("loadftv"),
                                                 uiOutput("loadfitmodel") ) ) ) ),
                            sidebarMenu(
                              menuItem(
                                fluidRow(column(12, align = "center", offset=0,
                                                uiOutput("algorithm") ) ) ) ),
                            sidebarMenu(
                              menuItem(
                                fluidRow(
                                  column(12, align="center", offset=0,
                                         uiOutput("botonClass") )
                                )
                              )
                            ),
                            sidebarMenu(
                              menuItem(
                                fluidRow(
                                  column(12, align="center", offset=0,
                                         uiOutput("botonResults") )
                                )
                              )
                            ),
                            sidebarMenu(
                              menuItem(
                                fluidRow(column(12, align = "center", offset = 0,
                                                uiOutput("savefvt"),
                                                uiOutput("savefitmodel") ) ) ) )
                            )

### BODY ###############
body <- dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "customDark.css")
      ),
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
               grVizOutput("dg", height = "600px"),
               sliderInput("thres", "Select threshold", min = 0, max = 5000, value=2000, step=10) ),
      column(width=9, 
             #plotOutput("imagen", click = "plot_click"),
             uiOutput("image"),
             textOutput("texto")
      )),
      fluidRow(column(width = 9, offset = 3,
                      textOutput("resultxt")))
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
  
  # mostrar flujograma ##
  # output$dg <- renderGrViz({
  #   # TODO meter todo lo que falta aqui
  # })
  # 
  # 
# InputFile #################
output$Image <- renderUI({
  fileInput("imagenFile",
            "Enter your image",
            placeholder = "image.png",
            accept = c(".png",".jpg") )
})

output$image <- renderUI({
  if(input$option == "opt1"){
    plotOutput("imagen", click = "plot_click")
  } else{
    plotOutput("imagen")
  }
    
})

shinyImageFile <- reactiveValues(shiny_img_origin = NULL)
va <- reactiveValues(x=NULL, y=NULL)
nova <- reactiveValues(x=NULL, y=NULL)
counter <- 1
train <- reactiveValues(ok=FALSE)
fv <- reactiveValues(ok = FALSE)

renameUpload <- function(inFile) {
    if(is.null(inFile))
      return(NULL)
    oldNames = inFile$datapath
    newNames = file.path(dirname(inFile$datapath), inFile$name)
    file.rename(from = oldNames, to = newNames)
    inFile$datapath <- newNames
    return(inFile$datapath)
  }
    
observeEvent(input$option, {
    validate(need(!is.null(input$imagenFile),""))
    #validate(need(input$option,""))
    imagenNew$image <- renameUpload(input$imagenFile)
    imagenNew$set_NUM(10)
    cargarImagen(imagenNew, imagenNew$image)
    shinyImageFile$shiny_img_origin <- shinyimg$new(renameUpload(input$imagenFile))
    output$imagen <- renderPlot({shinyImageFile$shiny_img_origin$render()})
    shinyalert("Mark points into vacuole object")
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
# 
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

tr <- reactiveValues(ok=FALSE)

observeEvent(input$algoritmo,{
  validate(need(input$algoritmo,""))
  if( input$algoritmo == "knn"){
    train = trainKnn(imagenNew)
    tr$ok <- TRUE
  }
})

output$botonClass <- renderUI({
  validate(need(isTRUE(tr$ok),""))
  actionButton(inputId = "buttonClass", label="Classify")
})

clf <- reactiveValues(ok=FALSE)

observeEvent(input$buttonClass, {
  if(isTRUE(tr$ok)){
    B = classImage(imagenNew)
    imagenNew$B = B
    clf$ok <- TRUE
    BR <- py_2_R_imageBW(B)
    output$imagen <- renderPlot({display(BR, method = "raster")})
  }
})

output$botonResults <- renderUI({
  validate(need(isTRUE(clf$ok),""))
  actionButton(inputId = "buttonResults", label="View Results")
})

stat <- reactiveValues(ok = FALSE)

observeEvent(input$buttonResults,{
  statistics(imagenNew)
  stat$ok <- TRUE
})

observeEvent(input$thres,{
  validate(need(input$thres,""))
  validate(need(isTRUE(stat$ok),"" ) )
  thr = input$thres # se pasará por según valor deslizador
  threshold(imagenNew, thr)
  imagenNew$ratio
  maskImage(imagenNew)
  maskR <- py_2_R_imageColor(imagenNew$masked)
  output$imagen <- renderPlot({display(maskR, method = "raster")}) 
})

}


















# Run the application 
shinyApp(ui = ui, server = server)
