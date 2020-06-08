
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
library(EBImage)
library(DiagrammeR)
library(png)
source("utils.R")

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
                                                 uiOutput("ftv"),
                                                 uiOutput("fitmodel") ) ) ) ),
                            sidebarMenu(
                              menuItem(
                                fluidRow(column(12, align = "center", offset=0,
                                                uiOutput("algorithm") ) ) ) ),
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
              textOutput("results")),
      column(width=9, 
             #plotOutput("imagen", click = "plot_click"),
             uiOutput("image"),
             textOutput("texto")
      )))
)
     
  



########################################## UI #################################################

ui <- dashboardPage(title="Image classifier",
                    header,
                    sidebar,
                    body
) # fin del UI


########################################## SERVER #################################################

server <- function(input, output, session) {
  
  observeEvent(input$aboutButton, {
    shinyalert("Rython app 2020", "Authors:
               User interface design: Miriam Riquelme Pérez
            Algorithm development: Fernando Pérez Sanz",
               imageUrl = "dna-svg-small-13.gif", 
               imageWidth = 200, imageHeight = 100)})
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
    shinyImageFile$shiny_img_origin <- shinyimg$new(renameUpload(input$imagenFile))
    output$imagen <- renderPlot({shinyImageFile$shiny_img_origin$render()})
  })        
  
output$texto <- renderText({
    validate(need(input$plot_click, ""))
        paste0(round(input$plot_click$x), " : ", round(input$plot_click$y))
        
    })


}


















# Run the application 
shinyApp(ui = ui, server = server)
