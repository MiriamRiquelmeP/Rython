
library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)
library(shinydashboardPlus)
library(shinyjs)
library(shinythemes)

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
                                  inputId = "algorithm",
                                  label = "Choose the method to train",
                                  choices = list( "Kruskall-Wallice" = "opt1", "Mi tia la calva" = "opt2", "Knn" = "opt3"),
                                  options = list(title = "Design"),
                                  selected = NULL ) )
                            ),
                            sidebarMenu( 
                              menuItem(
                                fluidRow(column(12, align = "center", offset=0, uiOutput("TrainClassifier"))))),
                            sidebarMenu(
                              menuItem(
                                fluidRow(column(12, align = "center", offset=0, uiOutput("ClassifyImage"))))),
                            box(
                            
                              menuItem(
                                fluidRow(column(12, offset=0, uiOutput("save")))),
                            
                              menuItem(
                                fluidRow(column(12, offset=0, uiOutput("load"))))
                            )
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
      bsAlert("alert")
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
            Fernando Pérez Sanz",
               imageUrl = "dna-svg-small-13.gif", 
               imageWidth = 200, imageHeight = 100)})
# InputFile #################
output$Image <- renderUI({
  fileInput("Image",
            "Enter your image",
            placeholder = "image.png",
            accept = ".png")
})

observeEvent(input$Image, {
  imagen <- readPNG(input$Image$datapath)
})

# Train #############################
output$TrainClassifier <- renderUI({
  actionButton("train", "Train Classifier")
})


output$ClassifyImage <- renderUI({
  actionButton("class", "Classify the image")
})


output$save <- renderUI({
  actionButton("save", "Save your trainer")
})


output$load <- renderUI({
  actionButton("load", "Load your trainer")
})


# generate reactive variable ###################



}


















# Run the application 
shinyApp(ui = ui, server = server)
