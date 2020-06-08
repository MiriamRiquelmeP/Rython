library(shiny)
library(imager)
library(ShinyImage)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Vision GUI beta"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("imagenFile", "Choose imagen file", multiple = FALSE, accept = c(".jpg", ".png") )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("imagen", click = "plot_click"),
            textOutput("texto")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
    observeEvent(input$imagenFile,{
        
    })    
  observeEvent(input$imagenFile, {
    validate(need(!is.null(input$imagenFile),""))
    shinyImageFile$shiny_img_origin <- shinyimg$new(renameUpload(input$imagenFile))
    output$imagen <- renderPlot({shinyImageFile$shiny_img_origin$render()})
  })        
    output$texto <- renderText({
        paste0(round(input$plot_click$x), " : ", round(input$plot_click$y))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
