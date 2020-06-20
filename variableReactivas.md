## Variables input

**input$imagenFile**: controla el uploader de la imagen
**input$option**: control manual train or from model (opt1/op2) 
**input$filename**: control para asignar nombre del fichero para salvar el modelo
**input$saveModel**: Botón para selecionar folder
**input$filemodel**: Control del upload de modelo
**input$plot_click**: control para clickar los puntos en el manual train
**input$algoritmo**: Control del selector de algoritmo
**input$buttonClass**: botón clasificar
**input$buttonResults**: botón obtener resultados (estadísticas)
**input$pixelsize**: diálogo para asignar el tamaño del pixel
**input$thres**: control del deslizador







## Variables reactivas

  **uploadImage**: ok cuando se sube la imagen  
  **shinyImageFile** : shiny_img_origin
  **va** : x, y coordenadas de los pixel vacuola
  **nova** : x,y coordenadas de los pixel no vacuolas
  **train** : ok cuando el modelo se ha entrenado
  **fv** : ok cuando se ha generado feature vector
  **loadimage** : ok cuando se renderiza la imagen
  **manualTrain** : ok cuando se selecciona Manual Train
  **selectAlgorithm** :ok cuando se ha seleccionado un algoritmo
  **stat** : ok al pulsar el botón result
  **tr** ok al seleccionar algoritmo
  **clf** : ok al pulsar el botón Clasificar
  **cargarmodelo** <- reactiveValues(ok=FALSE)

