library(DiagrammeR)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
header <- dashboardHeader(title = "RNAseq viewer and report App", 
                          titleWidth = 300, 
                          dropdownMenuOutput("messageMenu"),
                          tags$li(class = "dropdown", actionButton("aboutButton", "About"),
                                  style="margin-top:8px; margin-right: 5px")
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            actionButton("button1", "b1"))),
    sidebarMenu(
        menuItem(
            awesomeRadio(
                inputId = "train",
                selected = 'none',
                label = "Select option",
                #individual=TRUE,
                choices = c("Manual train", "From model"),
                status = "primary"
            )
            
            
        )
    )
    )

body <- dashboardBody(fluidPage(fluidRow(column(width = 3,
    grVizOutput("dg", height = "600px")
))))

ui <- dashboardPage(title = "Rython", header, sidebar, body)


server <- function(input, output, session) {
    
    s1 <- reactiveValues(a=NULL) 
    b1 <- reactiveValues(a=NULL)
    
    observeEvent( input$train, {
        s1$a <- input$train
        print(s1$a)
    })
    observeEvent(input$button1,{
        b1$a <- input$button1
        print(b1$a)
    })
    
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
node[fillcolor='@@4', fontcolor='@@17', style=filled, label='Load feature vector']
d
node[fillcolor='@@5', fontcolor='@@18', style=filled, label='Load fitted model']
e
node[fillcolor='@@6', fontcolor='@@19', style=filled, label='Mark points']
f
node[fillcolor='@@7', fontcolor='@@20', style=filled, label='Generate feature vector']
g
node[fillcolor='@@8', fontcolor='@@21', style=filled, label='Select algorithm']
h
node[fillcolor='@@9', fontcolor='@@22', style=filled, label='Train model']
i
node[fillcolor='@@10', fontcolor='@@23', style=filled, label='Classify image']
j
node[fillcolor='@@11', fontcolor='@@24', style=filled, label='View results']
k
node[fillcolor='@@12', fontcolor='@@25', style=filled, label='Save feature vector', shape=record]
l
node[fillcolor='@@13', fontcolor='@@26', style=filled, label='Save model', shape=record]
m

# edge definitions with the node IDs
a -> {b,c}
c -> {d,e}
b -> f
f -> g
g -> h
h -> i
i -> j
j -> k
g -> l [style=dashed, color=grey]
i -> m [style=dashed, color=grey]
d -> h
e -> j
{rank=same; b;c}
{rank=same; d;e}
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


    output$dg <- renderGrViz({
        if(!is.null(s1$a)){ 
            if(s1$a == "Manual train"){col2="'#3c8dbc'"; fcol2="'#ffffff'"}
            if(s1$a == "From model"){col3="'#3c8dbc'"; fcol3="'#ffffff'"}
            }
        if( !is.null( b1$a ) ){ if( b1$a[1] >= 1 ){ col1 = "'#3c8dbc'"; fcol1="'#ffffff'" } }
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
    
}

shinyApp(ui, server)
