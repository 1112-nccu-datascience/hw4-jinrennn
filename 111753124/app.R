library(shiny)
library(ggbiplot)

library(DT)
library(FactoMineR)
library(corrplot)
library(factoextra)

# Define UI for application that draws a histogram
ui <- fluidPage(
  #tags$style(HTML('body {font-family:"Lucida Sans Unicode", sans-serif;color:#F8F8F8; font-weight:bold;  background-color:#8E9BAE}')),
  tags$head(
    tags$style(HTML('
      body {
        font-family: "Lucida Sans Unicode", sans-serif;
        font-weight: bold;
        background-color: #8E9BAE;
      }

  
      #data {
        color: #F8F8F8;
      }
  
      #PCA_result, #Summary {
        color: #F8F8F8;
      }
 
      
    '))
  ),
  
  
  # Application title
  
  titlePanel("NCCU_DS2023_111753124"),
  navbarPage('',
             tabPanel('PCA',        
                      tabsetPanel(
                        tabPanel('PCA',
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderInput('rangePCA', "Choose how many input data to do PCA: ", min=1, max=150, value=c(1,150)),
                                     selectInput('pca_x', 'x axis',choices = c('PC1'=1, 'PC2'=2, 'PC3'=3, 'PC4'=4), selected = 1),
                                     selectInput('pca_y', 'y axis',choices = c('PC1'=1, 'PC2'=2, 'PC3'=3, 'PC4'=4), selected = 2) 
                                     ),
                                    
                                   mainPanel(plotOutput("pcaPlot"))
                                 )
                        ),
                        tabPanel('PCA_Result', verbatimTextOutput('pca_result') ),
                        tabPanel('Summary', verbatimTextOutput('pca_summary'))
                        
                      )),
             tabPanel('CA',  
                      tabsetPanel(
                        tabPanel('CA',
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderInput('rangeCA', "Choose how many input data to do CA: ", min=1, max=150, value=c(1,150) ) ),
                                   mainPanel(plotOutput("ca_biplot"))
                                 )),
                        tabPanel('CA_Result', verbatimTextOutput('ca_result') )
                      )),
             tabPanel('Dataset', DT::dataTableOutput('data') ))
  
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  data(iris)
  
  
  log.ir <- reactive(log(iris[input$rangePCA[1]:input$rangePCA[2], 1:4]))
  ir.species <- reactive(iris[input$rangePCA[1]:input$rangePCA[2], 5])
  ir.pca <- reactive(prcomp(log.ir(),center = TRUE, scale. = TRUE))
  ir.ca  <- reactive(CA(iris[input$rangeCA[1]:input$rangeCA[2], 1:4], graph = FALSE))
  
  observe({
    pca_y_choices <- setdiff(1:4, input$pca_x)
    updateSelectInput(session, "pca_y", "y axis", choices = setNames(pca_y_choices, paste0("PC", pca_y_choices)))
  })
  
  
  output$pcaPlot <- renderPlot({
    g <- ggbiplot(ir.pca(), choices = c(as.numeric(input$pca_x), as.numeric(input$pca_y)),obs.scale = 1, var.scale = 1, groups = ir.species(), ellipse = TRUE, circle = TRUE) 
    g <- g + scale_color_discrete(name = '') 
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  
  output$pca_result <- renderPrint(ir.pca())
  output$pca_summary <- renderPrint(summary(ir.pca()))
  output$data <- DT::renderDataTable(DT::datatable(iris, options = list(pageLength = 25) ) )
  
  output$ca_biplot <- renderPlot(fviz_ca_biplot(ir.ca(), title=""))
  output$ca_result <- renderPrint(summary(ir.ca()))
  
}





# Run the application 
shinyApp(ui = ui, server = server)
