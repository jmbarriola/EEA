#######                                                #######
####### Tomado de https://github.com/apapiu/Shiny-Apps #######
#######                                                #######

library(polynom)
library(ggplot2)
library(shiny)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("flatly"),
                
                titlePanel("Overfitting"),
                
                sidebarLayout(
                  
                  sidebarPanel(checkboxInput(inputId = "seed", label = "Modificar datos",value = TRUE),
                               sliderInput(inputId = "deg1", label = "Grado del modelo", value = 2,
                                           min = 1, max = 20,step = 1),
                               "Datos reales",
                               sliderInput(inputId = "n", label = "Cantidad de ejemplos", value = 50,
                                           min = 10, max = 200,step = 1),
                               sliderInput(inputId = "q", label = "Grado del target", value = 1,
                                           min = 1, max = 5,step = 1),
                               sliderInput(inputId = "s", label = "Cantidad de ruido", value = .5,
                                           min = 0, max = 3,step = .1), width = 3),
                  
                  mainPanel(plotOutput("plot", width = "800px", height = "600px"))
                )
)

server <- function (input, output) {
  
  data = reactive({
    
    n = input$n
    q = input$q
    s = input$s
    
    #set.seed(233)
    observeEvent(input$seed,{set.seed(sample(1:1000, 1))})
    x <- runif(n, min = -1, max = 1)
    epsi <- rnorm(n) #noise
    poly <- polynom::polynomial(rnorm(n = q+1)) #poly
    y <- predict(poly, x) + sqrt(s)*epsi #values of poly +noise
    
    return(list(data.frame(x, y), poly))
  })
  
  
  output$plot =  renderPlot({
    
    polynomial <- as.function(data()[[2]])
    
    ggplot(data()[[1]]) +
      aes(x, y) +
      geom_point(alpha = 0.7, size = 3, shape = 1) +
      theme_classic() +
      stat_function(fun = polynomial, size = 1.25, alpha = 0.9) +
      geom_smooth(method = "lm", formula = y ~ poly(x,input$deg1), 
                  color = "red", se = FALSE, size = 1.25, alpha = 0.9) +
      theme(panel.background = element_rect(color = "black"))
  })
  
  
  
}

shinyApp(ui, server)
