#######                                                #######
####### basado en https://github.com/apapiu/Shiny-Apps #######
#######                                                #######

library(polynom)
library(ggplot2)
library(shiny)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("paper"),
                
                titlePanel("Overfitting"),
                
                sidebarLayout(
                  sidebarPanel("Diego Kozlowski, Juan Manuel Barriola",
                               checkboxInput(inputId = "seed", label = "Mantener datos",value = TRUE),
                               sliderInput(inputId = "deg1", label = "Grado del modelo", value = 2,
                                           min = 1, max = 20,step = 1),
                               "Datos reales",
                               sliderInput(inputId = "n", label = "Cantidad de ejemplos", value = 50,
                                           min = 10, max = 200,step = 1),
                               sliderInput(inputId = "q", label = "Grado del target", value = 1,
                                           min = 1, max = 5,step = 1),
                               sliderInput(inputId = "s", label = "Cantidad de ruido", value = .5,
                                           min = 0, max = 3,step = .1), width = 3),
                  
                  mainPanel(
                    h2(textOutput("text1", container = span)),
                    h4(textOutput("text2", container = span)),
                    withMathJax(),
                    uiOutput('eq0'),
                    uiOutput('eq1'),
                    plotOutput("plot", width = "800px", height = "600px"))
                  
                  
                  
                  
                )
)

server <- function (input, output) {
  
  data = reactive({
    
    n = input$n
    q = input$q
    s = input$s
    
    observeEvent(input$seed,{set.seed(1234)}) #sample(1:1000, 1)
    x <- runif(n, min = -1, max = 1)
    epsi <- rnorm(n, mean = 0,sd = s) #ruido
    poly <- polynom::polynomial(rnorm(n = q+1)) #polinomio
    y <- predict(poly, x) + epsi #valores con ruido 
    
    return(list(data.frame(x, y), poly))
  })
  
  output$text1 <- renderText({
    paste0("El concepto de overfitting en el modelo lineal")})
  
  output$text2 <- renderText({
    paste0("El modelo SIEMPRE es lineal en los coeficientes y sus efectos SIEMPRE son aditivos")})
  
  output$eq0 <- renderUI({
    eq <- paste0("y  = \\beta_0")
    
    for (i in c(1:input$q)) {
      eq <- paste0(eq, '+ \\beta_{',i,'}','x^{',i,'}')
      
    }
    withMathJax(
      h5(paste0('Modelo Real  $$',eq,'+ \\epsilon$$',
                      'con $$\\epsilon\\sim\\mathcal{N}(0,\\,',input$s,')$$')))
  })
  
  
  
  output$eq1 <- renderUI({
    eq <- paste0("\\hat{y}  = \\beta_0")
    
    for (i in c(1:input$deg1)) {
      eq <- paste0(eq, '+ \\beta_{',i,'}','x^{',i,'}')
      
    }
    withMathJax(
      h5(paste0('Modelo Propuesto  $$',eq,'$$')))
  })
  
  output$plot =  renderPlot({
    
    polynomial <- as.function(data()[[2]])
    
    ggplot(data()[[1]]) +
      aes(x, y) +
      geom_point(alpha = 0.7, size = 3, color='forestgreen') +
      theme_classic() +
      stat_function(fun = polynomial, size = 1.25, alpha = 0.9) +
      geom_smooth(method = "lm", formula = y ~ poly(x,input$deg1), 
                  color = "red", se = FALSE, size = 1.25, alpha = 0.9) +
      theme(panel.background = element_rect(color = "black"))
  })
}
shinyApp(ui, server)
