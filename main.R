if(!require(shiny)){
  install.packages("shiny")
}
if(!require(shinyWidgets)){
  install.packages("shinyWidgets")
}

library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("Sondaż wyborczy partii X"),
  
  fluidRow(
    column(12,
      h3("Populacja"),
      p("Wygeneruj populację z zadaną wielkością i szansą na bycie zwolennikiem partii."),
      sidebarLayout(
        sidebarPanel(
          numericInput(inputId = "populationSize",
                      label = "Wielkość populacji:",
                      min = 1,
                      value = 10000000),
          formatNumericInput(inputId = "populationChance", 
                             label = "Szansa na bycie zwolennikiem:", 
                             value = 0.33, 
                             format = "percentageEU2dec"),
          actionButton("populationButton", label="Wylosuj populację")
        ),
        mainPanel(
          plotOutput(outputId = "populationPlot"),
          tableOutput(outputId = "populationInfo")
        )
      )
    )
  ),
  fluidRow(
    column(12,
      h3("Sondaż"),
      p("Przeprowadż sondaż wyborczy. Sondaż polega na wylosowaniu z populacji 
        pewnej liczby osób, a następnie sprawdzeniu jaka część z nich zagłosowałaby
        na partię X. Powtarzane jest to wiele razy dzięki czemu otrzymujemy
        rozkład wyników. Przedstawiony jest on na histogramie."),
      sidebarLayout(
        sidebarPanel(
          numericInput(inputId = "sampleSize",
                      label = "Wielkość próbki:",
                      min = 1,
                      value = 100),
          numericInput(inputId = "sampleNumber",
                      label = "Liczba próbek:",
                      min = 1,
                      value = 10000),
          actionButton("sampleButton", label="Znajdź rozkład"),
        ),
        mainPanel(
          plotOutput(outputId = "resultsPlot"),
          tableOutput(outputId = "resultsInfo")
        )
      )
    )
  ),
  fluidRow(
    column(12,
      h3("Prawdopodobieństwo"),
      sidebarLayout(
        sidebarPanel(
          formatNumericInput(inputId = "deviation", 
                             label = "Ile punktów procentowych?", 
                             value = 0.03, 
                             format = "percentageEU2dec")
        ),
        mainPanel(
          textOutput(outputId = "question"),
          textOutput(outputId = "probabilityResult",
                     container = tags$h2)
        )
      )
    )
  )
)

server <- function(input, output) {
  populationSize <- eventReactive( input$populationButton, {
    input$populationSize
  })
  
  populationChance <- eventReactive( input$populationButton, {
    input$populationChance
  })

  sampleSize <- eventReactive( input$sampleButton, {
    input$sampleSize
  })
  
  sampleNumber <- eventReactive( input$sampleButton, {
    input$sampleNumber
  })

  people <- reactive({
    rbinom(populationSize(), 1, populationChance())
  })
  
  output$populationPlot <- renderPlot({
    peopleCount <- as.data.frame(table(people()))
    barplot(height=peopleCount$Freq,
            names=c("Przeciw", "Za"), 
            main="Poparcie dla partii X",
            col=c("red", "green"),
            ylab="Liczba osób")
  })
  
  populationPercent = reactive(sum(people())/length(people()))
  output$populationInfo <- renderTable(
    data.frame(
      Przeciw = length(people())-sum(people()), 
      Za = sum(people()), 
      Procent = sprintf("%.2f%%", populationPercent()*100)
    )
  )
  
  result <- reactive({
    result = vector("double", sampleNumber())
    for(i in 1:sampleNumber()) {
      peopleSample <- sample(people(), sampleSize(), replace = TRUE)
      result[i] = sum(peopleSample)/sampleSize()
    }
    result
  })
  
  output$resultsPlot <- renderPlot(
    hist(result(),
         main="Rozkład zmierzonego poparcia",
         xlab="Procentowe poparcie",
         ylab="Ilość przypadków")
  )
  
  output$resultsInfo <- renderTable({
    fx <- fivenum(result())
    names(fx) <- c("Minimum", "Q1", "Mediana", "Q3", "Maksimum")
    as.list(fx)
  })
  
  output$question <- renderText(
    sprintf("Jak często wynik sondażu odbiega o co najwyżej %.0f punktów 
            procentowych od stanu faktycznego?", input$deviation*100)
  )
  
  output$probabilityResult <- renderText({
    txt <- sum(abs(result()-populationPercent()) < input$deviation)/length(result())
    sprintf("%.2f%%", txt*100)
  })
}

shinyApp(ui = ui, server = server)