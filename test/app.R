library(shiny)
reactiveConsole(TRUE)

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)


ui <- fluidPage(
  selectInput("var", "Sort by", choices = names(mtcars)),
  checkboxInput("desc", "Descending order?"),
  tableOutput("data")
)
server <- function(input, output, session) {
  sorted <- reactive({
    if (input$desc) {
      arrange(mtcars, desc(.data[[input$var]]))
    } else {
      arrange(mtcars, .data[[input$var]])
    }
  })
  output$data <- renderTable(sorted())
}


shinyApp(ui=ui,server=server)

DynamicValue <- R6::R6Class("DynamicValue", list(
  value = NULL,
  on_update = NULL,
  
  get = function() self$value,
  
  set = function(value) {
    self$value <- value
    if (!is.null(self$on_update)) 
      self$on_update(value)
    invisible(self)
  },
  
  onUpdate = function(on_update) {
    self$on_update <- on_update
    invisible(self)
  }
))

temp_c <- DynamicValue$new()
temp_c$onUpdate(function(value) {
  message("Converting") 
  temp_f <<- (value * 9 / 5) + 32
})

temp_c$set(10)
#> Converting
temp_f
#> [1] 50

temp_c$set(-3)
#> Converting
temp_f
#> [1] 26.6


temp_c<-reactiveVal(10)

temp_c(20)

temp_c()

temp_f <- reactive({
  message("Converting") 
  (temp_c() * 9 / 5) + 32
})

temp_c(-3)
temp_f()


#> Converting
#> [1] 68
# 
# diamonds %>% filter(carat>1)


#input$var--> env-varaible 
#carat --> statistical variable

#.data[[]] , .env$ in dplyr