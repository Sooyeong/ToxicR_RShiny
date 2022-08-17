library(shiny)
library(DT)
library(tidyverse)



mData <- matrix(c(0, 2,50,
                  1, 2,50,
                  3, 10, 50,
                  16, 18,50,
                  32, 18,50,
                  33, 17,50),nrow=6,ncol=3,byrow=TRUE)
D <- mData[,1]
Y <- mData[,2]
N <- mData[,3]


mData_df<-data.frame(mData)

colnames(mData_df)<-c("D","Y","N")

ui <- fluidPage(
  
  # Application title
  titlePanel("Editable Dataframe and Plot"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      DTOutput("my_datatable"),
      
      # Add a event listner
      actionButton("add",label = "Add Row"),
      actionButton("delete",label = "Delete Row"),
      actionButton("example",label = "Load Example Dataset"),
      actionButton("go",label = "Plot Data")
    ),
    
    # Show plot
    mainPanel(
      plotOutput("my_plot")
    )
  )
)

server <- function(input, output) {
  
  #initialize a blank dataframe
  v <- reactiveValues(data = { 
    # Can I add an action button add row or something? 
    default<-data.frame(D = numeric(0),Y = numeric(0), N=numeric(0)) %>% 
      add_row(D = rep(0,10),Y = rep(0,10), N=rep(0,10))
  })
  
  
  # Add event listner
  observeEvent(input$add, {
    v$data<-v$data %>% add_row(D=0,Y=0,N=0)
  })
  
  
  # Add event listner
  observeEvent(input$delete, {
    v$data<-v$data[-nrow(v$data),]
  })
  
  # Load example dataset
  observeEvent(input$example, {
    v$data<-data.frame(mData_df)
    
  })
  
  
  #output the datatable based on the dataframe (and make it editable)
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE, options=list(pageLength=50, searching=FALSE))
  })
  
  #when there is any edit to a cell, write that edit to the initial dataframe
  #check to make sure it's positive, if not convert
  observeEvent(input$my_datatable_cell_edit, {
    #get values
    info = input$my_datatable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    v$data[i,j] <- k
  })
  
  #render plot
  output$my_plot <- renderPlot({
    req(input$go) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
    isolate(v$data) %>%  #don't react to any changes in the data
      ggplot(aes(D,Y)) +
      geom_point() +
      geom_smooth(method = "lm")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)