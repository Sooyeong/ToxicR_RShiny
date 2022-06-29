
library(shiny)
library(ggplot2)
library(plotly)
library(scales)
library(ToxicR)

## Only run examples in interactive R sessions

# Model list - Dichotomous case
model_list<-list("hill","gamma","logistic","log-probit","weibull",
                 "log-logistic","probit","multistage")

model_list2<-list("FUNL","hill","exp-3","exp-5","power")



fit_type<-list("mcmc", "mle", "laplace")
ui<-navbarPage(title = "Toxic R", selected="Dichotomous Fitting",
               # Option 1. Select box option
               
               tabPanel("Dichotomous Fitting",  fluidPage(
                 sidebarLayout(
                   sidebarPanel(
                     
                     
                     conditionalPanel(condition="input.tabs =='Single Model'",
                                      helpText("Dichotomous Single Model"),
                                      
                                      selectInput(inputId="model", 
                                                  label= "Choose a model type",
                                                  choices=model_list, 
                                                  selected = "gamma"),
                                      
                                      selectInput(inputId="fit_type", 
                                                  label= "Choose a fit type",
                                                  choices=fit_type, 
                                                  selected = "mcmc"),
                                      
                                      sliderInput(inputId="bmr_slide",
                                                  label="Choose a BMR level",
                                                  min=0,max=1,value=0.1)
                     ),
                     
                     conditionalPanel(condition="input.tabs =='Model Average'",
                                      helpText("Dichotomous Model Average"),
                                      
                                      # selectInput(inputId="model", 
                                      #             label= "Choose a model type",
                                      #             choices=model_list, 
                                      #             selected = "gamma"),
                                      # 
                                      selectInput(inputId="fit_type2",
                                                  label= "Choose a fit type",
                                                  choices=fit_type,
                                                  selected = "mcmc"),
                                      
                                      sliderInput(inputId="bmr_slide2",
                                                  label="Choose a BMR level",
                                                  min=0,max=1,value=0.1)
                     )
                     
                     
                   ),
                   mainPanel(
                     
                     tabsetPanel(id="tabs",
                                 tabPanel("Single Model",plotlyOutput(outputId = "dic_sing_plot")),
                                 tabPanel("Model Average",plotlyOutput(outputId = "dic_ma_plot"))
                                 
                                 
                     )
                     
                   )
                 )
                 
               )
               
               ),
               
               tabPanel("Continous Fitting")
)



server<- function (input,output){
  
  
  output$dic_sing_plot<-renderPlotly({
    temp_fit = single_dichotomous_fit(mData[,1],mData[,2],mData[,3],model_type = input$model,fit_type = input$fit_type, BMR = input$bmr_slide)
    
    #For MCMC  
    if (input$fit_type=="mcmc"){
      .plot.BMDdich_fit_MCMC(fit=temp_fit,fit_type=input$fit_type)
    }
    
    #For MCMC  
    else if (input$fit_type!="mcmc"){
      .plot.BMDdich_fit_maximized(fit=temp_fit,fit_type=input$fit_type)
    }
    
    
  })
  
  output$dic_ma_plot<-renderPlotly({
    temp_fit = ma_dichotomous_fit(mData[,1],mData[,2],mData[,3],fit_type = input$fit_type2, BMR = input$bmr_slide2)
    .plot.BMDdichotomous_MA(A=temp_fit)
  })
  
}

shinyApp(ui=ui, server=server)
