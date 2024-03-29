## Author: Sooyoeng Lim
## Purpose: Implement ToxicR's base functions in GUI interface by using R-Shiny



# Load required libraries 
library(shiny)
library(ggplot2)
library(plotly)
library(scales)
library(ToxicR)

## Developer's Note- -
## 1. Only run examples in interactive R sessions 
## 2. Need to change outputs to reactive() object 
## 3. Action button should be added to run program - 07/24/22
## 4. For model average part- implement checkboxGroupInput()  -- Updated 07/24/22
## 5. Modify impelmented code for the most up to date Toxic R package
## 6. Provide example fitting case
## 7. Upload the discussion note with John

## 8. Create sample dichotomous dataset and test



mData <- matrix(c(0, 0, 59,
                  6, 54,60,
                  20, 59, 60,
                  60, 59,60),nrow=4,ncol=3,byrow=TRUE)

D<-mData[,1]
Y<-mData[,2]
N<-mData[,3]


res<-single_dichotomous_fit(mData[,1],mData[,2],mData[,3],model_type ="hill",fit_type = "laplace", BMR = 0.1)
# sumamry is only avaialbe for the fit=laplace case not for the mcmc
summary(res)

res2<-ma_dichotomous_fit(mData[,1],mData[,2],mData[,3],fit_type = "laplace", BMR = 0.1)

summary(res)

#BMD-BMDS, BMDL should be provided
summary(res2)


# Model list - Dichotomous case
ls_dich_models<-ToxicR:::.dichotomous_models
ls_cont_models<-ToxicR:::.continuous_models



# selectInput("dataset", label = "Dataset", choices = ls("package:datasets"))
# choices should be list format



# MLE will be removed from the APP
fit_type<-list("mcmc", "laplace")

ui<-navbarPage(title = "Toxic R", selected="Dichotomous Fitting",
               # Option 1. Select box option
               tabPanel("Dichotomous Fitting", 
               fluidPage(
                 sidebarLayout(
                   
                   sidebarPanel(
                     
                     fileInput("Upload your dataset",NULL),  
                     conditionalPanel(condition="input.tabs =='Single Model'",
                                      helpText("Dichotomous Single Model"),
                                      
                                      selectInput(inputId="model", 
                                                  label= "Choose a model type",
                                                  choices=ls_dich_models, 
                                                  selected = "gamma"),
                                      
                                      selectInput(inputId="fit_type", 
                                                  label= "Choose a fit type",
                                                  choices=fit_type, 
                                                  selected = "mcmc"),
                                      
                                      sliderInput(inputId="bmr_slide",
                                                  label="Choose a BMR level",
                                                  min=0,max=1,value=0.1),
                                      
                                      actionButton("run_dich_single","Run" ,class="btn-lg btn-success")
                                      
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
                                      checkboxGroupInput(inputId="dich_MA_input",
                                                         label="Models for fitting dichoutomous model average",
                                                         choices=ls_dich_models,
                                                         selected=ls_dich_models),
                                      sliderInput(inputId="bmr_slide2",
                                                  label="Choose a BMR level",
                                                  min=0,max=1,value=0.1),
                                      actionButton("run_dich_MA","Run",class="btn-lg btn-success")
                     )
                     
                     
                   ),
                   mainPanel(
                     
                     tabsetPanel(id="tabs",
                                 tabPanel("Single Model", plotlyOutput(outputId = "dic_sing_plot"),
                                          verbatimTextOutput("sum_dich_single"),
                                          verbatimTextOutput("bmd_dich_single"),
                                          downloadButton("download1", "Download")
                                          
                                          
                                          ),
                                 tabPanel("Model Average", plotlyOutput(outputId = "dic_ma_plot"), 
                                          verbatimTextOutput("sum_dich_ma"),
                                          verbatimTextOutput("bmd_dich_ma"),
                                          downloadButton("download2", "Download")
                                          
                                          ),
                    
                                 
                                                       
                     
                                 
                     
                                 
                     )
                     
                   )
                 )
                 
               )
               
               ),
               
               tabPanel("Continous Fitting")
)



server<- function (input,output){

  
  # Is there any part I can change them as reactive expression? 
  # 1. Input treatment -> 2. Output rendering
    
  
  
  # Handling inputs
  
  # Change the output style as reactive format 
  
  
  temp_fit<-reactive({
    single_dichotomous_fit(mData[,1],mData[,2],mData[,3],model_type = input$model,fit_type = input$fit_type, BMR = input$bmr_slide)
  })
  
  temp_fit2 <-reactive({
    ma_dichotomous_fit(mData[,1],mData[,2],mData[,3],fit_type = input$fit_type2, BMR = input$bmr_slide2)
  }) 
  
  # Output for the dichotomous single plot
  output$dic_sing_plot<-renderPlotly({
    
    #For MCMC  
    if (input$fit_type=="mcmc"){
      # .plot.BMDdich_fit_MCMC(fit=temp_fit,fit_type=input$fit_type)
      plot(temp_fit())
    }
    
    #For MCMC  
    else if (input$fit_type!="mcmc"){
      # .plot.BMDdich_fit_maximized(fit=temp_fit,fit_type=input$fit_type)
      plot(temp_fit())
    }
    
  })
  
  output$sum_dich_single<-renderPrint({
    summary(temp_fit())
  })
  
  
  
  
  output$bmd_dich_single<-renderPrint({
    temp_fit()$bmd
  })
  
  
  
  
  output$sum_dich_ma<-renderPrint({
    summary(temp_fit2())
  })
  
  
  output$bmd_dich_ma<-renderPrint({
    temp_fit2()$bmd
  })
  
  
  output$dic_ma_plot<-renderPlotly({
    
    plot(temp_fit2())
    
    # .plot.BMDdichotomous_MA(A=temp_fit)
  })
  
  
  

}

shinyApp(ui=ui, server=server)
