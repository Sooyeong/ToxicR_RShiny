## Author: Sooyoeng Lim
## Purpose: Implement ToxicR's base functions in GUI interface by using R-Shiny

# Load required libraries 
library(shiny)
library(ggplot2)
library(plotly)
library(scales)
library(ToxicR)
library(DT)


## Developer's Note- -
## 1. Only run examples in interactive R sessions 
## 2. Need to change outputs to reactive() object - > Partially updated, but 
## 3. Action button should be added to run program - 07/24/22
## 4. For model average part- implement checkboxGroupInput()  -- Updated 07/24/22
## 6. Provide example fitting case
## 7. Upload the discussion note with John
## 8. Create sample dichotomous dataset and test
## 9. Module part should be updated
## 10. User defined input channel should be updated
## 11. Load it from cvs/xlsl file 

# HTML part is added

## 08/09/22 - Add more information for single dichotmous cases --
## Parameter, GOF, etc information/CDF is added;
## User input/output




# example dataset
mData <- matrix(c(0, 2,50,
                  1, 2,50,
                  3, 10, 50,
                  16, 18,50,
                  32, 18,50,
                  33, 17,50),nrow=6,ncol=3,byrow=TRUE)


D <- mData[,1]
Y <- mData[,2]
N <- mData[,3]

# Change input as data.frame style
mData_df<-data.frame(mData)
colnames(mData_df)<-c("D","Y","N")





####Testing Bed#####

res<-single_dichotomous_fit(mData[,1],mData[,2],mData[,3],model_type ="hill",fit_type = "mcmc", BMR = 0.1)

res2<-ma_dichotomous_fit(mData[,1],mData[,2],mData[,3],fit_type = "mcmc", BMR = 0.1)





# sumamry is only avaialbe for the fit=laplace case not for the mcmc
# 
# res
# summary(res)
# 
# res$parameters
# 
# res$covariance
# 
# res$bmd_dist
# ggplot()+
#   geom_line(aes(x=res$bmd_dist[,1], y=res$bmd_dist[,2]))
# summary(res)
####TEST #####






# Model list - Dichotomous case
ls_dich_models<-ToxicR:::.dichotomous_models
ls_cont_models<-ToxicR:::.continuous_models


# MLE is restored from ToxicR App 
fit_type<-list("mcmc", "laplace","mle")



ui<-navbarPage(title = "Toxic R", selected="Dichotomous Fitting",
               # Option 1. Select box option
               tabPanel("Dichotomous Fitting", 
               fluidPage(
                 sidebarLayout(
                   
                   sidebarPanel(
                     
                     #fileInput("Upload your dataset",NULL),  
                     

                     conditionalPanel(condition="input.tabs =='Single Model'",
                                      helpText("Dichotomous Single Model"),
                                      DTOutput("my_datatable"),
                                      
                                      actionButton("add",label = "Add Row"),
                                      actionButton("delete",label = "Delete Row"),
                                      actionButton("example",label = "Load Example Dataset"),
                                      selectInput(inputId="model", 
                                                  label= "Choose a model type",
                                                  choices=ls_dich_models, 
                                                  selected = "gamma"),
                                      
                                      selectInput(inputId="fit_type", 
                                                  label= "Choose a fit type",
                                                  choices=fit_type, 
                                                  selected = "mcmc"),
                                      
                                      numericInput(inputId="bmr_slide",
                                                  label="Choose a BMR level",
                                                  min=0,max=1,value=0.1,step=0.1),
        
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
                                      numericInput(inputId="bmr_slide2",
                                                  label="Choose a BMR level",
                                                  min=0,max=1,value=0.1),
                                      actionButton("run_dich_MA","Run",class="btn-lg btn-success")
                     )
                     
                     
                   ),
                   mainPanel(
                     
                     tabsetPanel(id="tabs",
                                 tabPanel("Single Model", plotlyOutput(outputId = "dic_sing_plot"),
                                          plotOutput(outputId="dic_sing_plot_cdf"),
                                          verbatimTextOutput("sum_dich_single"),
                                          verbatimTextOutput("bmd_dich_single"),
                                          verbatimTextOutput("dich_single_parameters"),
                                          verbatimTextOutput("dich_single_covariance"),
                                          tableOutput("dich_single_gof"),
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
  # Inputs from downloaded part
  
  #initialize a blank dataframe
  v <- reactiveValues(data = { 
    # Can I add an action button add row or something? 
    data.frame(D = numeric(0),Y = numeric(0), N=numeric(0)) %>% 
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
  
  
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE, options=list(pageLength=50, searching=FALSE))
  })
  
  # Change the output style as reactive format 
  
  
  temp_fit<-reactive({
    req(input$run_dich_single)
    isolate(v$data)
    single_dichotomous_fit(v$data$D,v$data$Y,v$data$N,model_type = input$model,fit_type = input$fit_type, BMR = input$bmr_slide)
  })
  
  temp_fit2 <-reactive({
    ma_dichotomous_fit(mData[,1],mData[,2],mData[,3],fit_type = input$fit_type2, BMR = input$bmr_slide2)
  }) 
  
  # Output for the dichotomous single plot
  output$dic_sing_plot<-renderPlotly({
    req(input$run_dich_single)
    isolate(v$data)
    plot(temp_fit())
    
  })
  
  output$dic_sing_plot_cdf<-renderPlot({
    req(input$run_dich_single)
    isolate(temp_fit())
      ggplot()+
        geom_line(aes(x=temp_fit()$bmd_dist[,1], y=temp_fit()$bmd_dist[,2]))+
        xlab("BMD")+
        ylab("Probability")+
        ggtitle("\nCDF of BMD")+
        theme_classic()
  })
  
  
  output$sum_dich_single<-renderPrint({
    req(input$run_dich_single)
    isolate(temp_fit())
    summary(temp_fit())

  })
  
  output$bmd_dich_single<-renderPrint({
    req(input$run_dich_single)
    isolate(temp_fit())
    temp_fit()$bmd
  })
  
  # 08/09/22
  output$dich_single_parameters<-renderPrint({
    # Should we add models for the parameters?
    temp_fit()$parameters
  })
  
  output$dich_single_covariance<-renderPrint({
    temp_fit()$covariance
  })
  
  output$dich_single_gof<-renderTable({
    data.frame(c(temp_fit()$gof_p_value,temp_fit()$gof_chi_sqr_statistic))
  })
  
  
  # res$parameters
  # 
  # res$covariance
  
  
  
  
  output$sum_dich_ma<-renderPrint({
    summary(temp_fit2())
  })
  
  
  output$bmd_dich_ma<-renderPrint({
    temp_fit2()$bmd
  })
  
  
  output$dic_ma_plot<-renderPlotly({
    plot(temp_fit2())
  })
  
  
  

}

shinyApp(ui=ui, server=server)
