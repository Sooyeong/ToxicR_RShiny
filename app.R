## Author: Sooyoeng Lim
## Purpose: R-Shiny App of ToxicR Package
## Last Updated: 08/18/22

## Developer's Note- -


## 1. Action button should be added to run model fitting - Updated 07/24/22
## 2. For model average part- implement checkboxGroupInput() model selection  - Updated 07/24/22
## 3. Provide example dataset - Updated
## 4. Upload the discussion note with John

## 5. Module part should be updated - Working in Progress


## 6. User defined input layout - Updated 08/16/22
## 7. Load it from cvs/xlsl file - Updated 08/17/22. This needs to be discussed with Matt how to determine the format of input dataset 


## 08/09/22 - Add more information for single dichotmous cases --
## Parameter, GOF, etc information/CDF is added;
## User input/output




# Load required libraries 
library(ToxicR)
library(shiny)
library(ggplot2)
library(plotly)
library(scales)
library(DT)




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

list(ls_dich_models)
# 
# ####Testing Bed#####--> this part will be used for representing output in modeling page
# res<-single_dichotomous_fit(mData[,1],mData[,2],mData[,3],model_type ="hill",fit_type = "mcmc", BMR = 0.1)
# 
# # Multi fitting default model should be represented
# res2<-ma_dichotomous_fit(mData[,1],mData[,2],mData[,3],model_list=priors,fit_type = "mcmc", BMR = 0.1)
# 



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



# Input needs to be multiple vector or loop assignment is required

# Assign prior for the multy models
# 
# priors<-list()
# model_i=rep(0,length(ls_dich_models))
# ls_dich_models
# 

# Modified function for the ma_dich single fitting
.bayesian_prior_dich2<-function(model, degree = 2){
  dmodel = which(model == c("hill", "gamma", "logistic", "log-logistic", 
                            "log-probit", "multistage", "probit", "qlinear", "weibull"))
  if (dmodel == 1) {
    prior <- create_prior_list(normprior(-1, 2, -40, 40), 
                               normprior(0, 3, -40, 40), normprior(-3, 3.3, -40, 
                                                                   40), lnormprior(0.693147, 0.5, 0, 40))
    prior <- create_dichotomous_prior(prior, "hill")
  }
  if (dmodel == 2) {
    prior <- create_prior_list(normprior(0, 2, -18, 18), 
                               lnormprior(0.693147180559945, 0.424264068711929, 
                                          0.2, 20), lnormprior(0, 1, 0, 10000))
    prior <- create_dichotomous_prior(prior, "gamma")
  }
  if (dmodel == 3) {
    prior <- create_prior_list(normprior(0, 1, -20, 20), 
                               lnormprior(0, 2, 0, 40))
    prior <- create_dichotomous_prior(prior, "logistic")
  }
  if (dmodel == 4) {
    prior <- create_prior_list(normprior(0, 2, -20, 20), 
                               normprior(0, 1, -40, 40), lnormprior(0.693147180559945, 
                                                                    0.5, 0, 20))
    prior <- create_dichotomous_prior(prior, "log-logistic")
  }
  if (dmodel == 5) {
    prior <- create_prior_list(normprior(0, 2, -20, 20), 
                               normprior(0, 1, -40, 40), lnormprior(0.693147180559945, 
                                                                    0.5, 0, 20))
    prior <- create_dichotomous_prior(prior, "log-probit")
    
  }
  if (dmodel == 6) {
    startP <- create_prior_list(normprior(0, 2, -20, 20), 
                                lnormprior(0, 0.5, 0, 100))
    degree = floor(degree)
    if (degree >= 2) {
      for (ii in (2:degree)) {
        startP <- ToxicR:::.combine_prior_lists(startP, lnormprior(0, 
                                                          1, 0, 1e+06))
      }
    }
    prior <- startP
    prior <- create_dichotomous_prior(prior, "multistage")
  }
  if (dmodel == 7) {
    prior <- create_prior_list(normprior(0, 1, -20, 20), 
                               lnormprior(0, 2, 0, 40))
    prior <- create_dichotomous_prior(prior, "probit")
  }
  if (dmodel == 8) {
    prior <- create_prior_list(normprior(0, 2, -20, 20), 
                               lnormprior(0, 1, 0, 18))
    prior <- create_dichotomous_prior(prior, "qlinear")
  }
  if (dmodel == 9) {
    prior <- create_prior_list(normprior(0, 2, -20, 20), 
                               lnormprior(0.424264068711929, 0.5, 0, 40), lnormprior(0, 
                                                                                     1.5, 0, 10000))
    prior <- create_dichotomous_prior(prior, "weibull")
  }
  return(prior)
}

# 
# for (i in 1:length(ls_dich_models)){
#   # This function still have the problem   
#   priors[[i]]=.bayesian_prior_dich2(ls_dich_models[i])
# }
# 


# 
# 
# ToxicR:::.bayesian_prior_dich(list(ls_dich_models))


# MLE is restored from ToxicR App 
fit_type<-list("mcmc", "laplace","mle")

ui<-navbarPage(title = "Toxic R", selected="Dichotomous Fitting",
               # Option 1. Select box option
               tabPanel("Dichotomous Fitting", 
                 fluidPage(
                   sidebarLayout(
                     sidebarPanel(
                       # Input data handling part
                       conditionalPanel(condition="input.tabs =='Input'",
                                        helpText("Inputs"),
                                        DTOutput("my_datatable"),
                                        actionButton("add",label = "Add Row"),
                                        actionButton("delete",label = "Delete Row"),
                                        actionButton("example",label = "Load Example Dataset"),
                                        fileInput("upload1", "Upload a file (.csv format)",accept=".csv")
                                        ),
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
                                        textOutput("test"),
                                        numericInput(inputId="bmr_slide2",
                                                    label="Choose a BMR level",
                                                    min=0,max=1,value=0.1),
                                        actionButton("run_dich_MA","Run",class="btn-lg btn-success")
                                      )
                                ),
                     mainPanel(
                       tabsetPanel(id="tabs",
                                   tabPanel("Input",
                                            helpText("Innput example"),
                                            column(6,DT::dataTableOutput("input_data1"))
                                            ),
                                   tabPanel("Single Model", 
                                            plotlyOutput(outputId = "dic_sing_plot"),
                                            plotOutput(outputId="dic_sing_plot_cdf"),
                                            verbatimTextOutput("sum_dich_single"),
                                            verbatimTextOutput("bmd_dich_single"),
                                            verbatimTextOutput("dich_single_parameters"),
                                            verbatimTextOutput("dich_single_covariance"),
                                            tableOutput("dich_single_gof"),
                                            downloadButton("download1", "Download")
                                            ),
                                   tabPanel("Model Average", 
                                            plotlyOutput(outputId = "dic_ma_plot"), 
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
  # Empty dataframe for dichotomous model
  v <- reactiveValues(data = { 
    data.frame(D = numeric(0),Y = numeric(0), N=numeric(0)) %>% 
      dplyr::add_row(D = rep(0,10), Y = rep(0,10), N=rep(0,10))
  })
  
  # Add button for dichotomous model
  observeEvent(input$add, {
    v$data<-v$data %>% dplyr::add_row(D=0,Y=0,N=0)
  })
  
  # Delete button for dichotomous model
  observeEvent(input$delete, {
    v$data<-v$data[-nrow(v$data),]
  })
  
  # Load example dataset
  observeEvent(input$example, {
    v$data<-data.frame(mData_df)
  })
  
  
  ## Upload input handling
  data <- reactive({
    req(input$upload1)
    ext <- tools::file_ext(input$upload1$name)
    switch(ext,
           csv = vroom::vroom(input$upload1$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  
  #Handling data process - 
  observeEvent(input$upload1,{
    output$input_data1<-DT::renderDataTable({
      datatable(data(), extensions='Select',selection ="multiple", 
                options = list(ordering = FALSE, searching = FALSE, pageLength = 10))
      
    v$data<-data()  
    })
  })
  
  
  
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE, options=list(pageLength=50, searching=FALSE))
  })
  
  
  
  # Event reactive part is added
  temp_fit<-eventReactive(input$run_dich_single,{
    isolate(v$data)
    single_dichotomous_fit(v$data$D,v$data$Y,v$data$N,model_type = input$model,fit_type = input$fit_type, BMR = input$bmr_slide)
  })
  
  # temp_fit<-reactive({
  #   req(input$run_dich_single)
  #   
  #   single_dichotomous_fit(v$data$D,v$data$Y,v$data$N,model_type = input$model,fit_type = input$fit_type, BMR = input$bmr_slide)
  # })
  
  
  
  
  
  
  temp_fit2<-eventReactive(input$run_dich_MA,{
    isolate(v$data)
    ## model fittings
    priors<-list()
    for (i in 1:length(input$dich_MA_input)){
      # This function still have the problem   
      priors[[i]]=.bayesian_prior_dich2(input$dich_MA_input[i])
    }
    
    ma_dichotomous_fit(v$data$D,v$data$Y,v$data$N,model_list = priors,
                       fit_type = input$fit_type2, BMR = input$bmr_slide)
  })
  
  output$test<-renderPrint({
    input$dich_MA_input
  })
  
  # 
  # temp_fit2 <-reactive({
  #   ma_dichotomous_fit(mData[,1],mData[,2],mData[,3],fit_type = input$fit_type2, BMR = input$bmr_slide2)
  # }) 
  
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
