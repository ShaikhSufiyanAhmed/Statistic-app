library(shiny)
library(shinyjs)
library(shinythemes)

## 10530990 Shaikh Sufiyan Ahmed, 10531890 Karuppasamy Pillai, 10530985 Ritu Choudhary  
ui <- fluidPage( navbarPage(title = "Statistic-App", theme = shinytheme("superhero"),
                            useShinyjs(),
                            tabsetPanel(
                            tabPanel("Import Dataset", 
                                     tabsetPanel(
                                       
                                       tabPanel("Internal Dataset",
                                                fluidRow(column(3, selectInput(inputId = "ib", label = "Select a Data Set",choices = ls("package:datasets")),
                                                                selectInput(inputId = "ib_column" , label = "Select a Column", choices = "")
                                                )
                                                )
                                       ),
                                       
                                       tabPanel("External Dataset",
                                                fluidRow(column(3,fileInput("file", "choose a CSV File",accept = c("text/CSV",".csv")),
                                                                selectInput(inputId = "csv_column" , label = "Select a Column", choices = ""))
                                                )
                                       ))),
                            
                            tabPanel("Continous Model",
                                     tabsetPanel(
                                       tabPanel("Uniform",
                                                
                                                fluidRow(column(3,wellPanel(
                                                  # sliderInput("s_unif", "number of simulated data" ,min=1, max=1000, value = 10),
                                                  numericInput("a_unif", "parameter a in Normal" , value = -2),
                                                  numericInput("b_unif", "parameter b in Normal" , value = 0.8))), 
                                                  column(5,br(),strong("Test output:"),
                                                         plotOutput("uplot"),br(),)
                                                  
                                                )
                                                
                                                
                                       ),
# 10531890 Karuppasamy Pillai                                      
                                       tabPanel("Exponential",
                                                
                                                fluidRow(column(3,wellPanel(   condition = "input.conmodel == 'exponential'",
                                                                               numericInput("lam_exp", "parameter lambda in exponential" , value = 1)
                                                                               # numericInput("exp_supp", "support" , value = 2)
                                                )), column(5,br(),strong("Test output:"),
                                                           plotOutput("eplot"),br(),)
                                                
                                                )
                                                
                                       ),
# 10530985 Ritu Choudhary                                      
                                       tabPanel("Normal",
                                                
                                                fluidRow(column(3,wellPanel(  numericInput("mu_normal", "parameter mu in Normal" , value = 0),
                                                                              numericInput("sigma_normal", "parameter sigma in Normal" , value = 1)
                                                                              # numericInput("n_supp", "support" , value = 2)
                                                )),
                                                
                                                column(5,br(),strong("Test output:"),
                                                       plotOutput("nplot"),br(),))
                                       )
                                       
                                     )
                            ),

## 10530990 Shaikh Sufiyan Ahmed                                                     
                            tabPanel("Discrete Model",
                                     tabsetPanel(
                                       tabPanel("Binomial",
                                                
                                                fluidRow(column(3,wellPanel(    
                                                                              # numericInput("end_binom", "upper limit for x" , value = 5),
                                                                                numericInput("n_binom", "parameter n in Binomial" , value = 10),
                                                                                numericInput("p_binom", "parameter p in Binomial" , value = 0.5)  )),
                                                         column(5,br(),strong("Test output:"),
                                                                tableOutput("btab"),br(),),
                                                         column(7, 
                                                                plotOutput("bplot"),br())
                                                )
                                                
                                                
                                       ),
## 10531890 Karuppasamy Pillai                                     
                                       tabPanel("Poisson",
                                                
                                                fluidRow(column(3,wellPanel(     
                                                                                 sliderInput("s_pois", "number of simulated data" ,min=1, max=1000, value = 10),
                                                                                 numericInput("end_pois", "upper limit for x" , value = 5),
                                                                                 numericInput("lam_pois", "parameter lambda in Poisson" , value = 1)  )),
                                                         column(5,br(),strong("Test output:"),
                                                                tableOutput("ptab"),br(),),
                                                         
                                                         column(7, 
                                                                plotOutput("pplot"),br())
                                                         
                                                )
                                                
                                       ),
## 10530985 Ritu Choudhary                                      
                                       tabPanel("Geometric",
                                                
                                                fluidRow(column(3,wellPanel(  sliderInput("s_geom", "number of simulated data" ,min=1, max=1000, value = 10),
                                                                              numericInput("end_geom", "upper limit for x" , value = 5),
                                                                              numericInput("p_geom", "parameter p in Geometric" , value = 0.5)  )),
                                                         column(5,br(),
                                                                strong("Test output:"),
                                                                tableOutput("gtab"),br(),
                                                         ),
                                                         
                                                         column(7,
                                                                plotOutput("gplot"),br())
                                                         
                                                )
                                       
                                     )
                            )
                            
),

## 10531890 Karuppasamy Pillai , 10530985 Ritu Choudhary , 10530990 Shaikh Sufiyan Ahmed
tabPanel(id = "HT" , 
         fluidRow(
           
           column(3,
                  uiOutput("hypo1"),
                  tags$hr(),
                  numericInput("null1", label="Hypothesized value:", value=0),
                  selectInput("alt1", "Select a direction for Ha:", choices=list("two-sided","less than","greater than"),selected="two-sided"),
                  uiOutput("hypo2"),
                  tags$hr(),
                  sliderInput("alpha", label=HTML("Significance level &alpha;:"), value=.05, max=1, min=0, step=.01),
                  tags$hr(),
                  actionButton("teststart", strong("Perform t-test")),
                  br(),br(),br(),      
                  
           ),
           
           column(7,
                  br(),br(),
                  conditionalPanel(
                    condition="input.teststart>0",
                    column(7,
                           plotOutput("tdist"),br()),
                    column(5,br(),
                           strong("Test output:"),
                           tableOutput("test"),br(),
                           checkboxInput("showpoint","Point estimate(s):",FALSE),
                           uiOutput("est"),
                           checkboxInput("ci","Confidence interval:", FALSE),
                           tableOutput("citab"),)
                    
                  )
                  
           )),         
         value = 'ht', title="Test of Hypothesis",),

## 10531669  Shreya Goli
tabPanel(id = "GLM" , 
                  sidebarLayout(sidebarPanel(
                    uiOutput("model_select"),
                    uiOutput("var1_select"),
                    uiOutput("rest_var_select")),
                    mainPanel( helpText("Your Selected variables"),
                               verbatimTextOutput("other_val_show"),
                               plotOutput("roc_plot"))),
          
         value = 'glm', title="Generalized Linear Model",) ,

# 10531890 Karuppasamy Pillai  , 10530985 Ritu Choudhary , 10530990 Shaikh Sufiyan Ahmed
tabPanel(id = "DS" , 
         plotOutput("ds_plot"),
         textOutput("mean"),
         textOutput("sd"),
         textOutput("median"),
         textOutput("range"),
         textOutput("min"),
         textOutput("max"),
         textOutput("quantile"),
         textOutput("iqr"),
         textOutput("summary"),
         value = 'ds', title="Descriptive Statistics",) 
) , selected = "Import Dataset" )   )  