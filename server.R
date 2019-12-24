library(shiny)
library(dplyr)
library(rlang)
library(ggplot2)
library(shinyBS)
library(shinyjs)
library(shinythemes)

# 10531890 Karuppasamy Pillai 
## Code to Plot Graph fot Hypothesis Testing
t.dist.area = function(tstat,tail,df)
{
  x = seq(-5,5,length.out=200)
  df = round(df, digits=3)
  
  if(tail=="right")
  {
    xmin=tstat
    xmax=5
    
    area = seq(xmin,xmax,length.out=200)
    dat = data.frame(x=area,ymin=0,ymax=dt(area,df=df))
    
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat, mapping=aes(x=x, ymin=ymin, ymax=ymax), fill="navy") + 
      ggtitle(paste("t-distribution with", df, "degrees of freedom")) +
      xlab("t-values") + ylab("Relative frequency") + theme_bw()
  } else if(tail=="left")
  {
    xmin=-5
    xmax=tstat
    
    area = seq(xmin,xmax,length.out=200)
    dat = data.frame(x=area,ymin=0,ymax=dt(area,df=df))
    
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat, mapping=aes(x=x, ymin=ymin, ymax=ymax), fill="navy") +
      ggtitle(paste("t-distribution with", df, "degrees of freedom")) +
      xlab("t-values") + ylab("Relative frequency") + theme_bw()
  } else if(tail=="both")
  {
    xmin1=abs(tstat)
    xmax1=5
    area1 = seq(xmin1,xmax1,length.out=200)
    dat1 = data.frame(x=area1,ymin1=0,ymax1=dt(area1,df=df))
    
    xmin2=-5
    xmax2=-abs(tstat)
    area2 = seq(xmin2,xmax2,length.out=200)
    dat2 = data.frame(x=area2,ymin2=0,ymax2=dt(area2,df=df))
    
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat1, mapping=aes(x=x, ymin=ymin1, ymax=ymax1),fill="green") +
      geom_ribbon(data=dat2, mapping=aes(x=x, ymin=ymin2, ymax=ymax2),fill="green") +
      ggtitle(paste("t-distribution with", df, "degrees of freedom")) +
      xlab("t-values") + ylab("Relative frequency") + theme_bw()
  }
  return(graph)
}

#10530990 Shaikh Sufiyan Ahmed
server <- function(input,output,session)
{

## Declaring the variables as reactiveVariable so that it can be global and used across multiple render functions  
  data <- reactiveValues()  
  index = as.integer(0)
  column_data <- reactiveValues()
  
##    Fetching the external dataset
  file_data <- reactive({
    if (!is.null(input$file)) {
      file <- input$file
        if (is.null(file)) {
          return()          
        }
    }
    data = read.csv(file=file$datapath)   
  
  }
  )
  
## Fetching the data from Inbuild Datasets in R  
  inbuild_data <- reactive({
  
    if (!is.null(input$ib)) {
      data = data.frame(get(input$ib))    
  
    }
    
  })

##  updating the column field with the selected Data Set    
  observe({
    input$ib
    {
      if (!is.null(input$ib)) {
        updateSelectInput(session,"ib_column",choices = colnames(inbuild_data()))
        
      }
    }
    
  })

  ##  updating the column field with the selected Data Set    
  observe({
    input$file
    {
      if (!is.null(input$file)) {
        updateSelectInput(session,"csv_column",choices = colnames(file_data()))
        
      }
    }
    
  })
    
  #10530985 Ritu Choudhary 
## Plotting Geometric  
  output$gplot <- renderPlot({
    data <- data.frame(get(input$ib))     
    for( val in names(data) )
    {
      index = index + 1 
      if (val == input$ib_column || val == input$csv_column) {
        column_data <- data[,index]
      }
    }
     
      D = pgeom(column_data,input$p_geom)
      barplot(D,col="RED",xlab = input$ib_column,ylab = " ")
  })

  #10531890 Karuppasamy Pillai 
### Plotting Poisson
  output$pplot <- renderPlot({

    data <- data.frame(get(input$ib))     
    for( val in names(data) )
    {
      index = index + 1 
      if (val == input$ib_column || val == input$csv_column) {
        column_data <- data[,index]
      }
    }

      D=rpois(input$s_pois,input$lam_pois)
      barplot(column_data, col='blue')
      x=0:input$end_pois
      y=dpois(x,input$lam_pois)
      plot(x,y,type='b',col="RED",xlab = input$ib_column,ylab = " ")      
    
  })
  
  #10530990 Shaikh Sufiyan Ahmed  
### Plotting Binomial
  output$bplot <- renderPlot({
    
    data <- data.frame(get(input$ib))     
    for( val in names(data) )
    {
      index = index + 1 
      if (val == input$ib_column || val == input$csv_column) {
        column_data <- data[,index]
      }
    }
    d <- density(rbinom(column_data,input$n_binom,input$p_binom))
    plot(column_data, main="Kernel Density of generated data")
    polygon(d, col="red", border="blue")
    x=0:input$n_binom
    plot(column_data,dbinom(as.integer(column_data),input$n_binom,input$p_binom),col = "RED",xlab = input$ib_column,ylab = " ")
    
  })  
  
  #10530985 Ritu Choudhary
### Plotting Normal
  output$nplot <- renderPlot({
    
    data <- data.frame(get(input$ib))     
    for( val in names(data) )
    {
      index = index + 1 
      if (val == input$ib_column || val == input$csv_column) {
        column_data <- data[,index]
      }
    }

    par(mfrow=c(1,2))
    plot(column_data,rnorm(column_data,input$mu_normal, input$sigma_normal),type='l', col='RED',xlab = input$ib_column,ylab = " ")    
    
  })    
 
  #10531890 Karuppasamy Pillai 
  ### Plotting Exponential
  output$eplot <- renderPlot({
    
    data <- data.frame(get(input$ib))     
    for( val in names(data) )
    {
      index = index + 1 
      if (val == input$ib_column || val == input$csv_column) {
        column_data <- data[,index]
      }
    }
    
    plot(column_data,rexp(column_data,input$lam_exp),type='l', col='RED',xlab = input$ib_column,ylab = " ")  
    
  })      
  
  #10530990 Shaikh Sufiyan Ahmed
  ### Plotting Uniform
  output$uplot <- renderPlot({
    
    data <- data.frame(get(input$ib))     
    for( val in names(data) )
    {
      index = index + 1 
      if (val == input$ib_column || val == input$csv_column) {
        column_data <- data[,index]
      }
    }
    
    
    rand.unif <- runif(column_data, min = input$a_unif, max = input$b_unif)
    
    hist(rand.unif,
         freq = FALSE,
         
         xlab = input$ib_column,
         ylim = c(0, 0.4),
         xlim = c(-3,3),
         density = 20,
         col = "RED",
         main = "Uniform distribution")
    
  })        
  
  #10530985 Ritu Choudhary 
### Hyphothesis code
  output$hypo1 = renderUI({
    
      if(input$alt1=="less than") 
        HTML("Ho: &mu; =", input$null1,"<p> Ha: &mu; <",input$null1)
      else if(input$alt1=="greater than")
        HTML("Ho: &mu; =", input$null1,"<p> Ha: &mu; >",input$null1)
      else 
        HTML("Ho: &mu; =", input$null1,"<p> Ha: &mu; &ne;",input$null1)
    
  })
  
  output$hypo2 = renderUI({

      if(input$alt2=="less than") 
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> <",input$null2)
      else if(input$alt2=="greater than")
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> >",input$null2)
      else 
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> &ne;",input$null2)
     
  })  
    
 ## applyinh Hypothesis on the selected column data
  mod = reactive({
    input$teststart
    isolate({
      if(input$teststart>0)
      {
          if(input$alt1=="less than") 
          {
            if (input$csv_column == "") {
              mod = t.test(x=as.numeric(as.character(unlist(inbuild_data()))),alternative="less",mu=input$null1,conf.level=1-input$alpha)                                     
              }
            else if (input$ib_column == "")
            {
              mod = t.test(x=as.numeric(as.character(unlist(file_data()))),alternative="less",mu=input$null1,conf.level=1-input$alpha)              
            }
            else
            {
              mod = t.test(x=as.numeric(as.character(unlist(file_data()))),alternative="less",mu=input$null1,conf.level=1-input$alpha)              
            }
          }
          else if(input$alt1=="greater than") 
          {
            if (input$csv_column == "") {
              mod = t.test(x=as.numeric(as.character(unlist(inbuild_data()))),alternative="greater",mu=input$null1,conf.level=1-input$alpha)
            }
             else if (input$ib_column == "")
          {
              mod = t.test(x=as.numeric(as.character(unlist(file_data()))),alternative="greater",mu=input$null1,conf.level=1-input$alpha)    
             } 
            else
            {
              mod = t.test(x=as.numeric(as.character(unlist(file_data()))),alternative="greater",mu=input$null1,conf.level=1-input$alpha)
            }
          }
          else 
          {
            if (input$csv_column == "") {
              mod = t.test(x=as.numeric(as.character(unlist(inbuild_data()))),alternative="two.sided",mu=input$null1,conf.level=1-input$alpha)
            }
        else if (input$ib_column == "")
        {
          mod = t.test(x=as.numeric(as.character(unlist(file_data()))),alternative="two.sided",mu=input$null1,conf.level=1-input$alpha)
        }
            else
            {
              mod = t.test(x=as.numeric(as.character(unlist(file_data()))),alternative="two.sided",mu=input$null1,conf.level=1-input$alpha)            
            }
          }
                          
      }
    })
  })
  
  output$est=renderUI({
    if(input$teststart>0)
    {
      HTML("x&#773; =",round(mod()$estimate[1],2))
    }
  })
  
  #10531890 Karuppasamy Pillai 
## showing the df , t-value , p-value
    output$test = renderTable({
    input$teststart
    isolate({
      if(input$teststart>0)
      {
        tab = matrix(c(mod()$parameter,mod()$statistic,mod()$p.value),nrow=1)
        colnames(tab) = c("df","t-statistic","p-value")
        rownames(tab) = "Values"
        tab
      } 
    })
  })

    #10530990 Shaikh Sufiyan Ahmed
# Plotting the Hypothesis Results    
  output$tdist = renderPlot({
    input$teststart
    isolate({
      if(input$alt1=="less than")
      {
        tail="left"
      } else if(input$alt1=="greater than")
      {
        tail="right"
      } else if(input$alt1=="two-sided")
      {
        tail="both"
      } 
      
      return(t.dist.area(mod()$statistic,tail=tail,mod()$parameter))
    })
  })
 
  #10531890 Karuppasamy Pillai 
## showing the lower and upper bound value for each scenario  
  output$citab = renderTable({
    if(input$ci & input$teststart>0)
    {
      tab = matrix(c(mod()$conf.int[1],mod()$conf.int[2]),nrow=1)
      colnames(tab) = c("Lower bound","Upper bound")
      rownames(tab) = paste(round(1-input$alpha, digits=3)*100,"% CI",sep="")
      tab
    }
  })
  
  #10531669  Shreya Goli
#### GLM Code
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  output$tb1 <- renderUI({
    tableOutput("table")
  })

# Code to enable selection of the algorithm  
  output$model_select<-renderUI({
    selectInput("modelselect","Select Algo",choices = c("Logistic_reg"="logreg"))
  })
  
# Code to enable the selection of Independent variable
  output$var1_select<-renderUI({
    if (input$ib_column == "") {
      selectInput("ind_var_select","Select Independent Var", choices =as.list(names(file_data())),multiple = FALSE) 
    }
    else if(input$csv_column == "")
      {
      selectInput("ind_var_select","Select Independent Var", choices =as.list(names(inbuild_data())),multiple = FALSE)
    }
    else
    {
      selectInput("ind_var_select","Select Independent Var", choices =as.list(names(inbuild_data())),multiple = FALSE)
    }
    
  })

## Code to enable the selection of dependent variables  
  output$rest_var_select<-renderUI({
    if (input$ib_column == "") {
      checkboxGroupInput("other_var_select","Select other Var",choices =as.list(names(file_data())))
    }
    else if (input$csv_column == "")
    {
      checkboxGroupInput("other_var_select","Select other Var",choices =as.list(names(inbuild_data())))      
    }
    else
    {
      checkboxGroupInput("other_var_select","Select other Var",choices =as.list(names(inbuild_data())))     
    }
    
  })
  
# Code to perform the Logistic Regression  
  output$other_val_show<-renderPrint({
    input$other_var_select
    input$ind_var_select
    library(caret)
    form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
    print(form)
    
    if (input$ib_column == "") {
      logreg <-glm(as.formula(form),family=binomial(),data=file_data())
    }
    else if (input$csv_column == "")
    {
      logreg <-glm(as.formula(form),family=binomial(),data=inbuild_data())  
    }
    else
    {
      logreg <-glm(as.formula(form),family=binomial(),data=inbuild_data())      
    }

# 10531890 - Karuppasamy Pillai        
    print(summary(logreg))
    output$roc_plot <- renderPlot({
      modelfit <- glm(formula=form, family=binomial(), data=inbuild_data(), na.action=na.omit)
      plot(modelfit)   
      
    })
    
  })  

## 10531890 Karuppasamy Pillai 
## Code for Descriptive analytics  
  output$ds_plot <- renderPlot({
    
    if (!is.null(inbuild_data())) 
      {
      data <- data.frame(get(input$ib))     
      for( val in names(data))
      {
        index = index + 1 
        if (val == input$ib_column) {
          column_data <- data[,index]
        }
      }    
      hist(column_data , col = "ORANGE" , xlab = input$ib_column, main="")
    }
    else if (!is.null(file_data()))
    {
        for( val in names(file_data()))
        {
          index = index + 1
          if (val == input$csv_column) {
            column_data <- inbuild_data[,index]
          }
        }
      hist(column_data , col = "ORANGE" , xlab = input$csv_column)
        }
  })
    
## 10530985 Ritu Choudhary 
## Mean of the column
    output$mean <- renderText({
    data <- data.frame(get(input$ib))     
    for( val in names(data))
    {
      index = index + 1 
      if (val == input$ib_column) {
        column_data <- data[,index]
      }
    }
    col_mean <- mean(column_data)
    form <- sprintf("The Mean of Column %s is %f",input$ib_column,col_mean)
    print(form)    
  })  

  ## Standard Deviation of the column
  output$sd <- renderText({
    data <- data.frame(get(input$ib))     
    for(val in names(data))
    {
      index = index + 1 
      if (val == input$ib_column) {
        column_data <- data[,index]
      }
    }
    col_sd <- sd(column_data)
    form <- sprintf("The Standard Deviation of Column %s is %f",input$ib_column,col_sd)
    print(form)    
  })    

  ## 10530990 Shaikh Sufiyan Ahmed
  ## Median of the column  
  output$median <- renderText({
    data <- data.frame(get(input$ib))     
    for(val in names(data))
    {
      index = index + 1 
      if (val == input$ib_column) {
        column_data <- data[,index]
      }
    }
    col_median <- median(column_data)
    form <- sprintf("The Median of Column %s is %f",input$ib_column,col_median)
    print(form)    
  })    

  ## Range of the column  
  output$range <- renderText({
    data <- data.frame(get(input$ib))     
    for(val in names(data))
    {
      index = index + 1 
      if (val == input$ib_column) {
        column_data <- data[,index]
      }
    }
    col_range <- range(column_data)
    form <- sprintf("The Lower and Upper Range of Column %s is %f",input$ib_column,col_range)
    print(form)    
  })    

  ##10531890 Karuppasamy Pillai 
  ## Minimum Value of the column  
  output$min <- renderText({
    data <- data.frame(get(input$ib))     
    for(val in names(data))
    {
      index = index + 1 
      if (val == input$ib_column) {
        column_data <- data[,index]
      }
    }
    col_min <- min(column_data)
    form <- sprintf("The Minimum value of Column %s is %f",input$ib_column,col_min)
    print(form)    
  })      

  ## Maximum value of the column  
  output$max <- renderText({
    data <- data.frame(get(input$ib))     
    for(val in names(data))
    {
      index = index + 1 
      if (val == input$ib_column) {
        column_data <- data[,index]
      }
    }
    col_max <- max(column_data)
    form <- sprintf("The Maximum value of Column %s is %f",input$ib_column,col_max)
    print(form)    
  })        
                         
}


