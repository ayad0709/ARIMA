shinyServer(function(input, output, session) {
  library(forecast)
  library(openxlsx)
  library(tools)
  if (as.vector(Sys.info()['sysname']) == "Windows") {
    Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
  }

  
  tsMainTitle = "Main title"
  tsXlabel = "X-axis label"
  tsYlabel = "y-axix label" 
  
  
  helpLjungBox <- function(){
    print(".........................................................................................") 
    print(" The Ljung (pronounced Young) Box test ( or just the Box test)                           ")
    print(" is a way to test for the absence of serial autocorrelation, up to a specified lag k.    ")
    print("                                                                                         ")
    print(" The test determines whether or not errors are iid (i.e. white noise) or whether there   ")
    print(" is something more behind them; whether or not the autocorrelations for the errors       ")
    print(" or residuals are non zero. Essentially, it is a test of lack of fit: if the             ")
    print(" autocorrelations of the residuals are very small, we say that the model doesn’t show    ")
    print(" ‘significant lack of fit’.                                                              ")
    print(".........................................................................................") 
    print("                 (H0) il n'y a pas auto-corrélation des erreurs d'ordre 1 à r.           ") 
    print("                 (H1) il y a auto-corrélation des erreurs d'ordre 1 à r.                 ")
    print(".........................................................................................") 
    print(" Idéalement, nous aimerions ne pas rejeter l'hypothèse nulle.                            ") 
    print(" Autrement dit,                                                                          ") 
    print(" nous aimerions que la valeur p du test soit supérieure à 0,05 car                       ") 
    print(" cela signifie que les résidus de notre modèle de série chronologique sont indépendants, ") 
    print(" ce qui est souvent une hypothèse que nous faisons lors de la création d'un modèle.      ") 
    print(".........................................................................................")
    print("  Changer la valeur du 'lag' puis cliquer sur 'Soumettre' pour exectuter un autre test   ") 
    print(".........................................................................................")
  }
  
  observe({
    val <- input$time
    val1 <- input$year
    v1 <- c("Daily", "Monthly", "Quarterly", "Yearly")
    v2 <- list(c(1:366), c(1:12), c(1:4), c(1))
    v3 <- c("Day", "Month", "Quarter", "Year")
    w <- which(v1 == val)
    if (val != "Yearly") {
      updateSelectInput(
        session,
        "month",
        paste("Enter the starting", v3[w], sep = " "),
        selected = as.numeric(1),
        choices = as.numeric(v2[[w]][1]:v2[[w]][length(v2[[w]])])
      )
    }
    else {
      updateSelectInput(
        session,
        "month",
        paste("Enter the starting", v3[w], sep = " "),
        selected = val1,
        choices = val1
      )
    }
  })

  
  observe ({
    vall <- input$time
    if (vall == "Yearly") {
      updateSelectInput(
        session,
        "Model",
        "Select the Model:",
        selected = "ARIMA",
        choices = c("ARIMA", "HOLT's Exponential Smoothing")
      )
    }
    else {
      updateSelectInput(
        session,
        "Model",
        "Select the Model:",
        selected = "ARIMA",
        choices = c(
          "ARIMA",
          "HOLT's Exponential Smoothing",
          "Holt-Winters Additive",
          "Holt-Winters Multiplicative"
        )
      )
    }
  })

  
  
  

  mm <- function(Model, col, time, year, month, length) {
    inFile <- reactive({
      input$file1
    })
    d <- reactive({
      validate(
        need(
          input$file1 != "",
          "Please select a data set, right now only .txt, .csv and .xlsx data files can be processed, make sure the 1st row of your data contains the variable name."
        )
      )
      if (is.null(inFile))
        return(NULL)
      if (file_ext(inFile()$name) == "xlsx") {
        read.xlsx(inFile()$datapath)
      }
      else if (file_ext(inFile()$name) == "csv")  {
        read.csv(inFile()$datapath, header = T)
      }
      else {
        read.table(inFile()$datapath, header = T)
      }
    })
    output$fileUploaded <- reactive({
      return(!is.null(inFile()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
    
    
    xx <- c("Daily", "Monthly", "Quarterly", "Yearly")
    yy <- c(365, 12, 4, 1)
    if (time != "Yearly") {
      b <-
        ts(d()[, col],
           frequency = yy[which(xx == time)],
           start = c(year, month))
    }
    else {
      b <- ts(d()[, col], frequency = yy[which(xx == time)], start = c(year))
    }
    
    
    bb <- holt(b, h = length)
    if (Model == "ARIMA") {
      
      # Rapid Arima
      a <- auto.arima(b, trace=TRUE, allowdrift=TRUE)
      
      # Slow Arima
      # a <- auto.arima(b, stepwise=FALSE, approximation=FALSE, trace=TRUE, allowdrift=TRUE)
      
    }
    else if (Model == "Holt-Winters Additive") {
      a <- hw(b, "additive", h = length)$model
    }
    else if (Model == "Holt-Winters Multiplicative") {
      a <- hw(b, "multiplicative", h = length)$model
    }
    else {
      a <- holt(b, h = length)$model
    }
    f <- forecast(a, level = c(80, 95), h = length)
    
    #     Plot the ts with forcast
    #
    # pp <- autoplot(f, predict.size = 1, size = 1) + ggtitle(tsMainTitle) + xlab(tsXlabel) + ylab(tsYlabel) +theme_bw() 
    
    
    # pp <- autoplot(f, lwd = 2)   # , shadecols = "oldstyle"
    pp <- plot(f, lwd = 2) 
    ff <- as.data.frame(f)
    fff <- data.frame(date = row.names(ff), ff)
    
    
    modelRes <- a$resid
    modelResdf <- as.data.frame(modelRes)
    
    
    if (input$time == "Daily") {
      row.names(ff) <- ((nrow(d()) + 1):(nrow(d()) + length))
    }
    list(
      model = a,
      plot = pp,
      fore = f,
      foreT = ff,
      tab = fff,
      tsdata = d(),
      tsdata2 = b,
      modelResidual = modelResdf
    )
  }

  
  
  mmm <- function(Model, col, time, year, month, length) {
    inFile <- reactive({
      input$file1
    })
    d <- reactive({
      validate(
        need(
          input$file1 != "",
          "Please select a data set, right now only .txt, .csv and .xlsx data files can be processed, make sure the 1st row of your data contains the variable name."
        )
      )
      if (is.null(inFile))
        return(NULL)
      if (file_ext(inFile()$name) == "xlsx") {
        read.xlsx(inFile()$datapath)
      }
      else if (file_ext(inFile()$name) == "csv")  {
        read.csv(inFile()$datapath, header = T)
      }
      else {
        read.table(inFile()$datapath, header = T)
      }
    })
    output$fileUploaded <- reactive({
      return(!is.null(inFile()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
    
    
    xx <- c("Daily", "Monthly", "Quarterly", "Yearly")
    yy <- c(365, 12, 4, 1)
    if (time != "Yearly") {
      b <-
        ts(d()[, col],
           frequency = yy[which(xx == time)],
           start = c(year, month))
    }
    else {
      b <- ts(d()[, col], frequency = yy[which(xx == time)], start = c(year))
    }
    

    list(
      data = d(),
      tsdata2 = b,
      nSaison = yy[which(xx == time)]
    )
  }
  
  
  
  

  

  ####### data visualisation  ###############################################################

  output$dataPrint <- renderTable({
    myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$data
    # dd <- as.data.frame(myData)
    # ddd <- data.frame(date = row.names(dd), dd)
    #ddd 
    myData
  })
  

  ####### time series plot + ACF + PACF ###############################################################

  output$tsPlot <- renderPlot({
    myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
    plot(myData, main=tsMainTitle, xlab=tsXlabel, ylab=tsYlabel, type = 'l',lwd = 2)
  })
    
    
  output$StACF <- renderPlot({
    myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
    plot(Acf(myData), lwd = 2)
  })
    
    
    output$StPACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      plot(Pacf(myData), lwd = 2)
    })
    
    
    ####### log(St) plot + ACF + PACF ###############################################################
    
    
    output$plotLogSt <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      log_x <- log(myData)
      plot(log_x, type = 'l', lwd = 2)
    })
    
    
    output$logStACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      log_st <- log(myData)
      plot(Acf(log_st), lwd = 2)
    })
    
    
    output$logStPACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      log_st <- log(myData)
      plot(Pacf(log_st), lwd = 2)
    })
    
    
    ####### difference d'ordre 1 ################################################# ##############  
    
    output$difference1 <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(diff_st, type = 'l', lwd = 2)
    })
    
    output$d1StACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(Acf(diff_st), lwd = 2)
    })
    
    output$d1StPACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(Pacf(diff_st), lwd = 2)
    })
    
    
    
    
    ######## diff log copie ##############################################################  
    
    output$plot10a <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      dlog_st <- diff(log(myData))
      plot(dlog_st, type = 'l', lwd = 2)
    })
    
    output$d1LogStACFa <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      dlog_st <- diff(log(myData))
      plot(Acf(dlog_st), lwd = 2)
    })
    
    
    output$d1LogStPACFa <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      dlog_st <- diff(log(myData))
      plot(Pacf(dlog_st), lwd = 2)
    })
    
    ######################################################################
    
    
    
    
    
    ######################################################################
    ###########         stats plots                    ###################
    ######################################################################
   
     output$tsDisplay <- renderPlot({
       myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
       ggtsdisplay(myData)
      
    })
    
    output$boxP <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      boxplot(myData~cycle(myData),xlab="Date")
    })
    
    
    output$SubSeriesPlot <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ggsubseriesplot(myData)+
        ylab("y label") +
        ggtitle("Title")
      
    })
    
    output$SeasonPlot <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ggseasonplot(myData, year.labels=TRUE, year.labels.left=TRUE) +
        ylab("y label") +
        ggtitle("Title SeasonPlot")
    })
    
    
    output$SeasonPlotPolar <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ggseasonplot(myData, polar=TRUE) +
        ylab("y label") +
        ggtitle("Title SeasonPlotPolar")
    }) 
    
    output$lagPlot <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      gglagplot(myData)
    })
    
    
    
    ######################################################################
    ###########         differences                    ###################
    ######################################################################
    
    
    
    ######################## diff copie ##############################################  
    
    
    output$difference12 <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(diff_st, lwd = 2)
    })
    
    output$d1StACF2 <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(Acf(diff_st), lwd = 2)
    })
    
    output$d1StPACF2 <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(Pacf(diff_st), lwd = 2)
    })
    
    
    ####################### Diff Sainonniere ###############################################  
    
    output$DS1Stplot <- renderPlot({
      myData <- mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      diffS_st <- diff(myData, ns)
      plot(diffS_st, lwd = 2)
    })
    
    output$DS1StACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      diffS_st <- diff(myData, ns)
      plot(Acf(diffS_st), lwd = 2)
    })
    
    output$DS1StPACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffS_st <- diff(myData, ns)
      plot(Pacf(diffS_st), lwd = 2)
    })  
    
    ######################## diff log ############################################## 
    
    output$plot10 <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      dlog_st <- diff(log(myData))
      plot(dlog_st, lwd = 2)
    })
    
    output$d1LogStACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      dlog_st <- diff(log(myData))
      plot(Acf(dlog_st), lwd = 2)
    })
    
    
    output$d1LogStPACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      dlog_st <- diff(log(myData))
      plot(Pacf(dlog_st), lwd = 2)
    })
    
    ######################### diff d'ordre 2 #############################################  
    
    
    output$difference2 <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff2_ts <- diff(myData, differences = 2)
      plot(diff2_ts, lwd = 2)
    }) 
    
    
    output$difference2ACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff2_ts <- diff(myData, differences = 2)
      plot(Acf(diff2_ts), lwd = 2)
    })
    
    output$difference2PACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff2_ts <- diff(myData, differences = 2)
      plot(Pacf(diff2_ts), lwd = 2)
    })  
    
    
    
    ################## d[1] ( D[1] ( log(St) ) ) ####################################################  
    
    output$dDlogplot <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffS_x <- diff(diff(log(myData), ns))
      plot(diffS_x, lwd = 2)
    })
    
    output$dDlogplotACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffS_x <- diff(diff(log(myData), ns))
      plot(Acf(diffS_x), lwd = 2)
    })
    
    output$dDlogplotPACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffS_x <- diff(diff(log(myData), ns))
      plot(Pacf(diffS_x), lwd = 2)
    }) 
    
    
    
    ##################### D[1] ( log(S(t)) )#################################################  
    
    output$Dlogplot <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffSlog_x <- diff(log(myData), ns)
      plot(diffSlog_x, lwd = 2)
    })
    
    
    output$DlogplotACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffSlog_x <- diff(log(myData), ns)
      plot(Acf(diffSlog_x), lwd = 2)
    })
    
    output$DlogplotPACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffSlog_x <- diff(log(myData), ns)
      plot(Pacf(diffSlog_x), lwd = 2)
    }) 
    
    
    ######################## d[1] ( D[1] (St) ) ##############################################  
    
    
    output$ddsplot <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffS_x <- diff(diff(myData, ns))
      plot(diffS_x, lwd = 2)
    })
    
    
    output$ddsplotACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffS_x <- diff(diff(myData, ns))
      plot(Acf(diffS_x), lwd = 2)
    })
    
    output$ddsplotPACF <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffS_x <- diff(diff(myData, ns))
      plot(Pacf(diffS_x), lwd = 2)
    }) 
    
    

    
    
    
    
    ###############################################################################
    ####  ARIMA p d q P D Q S###################################################### 
    ###############################################################################
    
    

    output$PrevisionsPlotpdq <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
    
      
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = TRUE) 
      fcst <- forecast(fit,h=input$length)
      plot(fcst, lwd = 2)
      # autoplot(fcst, lwd = 2, include =2)
      
    })
    
    
    output$textARIMApdq <- renderPrint({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = TRUE) 
      fit
    })
    
    output$plotACFRespdq <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = TRUE)
      plot(Acf(fit$residuals), lwd = 2)
    })
    
    output$plotPACFRespdq <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = TRUE)
      plot(Pacf(fit$residuals), lwd = 2)
    })
    
    
    
    output$chkResARIMApdq <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = TRUE) 
      checkresiduals(fit)
    })
    
    
    output$tsdiagARIMApdq <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = TRUE) 
      ggtsdiag(fit)
    })
    
    
    

    
    output$FARIMApdq <- renderTable({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = TRUE) 
      pred <- forecast(fit, h=input$length)
      asdfpred <- as.data.frame(pred)
      dfpred <- data.frame(date = row.names(asdfpred), asdfpred)
      dfpred
    })
    
    
    output$testLBnARIMApdq <- renderPrint({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      best_ARIMA <- Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = TRUE) 
      
      helpLjungBox()
      
      myDataResiduals <- best_ARIMA$resid
      
      Box.test(myDataResiduals, lag=input$lagorder1, type="Ljung-Box")
      
    })
    
    
    output$teststationariteARIMApdq <- renderPrint({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      print(".............................................................................................") 
      print("                                                                                             ")
      print("  (H0)  la série a été générée par un processus présentant une racine unitaire,              ") 
      print("        et donc, qu'elle n'est pas stationnaire                                              ")
      print("  (H1)  la série est stationnaire                                                            ")
      print("                                                                                             ")
      print(".............................................................................................") 
      
      adf.test(myData) 
    })
    
    
    #########################################################################
    ######### decomposition #################################################
    #########################################################################
    
    
    output$decompose <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      decompose_time_Series <- decompose(myData,input$model1)
      plot(decompose_time_Series)
    })  
    
    
    output$decompose2 <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% decompose(type=input$model1) %>%
        autoplot() + 
        ggtitle("Title")
    })
    
    
    
    output$dFactors <- renderPrint({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% decompose(type=input$model1) -> fit
      print(".......................................................") 
      print("                                                       ")
      print("            Coefficients saisonnier                    ")
      print("                                                       ")
      print(".......................................................") 
      print("                                                       ")
      
      fit$figure
      
    })
    
    
    output$X11decompose <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% seas(x11="") -> fit
      autoplot(fit) +
        ggtitle("X11 decomposition of ...............")
    })
    
    
    
    output$SEATSdecompose <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% seas() %>%
        autoplot() +
        ggtitle("SEATS decomposition of ..............")
    })
    
    
    output$SEATSFactors <- renderPrint({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% seas() -> fit
      summary(fit)
    })
    
    output$X11Factors <- renderPrint({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% seas(x11="") -> fit
      summary(fit)
    })
    
    output$STLdecompose <- renderPlot({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% stl(t.window=13, s.window="periodic", robust=TRUE) %>% autoplot()
    })
    
    
    output$STLFactors <- renderPrint({
      myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% stl(t.window=13, s.window="periodic", robust=TRUE) -> fit
      fit
    })   
    
    
    #########################################################################
    ########### Forecasting Models ( SARIMA and HW ) #########################
    #########################################################################
    
    
    
  output$M <-
    renderPlot({
      mm(
        input$Model,
        input$col,
        input$time,
        input$year,
        as.numeric(input$month),
        input$length
      )[[2]]
    })
  
 
  output$P <-
    renderPrint({
      mm(
        input$Model,
        input$col,
        input$time,
        input$year,
        as.numeric(input$month),
        input$length
      )$model
    })

  output$F <-
    renderTable({
      mm(
        input$Model,
        input$col,
        input$time,
        input$year,
        as.numeric(input$month),
        input$length
      )[[5]]
    })

  
  output$chkRes <- renderPlot({
    myData<-mm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$model
    checkresiduals(myData)
  })
  
  
  output$tsdiag <- renderPlot({
    myData<-mm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$model
    ggtsdiag(myData)
  })
  
  
  
###########----------------------------------------------- 
  
  output$plotACFRes <- renderPlot({
    myData<-mm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$modelResidual
    fit<-myData
    plot(Acf(fit), lwd = 2)
    #plot(Acf(fit$residuals), lwd = 2)
    
  })
  
  output$plotPACFRes <- renderPlot({
    myData<-mm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$modelResidual
    fit<-myData
    plot(Pacf(fit), lwd = 2)
  })
  
###########-----------------------------------------------  
  
  output$testLBn <- renderPrint({
    helpLjungBox()
    
    b<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
    

        if (input$Model == "ARIMA") {
          a <- auto.arima(b, trace=FALSE, allowdrift=TRUE)
        }
        else if (input$Model == "Holt-Winters Additive") {
          a <- hw(b, "additive", h = length)$model
        }
        else if (input$Model == "Holt-Winters Multiplicative") {
          a <- hw(b, "multiplicative", h = length)$model
        }
        else {
          a <- input$Model(b, h = length)$model
        }

    
    myDataResiduals <- a$resid
    Box.test(myDataResiduals , lag=input$lagorder, type="Ljung-Box")
    
  })
  
  
  

  
  output$testTrend <- renderPrint({
    myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
    print("....................................................................................................") 
    print("                                                                                                    ") 
    print("  Un test de tendance de Mann-Kendall est utilisé pour déterminer s'il existe ou non                ") 
    print("  une tendance dans les données de séries chronologiques.                                           ") 
    print("  Il s'agit d'un test non paramétrique, ce qui signifie qu'aucune hypothèse sous-jacente            ") 
    print("  n'est faite quant à la normalité des données.                                                     ") 
    print("                                                                                                    ")
    print("  (H0) : Il n'y a pas de tendance dans la série                                                     ") 
    print("  (Ha) : Il existe une tendance dans la série                                                       ") 
    print("                                                                                                    ")
    print("....................................................................................................") 
    print("                                                                                                    ")
    
    MannKendall(myData) 
  })
  
  
  output$teststationarite <- renderPrint({
    myData<-mmm(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
    print("..............................................................................................") 
    print("                                                                                              ")
    print("  (H0)  la série a été générée par un processus présentant une racine unitaire,               ") 
    print("        et donc, qu'elle n'est pas stationnaire                                               ")
    print("  (H1)  la série est stationnaire                                                             ")
    print("                                                                                              ")
    print("..............................................................................................") 
    
    adf.test(myData) 
  })
  
  
  
  
  
  
  
  
  
  
  output$downloadData <- downloadHandler(
    filename = paste("forecast_", Sys.Date(), ".xlsx"),
    content = function(file) {
      write.xlsx(
        mm(
          input$Model,
          input$col,
          input$time,
          input$year,
          as.numeric(input$month),
          input$length
        )[[5]],
        file,
        asTable = T,
        row.names = F
      )
    }
  )

  output$downloadPlot <- downloadHandler(
    filename = paste("forecast", Sys.Date(), ".png", sep = ""),
    content = function(file) {
      png(file,
          width = 1920,               # 1280x720     
          height = 1080,              # 3840x2160
          units = "px")
      plot(
        mm(
          input$Model,
          input$col,
          input$time,
          input$year,
          as.numeric(input$month),
          input$length
        )[[3]],
        col = "red"
      )
      dev.off()
    }
  )

  output$downloadPlot2 <- downloadHandler(
    filename = paste("forecast", Sys.Date(), ".png", sep = ""),
    content = function(file) {
      png(file,
          width = 3840,               # 1280x720     
          height = 2160,              # 3840x2160
          units = "px")
      plot(
        mm(
          input$Model,
          input$col,
          input$time,
          input$year,
          as.numeric(input$month),
          input$length
        )[[3]],
        col = "red"
      )
      dev.off()
    }
  )

 
  
  
  #########################################################################
  ########  H E L P########################################################
  #########################################################################
  
  
  output$AboutAng <- renderPrint({
    
    print(".......................................................................................................") 
    print("                                                                                                       ") 
    print("                                                                                                       ")
    print("                                                                                                       ")
    print("                              University Abdelmalek Essaadi                                            ")
    print("                                                                                                       ")
    print("                                                                                                       ")
    print("                                                                                                       ")
    print(".......................................................................................................") 
    print("                                                                                                       ")
    print("        d[1] (St) : simple difference of order 1                                                       ")
    print("                                                                                                       ")
    print("                                                                                                       ")
    print("        D[1] (St) : seasonal difference of order 1                                                     ")
    print("                    change seasonality value                                                           ")
    print("                                                                                                       ")
    print("                                                                                                       ")
    print("                                                                                                       ")
    print("                                                                                                       ")
    print(".......................................................................................................") 
    print("                                                                                                       ")
    print("                                                                                                       ")
    print("                       Summary of rules for identifying ARIMA models                                   ")
    print("                                                                                                       ")
    print("                                                                                                       ")
    print("                                                                                                       ")
    print("-> Identifying the order of differencing and the constant:                                             ")
    print("   ------------------------------------------------------                                              ")
    print("                                                                                                       ")
    print("Rule 1: If the series has positive autocorrelations out to a high number of lags (say, 10 or more),    ")
    print("        then it probably needs a higher order of differencing.                                         ")
    print("                                                                                                       ")
    print("Rule 2: - If the lag-1 autocorrelation is zero or negative, or the autocorrelations are all small and  ")
    print("          patternless, then the series does not need a higher order of differencing.                   ")
    print("        - If the lag-1 autocorrelation is -0.5 or more negative, the series may be overdifferenced.    ") 
    print("        BEWARE OF OVERDIFFERENCING.                                                                    ")
    print("                                                                                                       ")
    print("Rule 3: The optimal order of differencing is often the order of differencing at which the standard     ")
    print("        deviation is lowest. (Not always, though. Slightly too much or slightly too little             ")
    print("        differencing can also be corrected with AR or MA terms. See rules 6 and 7.)                    ")
    print("                                                                                                       ")
    print("Rule 4: - A model with no orders of differencing assumes that the original series is stationary (among ")
    print("          other things, mean-revertin).                                                                ")
    print("        - A model with one order of differencing assumes that the original series has a constant       ")
    print("          average trend (e.g. a random walk or SES-type model, with or without growth).                ")
    print("        - A model with two orders of total differencing assumes that the original series has           ")
    print("          a time-varying trend (e.g. a random trend or LES-type model).                                ")
    print("                                                                                                       ")
    print("Rule 5: - A model with no orders of differencing normally includes a constant term (which allows for   ")
    print("          a non-zero mean value).                                                                      ")
    print("        - A model with two orders of total differencing normally does not includea constant term.      ")
    print("        - In a model with one order of total differencing, a constant term should be included if       ")
    print("          the series has a non-zero average trend.                                                     ")
    print("                                                                                                       ")
    print(".......................................................................................................") 
    print("                                                                                                       ")
    print("-> Identifying the numbers of AR and MA terms:                                                         ")
    print("   ------------------------------------------                                                          ")
    print("                                                                                                       ")
    print("Rule 6: If the partial autocorrelation function (PACF) of the differenced series displays a sharp      ") 
    print("        cutoff and/or the lag-1 autocorrelation is positive--i.e., if the series appears slightly      ") 
    print("        'underdifferenced'--then consider adding one or more AR terms to the model. The lag beyond     ")
    print("        which the PACF cuts off is the indicated number of AR terms.                                   ")
    print("                                                                                                       ")
    print("Rule 7: If the autocorrelation function (ACF) of the differenced series displays a sharp cutoff        ") 
    print("        and/or the lag-1 autocorrelation is negative--i.e., if the series appears slightly             ")
    print("        'overdifferenced' --then consider adding an MA term to the model.                              ") 
    print("        The lag beyond which the ACF cuts off is the indicated number of MA terms.                     ")
    print("                                                                                                       ")
    print("Rule 8: It is possible for an AR term and an MA term to cancel each other's effects,                   ") 
    print("        so if a mixed AR-MA model seems to fit the data, also try a model with one fewer AR term       ") 
    print("        and one fewer MA term--particularly if the parameter estimates in the original model require   ") 
    print("        more than 10 iterations to converge.                                                           ")
    print("        BEWARE OF USING MULTIPLE AR TERMS AND MULTIPLE MA TERMS IN THE SAME MODEL.                     ")
    print("                                                                                                       ")
    print("Rule 9: If there is a unit root in the AR part of the model--i.e., if the sum of the AR coefficients   ") 
    print("        is almost exactly 1--you should reduce the number of AR terms by one and increase the order    ")
    print("        of differencing by one.                                                                        ")
    print("                                                                                                       ")
    print("Rule 10: If there is a unit root in the MA part of the model--i.e., if the sum of the MA coefficients  ") 
    print("        is almost exactly 1--you should reduce the number of MA terms by one and reduce                ")
    print("        the order of differencing by one.                                                              ")
    print("                                                                                                       ")
    print("Rule 11: If the long-term forecasts* appear erratic or unstable, there may be a unit root              ")
    print("        in the AR or MA coefficients.                                                                  ")
    print("                                                                                                       ")
    print(".......................................................................................................") 
    print("                                                                                                       ")
    print("-> Identifying the seasonal part of the model:                                                         ")
    print("   ------------------------------------------                                                          ")
    print("                                                                                                       ")
    print("Rule 12: If the series has a strong and consistent seasonal pattern, then you must use an order of     ") 
    print("        seasonal differencing (otherwise the model assumes that the seasonal pattern will fade away    ") 
    print("        over time). However, never use more than one order of seasonal differencing or                 ") 
    print("        more than 2 orders of total differencing (seasonal+nonseasonal).                               ")
    print("                                                                                                       ")
    print("Rule 13: If the autocorrelation of the appropriately differenced series is positive at lag 's',        ") 
    print("        where 's' is the number of periods in a season, then consider adding an SAR term to the model. ") 
    print("        If the autocorrelation of the differenced series is negative at lag s, consider adding an SMA  ") 
    print("        term to the model. The latter situation is likely to occur if a seasonal difference has been   ") 
    print("        used, which should be done if the data has a stable and logical seasonal pattern.              ") 
    print("        The former is likely to occur if a seasonal differencehas not been used, which would only      ") 
    print("        be appropriate if the seasonal pattern is not stable over time.                                ") 
    print("        You should try to avoid using more than one or two seasonal parameters (SAR+SMA) in the        ") 
    print("        same model, as this is likely to lead to overfitting of the data and/or problems in estimation.")
    print("                                                                                                       ")
    print("                                                                                                       ")
    print(".......................................................................................................") 
    print("                                                                                                       ")
    print("                                                                                                       ")
    print(" *A caveat about long-term forecasting in general:                                                     ")
    print("                                                                                                       ")
    print("        linear time series models such as ARIMA and exponential smoothing models predict the more      ") 
    print("     distant future by making a series of one-period-ahead forecasts and plugging them in for unknown  ") 
    print("     future values as they look farther ahead. For example, a 2-period-ahead forecast is computed by   ") 
    print("     treating the 1-period-ahead forecast as if it were data and then applying the same forecasting    ") 
    print("      equation.                                                                                        ")
    
    print("     This step can be repeated any number of times in order to forecast as far into the future         ") 
    print("     as you want, and the method also yields formulas for computing theoretically-appropriate          ") 
    print("     confidence intervalsaround the longer-term forecasts.                                             ") 
    print("        However, the models are identified and optimized based on their one-period-ahead forecasting   ") 
    print("     performance, and rigid extrapolation of them may not be the best way to forecast many periods     ") 
    print("     ahead (say, more than one year when working with monthly or quarterly business data),             ") 
    print("     particularly when the modeling assumptions are at best only approximately satisfied               ") 
    print("     -which is nearly always the case.                                                                 ")
    
    print("     If one of your objectives is to generate long-term forecasts, it would be good to also draw on    ") 
    print("     other sources of information during the model selection process and/or to optimize the parameter  ") 
    print("     estimates for multi-period forecasting if your software allows it and/or use an auxiliary model   ") 
    print("     (possibly one that incorporates expert opinion) for long-term forecasting.                        ")
    print("                                                                                                       ")
    print("                                                                                                       ")
    print(".......................................................................................................") 
  })
  
  
  
  output$AboutFr <- renderPrint({
    
    print("..........................................................................................................") 
    print("                                                                                                          ") 
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("                              Université Abdelmalek Essaadi                                               ")
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("..........................................................................................................") 
    print("                                                                                                          ")
    print("        d[1] (St) : difference simple d'ordre 1                                                           ")
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("        D[1] (St) : difference saisonnière d'ordre 1                                                      ")
    print("                    changer la valeur de la saisonnalité                                                  ")
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("..........................................................................................................") 
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("                       Règles d'identification des modèles ARIMA                                          ")
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("-> Identifier l'ordre de différenciation et la constante:                                                 ")
    print("   ------------------------------------------------------                                                 ")
    print("                                                                                                          ")
    print("Règle 1 : si la série présente des autocorrélations positives avec un nombre élevé de décalages           ")
    print("     (par exemple, 10 ou plus), alors on a probablement besoin d'un ordre de différenciation plus élevé.  ")
    print("                                                                                                          ")
    print("Règle 2 : Si l'autocorrélation de lag-1 est nulle ou négative, ou si les autocorrélations sont toutes     ")
    print("         petites et  sans motif, alors la série n'a pas besoin d'un ordre de différenciation supérieur.   ")
    print("        - Si l'autocorrélation lag-1 est négative, -0,5 ou plus, la série peut être surdifférente.        ") 
    print("        ATTENTION AUX DIFFÉRENCES EXCESSIVES.                                                             ")
    print("                                                                                                          ")
    print("Règle 3 : L'ordre de différenciation optimal est souvent l'ordre de différenciation auquel l'écart type   ")
    print("         est le plus faible. (Pas toujours, cependant. Un peu trop ou un peu trop peu de différenciation  ")
    print("        peut également être corrigé avec des termes AR ou MA. Voir les règles 6 et 7.)                    ")
    print("                                                                                                          ")
    print("Règle 4 : Un modèle sans ordre de différenciation suppose que la série originale est stationnaire.        ")
    print("        Un modèle avec un seul ordre de différenciation suppose que la série originalea une tendance      ")
    print("        moyenne constante -par ex. un modèle de marche aléatoire ou de type SES, avec ou sans croissance. ")
    print("        - Un modèle avec deux ordres de différenciation  suppose que la série d'origine a une tendance    ")
    print("         variant dans le temps (par exemple, une tendance aléatoire ou un modèle de type LES).            ")
    print("                                                                                                          ")
    print("Règle 5 : Un modèle sans ordre de différenciation comprend normalement un terme constant - qui permet     ")
    print("          une valeur moyenne non nulle.                                                                   ")
    print("        - Un modèle avec deux ordres de différence totale n'inclut normalement pas un terme constant.     ")
    print("        - Dans un modèle avec un ordre de différenciation totale, un terme constant doit être inclus      ")
    print("          si la série a une tendance moyenne non nulle.                                                   ")
    print("                                                                                                          ")
    print("..........................................................................................................") 
    print("                                                                                                          ")
    print("-> Identification des nombres de termes AR et MA :                                                        ")
    print("   ------------------------------------------                                                             ")
    print("                                                                                                          ")
    print("Règle 6 : Si la fonction d'autocorrélation partielle (PACF) de la série différenciée affiche une forte    ") 
    print("          le seuil de coupure et/ou l'autocorrélation de lag-1 est positive, c'est-à-dire si la série     ") 
    print("          apparaît légèrement 'sous-différencié'--envisagez alors d'ajouter un ou plusieurs               ")
    print("          termes AR au modèle.                                                                            ")
    print("          Le décalage au-delà que le PACF coupe est le nombre indiqué de termes AR.                       ")
    print("                                                                                                          ")
    print("Règle 7 : Si la fonction d'autocorrélation (ACF) de la série différenciée affiche une coupure nette et/ou ") 
    print("        l'autocorrélation de lag-1 est négative, c'est-à-dire si la série apparaît légèrement             ")
    print("        'surdifférente' --envisagez alors d'ajouter un terme MA au modèle.                                ") 
    print("        Le décalage au-delà duquel l'ACF se coupe est le nombre indiqué de termes de MA.                  ")
    print("                                                                                                          ")
    print("Règle 8 : Il est possible qu'un terme AR et un terme MA s'annulent mutuellement, donc si un modèle        ") 
    print("        mixte AR-MA semble correspondre aux données, essayez également un modèle avec un terme AR de      ") 
    print("        moins et un terme MA de moins - en particulier si les estimations des paramètres dans le modèle   ") 
    print("        d'origine nécessitent plus de 10 itérations pour converger. more than 10 iterations to converge.  ")
    print("        MÉFIEZ-VOUS D'UTILISER PLUSIEURS TERMES AR ET PLUSIEURS TERMES MA DANS LE MÊME MODÈLE.            ")
    print("                                                                                                          ")
    print("Règle 9 : S'il y a une racine unitaire dans la partie AR du modèle, c'est-à-dire si la somme              ") 
    print("        des coefficients AR est presque exactement 1                                                      ")
    print("      - vous devez réduire le nombre de termes AR de un et augmenter l'ordre de différenciation de un.    ")
    print("                                                                                                          ")
    print("Règle 10 : S'il y a une racine unitaire dans la partie MA du modèle, c'est-à-dire si la somme des         ") 
    print("         coefficients MA est presque exactement 1                                                         ")
    print("        - vous devez réduire le nombre de termes MA de un et réduire l'ordre de différenciation de un.    ")
    print("                                                                                                          ")
    print("Règle 11 : Si les prévisions à long terme* apparaissent erratiques ou instables, il peut y avoir          ")
    print("          une racine unitaire dans les coefficients AR ou MA.                                             ")
    print("                                                                                                          ")
    print("..........................................................................................................") 
    print("                                                                                                          ")
    print("-> Identification de la partie saisonnière du modèle :                                                    ")
    print("   ------------------------------------------                                                             ")
    print("                                                                                                          ")
    print("Règle 12 : Si la série a un modèle saisonnier fort et cohérent, vous devez utiliser un ordre de           ") 
    print("         différenciation saisonnière (sinon le modèle suppose que le modèle saisonnier s'estompera        ") 
    print("         avec le temps). Cependant, n'utilisez jamais plus d'une commande de différenciation saisonnière  ") 
    print("         ou plus de 2 commandes de total différenciation (saisonnier+non saisonnier).                     ")
    print("                                                                                                          ")
    print(" Règle 13 : Si l'autocorrélation de la série correctement différenciée est positive au décalage s,        ") 
    print("         où s est le nombre de périodes dans une saison, puis envisagez d'ajouter un terme SAR au modèle. ")
    print("         Si l'autocorrélation de la série différenciée est négatif au décalage s, envisagez d'ajouter     ")
    print("         un terme SMA au modèle.                                                                          ") 
    print("         Le dernier situation est susceptible de se produire si une différence saisonnière a été utilisée,")
    print("         ce qui devrait être fait si les données ont été une saisonnalité stable et logique.              ")
    print("         Le premier est susceptible de se produire si une différence saisonnière n'a pas été utilisé,     ") 
    print("         ce qui ne serait approprié que si le profil saisonnier n'est pas stable dans le temps.           ")
    print("         Vous devriez essayer d'éviter d'utiliser plus d'un ou deux paramètres saisonniers (SAR + SMA)    ")
    print("         dans le même modèle, car cela est susceptible d'entraîner un surajustement des données et/ou     ") 
    print("         des problèmes d'estimation.                                                                      ")
    print("                                                                                                          ")
    print("                                                                                                          ")
    print("..........................................................................................................") 
    print("                                                                                                          ")
    print("                                                                                                          ")
    print(" *Une mise en garde sur les prévisions à long terme en général :                                          ")
    print("                                                                                                          ")
    print("      les modèles de séries chronologiques linéaires tels que ARIMA et les modèles de lissage exponentiel ") 
    print("     prédisent l'avenir le plus lointain en faisant une série de prévisions à une période et en           ") 
    print("     les branchant pour des valeurs futures inconnues à mesure qu'ils regardent plus loin.                ") 
    print("     Par exemple, une prévision à 2 périodes est calculée en traitant la prévision à 1 période comme      ") 
    print("     s'il s'agissait de données, puis en appliquant la même équation de prévision. Cette étape peut être  ") 
    print("     répétée un certain nombre de fois afin de prévoir aussi loin dans le futur que vous le souhaitez,    ") 
    print("     et la méthode donne également des formules pour calculer des intervalles de confiance théoriquement  ") 
    print("     appropriés autour des prévisions à plus long terme.                                                  ") 
    print("     Cependant, les modèles sont identifiés et optimisés en fonction de leurs performances de prévision   ") 
    print("     pour une période à l'avance, et leur extrapolation rigide peut ne pas être la meilleure façon        ") 
    print("     de prévoir de nombreuses périodes à l'avance (par exemple, plus d'un an lorsque l'on travaille avec  ") 
    print("     des données commerciales mensuelles ou trimestrielles ), en particulier lorsque les hypothèses de    ") 
    print("     modélisation ne sont au mieux qu'approximativement satisfaites, ce qui est presque toujours le cas.  ") 
    print("     Si l'un de vos objectifs est de générer des prévisions à long terme, il serait bon de puiser         ") 
    print("     également dans d'autres sources d'informations lors du processus de sélection du modèle et/ou        ") 
    print("     d'optimiser les estimations des paramètres pour la prévision multipériode si votre logiciel          ")
    print("     le permet et/ou utiliser un modèle auxiliaire pour les prévisions à long terme.                      ")
    print("                                                                                                          ")
    print("..........................................................................................................") 
  })
  
  

})
