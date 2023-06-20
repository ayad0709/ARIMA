shinyServer(function(input, output, session) {
  library(forecast)
  library(openxlsx)
  library(tools)
  if (as.vector(Sys.info()['sysname']) == "Windows") {
    Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
  }

  
  tsMainTitle = ""
  tsXlabel = "date"
  tsYlabel = "RH (%)" 
  tsXlabelmd ="month"  #month ou mois
  
   
  
  observe({
    val <- input$time
    val1 <- input$year
    v1 <- c("Daily", "Monthly", "1/2 year", "Quarterly", "Yearly")
    v2 <- list(c(1:366), c(1:12), c(1:6), c(1:4), c(1))
    v3 <- c("Day", "Month", "halfYear", "Quarter", "Year")
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

  
  # load data and performe modelisation

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
    
    
    xx <- c("Daily", "Monthly", "1/2 year", "Quarterly", "Yearly")
    yy <- c(365, 12, 6, 4, 1)
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
    

    pp <- plot(f, lwd = 2, xlab=input$lab_x, ylab=input$lab_y) 
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


  
  # load data only
  
  loadData <- function(Model, col, time, year, month, length) {
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
    
    
    xx <- c("Daily", "Monthly", "1/2 year", "Quarterly", "Yearly")
    yy <- c(365, 12, 6, 4, 1)
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
    myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$data
    # dd <- as.data.frame(myData)
    # ddd <- data.frame(date = row.names(dd), dd)
    #ddd 
    print(myData)
  })
  
  
  output$data_StatsticsTable <- renderTable({
    myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$data
    describe(myData[ , c(input$col)])
  })
  
  output$data_StatsticsText <- renderPrint({
    myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$data
    # summarytools::descr(myData)
    my_Data <- myData[ , c(input$col)]
    descr(my_Data)
  })
  
  output$data_StatsticsText2 <- renderPrint({
    myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$data
    res <- stat.desc(myData[ , c(input$col)])
    round(res, 2)
  })

  ####### time series plot + ACF + PACF ###############################################################

  output$tsPlot <- renderPlot({
    myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
    plot(myData, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y, type = 'l',lwd = 2)
  })
    
  
  output$tsPlot3 <- renderPlot({
    myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
    plot(myData, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y, type = 'l',lwd = 2)
  })
    
  output$StACF <- renderPlot({
    myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
    plot(Acf(myData), lwd = 2, main=input$Main_title)
  })
    
    
    output$StPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      plot(Pacf(myData), lwd = 2, main=input$Main_title)
    })
    
    
    output$StACFPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      acf2(myData, lwd = 3, main=input$Main_title) 
    })
    
    output$tsDisplay2 <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ggtsdisplay(myData, plot.type = input$plot_type , main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
      
    })

    
    output$teststationariteSt <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      helpADF()
      
      adf.test(myData, alternative =input$alternSt, k=input$LagOrderADFSt)

    })
    
    ####### log(St) plot + ACF + PACF ###############################################################
    
    
    output$plotLogSt <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      log_x <- log(myData)
      plot(log_x, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y, type = 'l', lwd = 2)
    })
    
    
    output$logStACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      log_st <- log(myData)
      plot(Acf(log_st), lwd = 2, main=input$Main_title)
    })
    
    
    output$logStPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      log_st <- log(myData)
      plot(Pacf(log_st), lwd = 2, main=input$Main_title)
    })
    
    
    output$logStACFPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      log_st <- log(myData)
      acf2(log_st, lwd = 3, main=input$Main_title) 
    })
    
    output$log_ts_Display <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      log_st <- log(myData)
      ggtsdisplay(log_st, plot.type = input$plot_type , main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
      
    })
    
    output$teststationariteLogSt <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      helpADF()
      log_st <- log(myData)
      
      adf.test(log_st, alternative =input$alternLogSt, k=input$LagOrderADFLogSt)

    })
    
    ####### difference d'ordre 1 ################################################# ##############  
    
    output$difference1 <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(diff_st, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y, type = 'l', lwd = 2)
    })
    
    output$d1StACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(Acf(diff_st), lwd = 2, main=input$Main_title)
    })
    
    
    output$d1StPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(Pacf(diff_st), lwd = 2, main=input$Main_title)
    })
    
    output$d1StACFPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      acf2(diff_st, lwd = 3, main=input$Main_title) 
    })
    
    output$d1_ts_Display <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      ggtsdisplay(diff_st, plot.type = input$plot_type , main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
    })
    
    output$teststationarited1St <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      helpADF()
      diff_st <- diff(myData)
      adf.test(diff_st, alternative =input$alternd1St, k=input$LagOrderADFd1St)
      
    })
    
    #######################  Seasonal difference  ###############################################  
    
    output$DS1Stplot <- renderPlot({
      myData <- loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      diffS_st <- diff(myData, ns)
      plot(diffS_st, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y, lwd = 2)
    })
    
    output$DS1StACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      diffS_st <- diff(myData, ns)
      plot(Acf(diffS_st), lwd = 2, main=input$Main_title)
    })
    
    output$DS1StPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffS_st <- diff(myData, ns)
      plot(Pacf(diffS_st), lwd = 2, main=input$Main_title)
    })  
    
    
    output$DS1StACFPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffS_st <- diff(myData, ns)
      acf2(diffS_st, lwd = 3, main=input$Main_title) 
      
    })  
    
    
    output$Ds1_ts_Display <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffS_st <- diff(myData, ns)
      ggtsdisplay(diffS_st, plot.type = input$plot_type , main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
      
    })
    
    
    output$teststationariteDs1St <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      helpADF()
      diffS_st <- diff(myData, ns)
      
      adf.test(diffS_st, alternative =input$alternDs1St, k=input$LagOrderADFDs1St)
      
    })
    

    ######################## d[1] ( D[1] (St) ) ##############################################  
    
    
    output$ddsplot <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      d1_D1_St <- diff(diff(myData, ns))
      plot(d1_D1_St, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y, lwd = 2)
    })
    
    
    output$ddsplotACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      d1_D1_St <- diff(diff(myData, ns))
      plot(Acf(d1_D1_St), lwd = 2, main=input$Main_title)
    })
    
    output$ddsplotPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      d1_D1_St <- diff(diff(myData, ns))
      plot(Pacf(d1_D1_St), lwd = 2, main=input$Main_title)
    }) 
    
    
    output$ddsplotACFPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      d1_D1_St <- diff(diff(myData, ns))
      acf2(d1_D1_St, lwd = 3, main=input$Main_title) 
    }) 
    
    
    output$d1_D1_ts_Display <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      d1_D1_St <- diff(diff(myData, ns))
      ggtsdisplay(d1_D1_St, plot.type = input$plot_type , main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
      
    })
    
    output$teststationarited1Ds1St <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      helpADF()
      d1_D1_St <- diff(diff(myData, ns))
      
      adf.test(d1_D1_St, alternative =input$alternd1Ds1St, k=input$LagOrderADFd1Ds1St)
      
    })
    
    
    ######## diff log copie ##############################################################  
    
    output$plotd1Log <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      d1_log_st <- diff(log(myData))
      plot(d1_log_st, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y, type = 'l', lwd = 2)
    })
    
    output$d1LogStACFa <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      d1_log_st <- diff(log(myData))
      plot(Acf(d1_log_st), lwd = 2, main=input$Main_title)
    })
    
    
    output$d1LogStPACFa <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      d1_log_st <- diff(log(myData))
      plot(Pacf(d1_log_st), lwd = 2, main=input$Main_title)
    })
    
    
    output$d1LogStACFPACFa <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      d1_log_st <- diff(log(myData))
      acf2(d1_log_st, lwd = 3, main=input$Main_title) 
    })
    
    
    output$d1_log_ts_Display <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      d1_log_st <- diff(log(myData))
      
      ggtsdisplay(d1_log_st,plot.type = input$plot_type , main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
      
    })
    
    
    output$teststationarited1LogSt <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2

      helpADF()
      d1_log_st <- diff(log(myData))
      
      adf.test(d1_log_st, alternative =input$alternd1LogSt, k=input$LagOrderADFd1LogSt)
      
    })
    
    
    
    
    ##################### D[1] ( log(S(t)) )#################################################  
    
    output$Dlogplot <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffSlog_x <- diff(log(myData), ns)
      plot(diffSlog_x, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y, lwd = 2)
    })
    
    
    output$DlogplotACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffSlog_x <- diff(log(myData), ns)
      plot(Acf(diffSlog_x), lwd = 2, main=input$Main_title)
    })
    
    output$DlogplotPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffSlog_x <- diff(log(myData), ns)
      plot(Pacf(diffSlog_x), lwd = 2, main=input$Main_title)
    }) 
    
    
    output$DlogplotACFPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffSlog_x <- diff(log(myData), ns)
      acf2(diffSlog_x , lwd = 3, main=input$Main_title) 
      
    }) 
    
    
    output$Ds1_log_ts_Display <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      diffSlog_x <- diff(log(myData), ns)
      ggtsdisplay(diffSlog_x,plot.type = input$plot_type , main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
      
    })
    
    
    output$teststationariteDs1LogSt <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      helpADF()
      Ds1_log_St <- diff(log(myData), ns)
      
      adf.test(Ds1_log_St, alternative =input$alternDs1LogSt, k=input$LagOrderADFDs1LogSt)
      
    })
    
    
    ################## d[1] ( D[1] ( log(St) ) ) ####################################################  
    
    output$dDlogplot <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      dDlog_St <- diff(diff(log(myData), ns))
      plot(dDlog_St, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y, lwd = 2)
    })
    
    output$dDlogplotACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      dDlog_St <- diff(diff(log(myData), ns))
      plot(Acf(dDlog_St), lwd = 2, main=input$Main_title)
    })
    
    output$dDlogplotPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      dDlog_St <- diff(diff(log(myData), ns))
      plot(Pacf(dDlog_St), lwd = 2, main=input$Main_title)
    })
    
    
    output$dDlogplotACFPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      dDlog_St <- diff(diff(log(myData), ns))
      acf2(dDlog_St , lwd = 3, main=input$Main_title) 
    })
    
    
    output$d1_Ds1_log_ts_Display <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      dDlog_St <- diff(diff(log(myData), ns))
      ggtsdisplay(dDlog_St, plot.type = input$plot_type , main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
      
    })
    
    output$teststationarited1Ds1LogSt <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      helpADF()
      d1_Ds1_log_St <- diff(diff(log(myData), ns))
      
      adf.test(d1_Ds1_log_St, alternative =input$alternd1Ds1LogSt, k=input$LagOrderADFd1Ds1LogSt)
      
    })

    
    ######################### Simple difference of order 2 #############################################  
    
    
    output$difference2 <- renderPlot({
      myRawData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      if (input$islog == "Yes")
        {
        if (input$d_n==0 && input$DS_n==0 ){
           myData <- log(myRawData)
        }
        else
          {
          
          if (input$d_n==0 && input$DS_n>0 ){
             myData <- diff(log(myRawData), input$DS_n * ns)
          }
            else
              {
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(log(myRawData), difference = input$d_n)
              
            }
                else
                  {
               myData <- diff(diff(log(myRawData), input$DS_n * ns), difference = input$d_n)

            }

          }

        }
        
      }
      else
        {
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- myRawData
        }
          else
            {
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(myRawData, input$DS_n * ns)
          }
              else
                {
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(myRawData, difference = input$d_n)
              
            }
                  else
                    {
              myData <- diff(diff(myRawData, input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
      }
      
      plot(myData, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y, lwd = 2)
    }) 
    
    
    output$difference2ACF <- renderPlot({
      myRawData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      if (input$islog == "Yes"){
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- log(myRawData)
        }else{
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(log(myRawData), input$DS_n * ns)
          }else{
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(log(myRawData), difference = input$d_n)
              
            }else{
              myData <- diff(diff(log(myRawData), input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
        
      }else{
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- myRawData
        }else{
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(myRawData, input$DS_n * ns)
          }else{
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(myRawData, difference = input$d_n)
              
            }else{
              myData <- diff(diff(myRawData, input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
      }
      
      plot(Acf(myData), lwd = 2, main=input$Main_title)
    })
    
    output$difference2PACF <- renderPlot({
      myRawData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      if (input$islog == "Yes"){
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- log(myRawData)
        }else{
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(log(myRawData), input$DS_n * ns)
          }else{
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(log(myRawData), difference = input$d_n)
              
            }else{
              myData <- diff(diff(log(myRawData), input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
        
      }else{
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- myRawData
        }else{
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(myRawData, input$DS_n * ns)
          }else{
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(myRawData, difference = input$d_n)
              
            }else{
              myData <- diff(diff(myRawData, input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
      }
      
      plot(Pacf(myData), lwd = 2, main=input$Main_title)
    })  
    
    
    output$difference2ACFPACF <- renderPlot({
      myRawData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      if (input$islog == "Yes"){
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- log(myRawData)
        }else{
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(log(myRawData), input$DS_n * ns)
          }else{
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(log(myRawData), difference = input$d_n)
              
            }else{
              myData <- diff(diff(log(myRawData), input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
        
      }else{
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- myRawData
        }else{
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(myRawData, input$DS_n * ns)
          }else{
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(myRawData, difference = input$d_n)
              
            }else{
              myData <- diff(diff(myRawData, input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
      }
      
      acf2(myData , lwd = 3, main=input$Main_title) 
      
    })  
    
    
    output$d2_ts_Display <- renderPlot({
      myRawData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      if (input$islog == "Yes"){
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- log(myRawData)
        }else{
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(log(myRawData), input$DS_n * ns)
          }else{
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(log(myRawData), difference = input$d_n)
              
            }else{
              myData <- diff(diff(log(myRawData), input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
        
      }else{
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- myRawData
        }else{
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(myRawData, input$DS_n * ns)
          }else{
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(myRawData, difference = input$d_n)
              
            }else{
              myData <- diff(diff(myRawData, input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
      }
      
      ggtsdisplay(myData, plot.type = input$plot_type , main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
      
    })
    
    
    output$teststationarited2St <- renderPrint({
      myRawData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ns <-  loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      if (input$islog == "Yes"){
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- log(myRawData)
        }else{
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(log(myRawData), input$DS_n * ns)
          }else{
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(log(myRawData), difference = input$d_n)
              
            }else{
              myData <- diff(diff(log(myRawData), input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
        
      }else{
        if (input$d_n==0 && input$DS_n==0 ){
          myData <- myRawData
        }else{
          
          if (input$d_n==0 && input$DS_n>0 ){
            myData <- diff(myRawData, input$DS_n * ns)
          }else{
            
            if (input$d_n>0 && input$DS_n==0){
              
              myData <- diff(myRawData, difference = input$d_n)
              
            }else{
              myData <- diff(diff(myRawData, input$DS_n * ns), difference = input$d_n)
              
            }
            
          }
          
        }
      }
      
      helpADF2()
      
      adf.test(myData, alternative =input$alternd2St, k=input$LagOrderADFd2St)
      
    })
    
    
    
    
    
    
    ######################################################################
    ###########         stats plots                    ###################
    ######################################################################
   
     output$tsDisplay <- renderPlot({
       myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
       ggtsdisplay(myData, main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
      
    })
    
    output$boxP <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      boxplot(myData~cycle(myData), main=input$Main_title, xlab=input$lab_x, ylab=input$lab_y)
      
    })
    
    
    output$SubSeriesPlot <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ggsubseriesplot(myData)+
        xlab(input$lab_x)+
        ylab(input$lab_y) +
        ggtitle(input$Main_title)
      
    })
    
    output$SeasonPlot <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ggseasonplot(myData, year.labels=TRUE, year.labels.left=TRUE) +
        xlab(input$lab_x)+
        ylab(input$lab_y) +
        ggtitle(input$Main_title)
    })
    
    
    output$SeasonPlotPolar <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      ggseasonplot(myData, polar=TRUE) +
        xlab(input$lab_x)+
        ylab(input$lab_y) +
        ggtitle(input$Main_title)
    }) 
    
    output$lagPlot <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      gglagplot(myData)+
        xlab(input$lab_x)+
        ylab(input$lab_y) +
        ggtitle(input$Main_title)
    })
    
    
    
    ######################################################################
    ###########         differences                    ###################
    ######################################################################
    
    
    
    ######################## diff copie ##############################################  
    
    
    output$difference12 <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(diff_st, main=tsMainTitle, xlab=tsXlabel, ylab=tsYlabel, lwd = 2)
    })
    
    output$d1StACF2 <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(Acf(diff_st), lwd = 2)
    })
    
    output$d1StPACF2 <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      diff_st <- diff(myData)
      plot(Pacf(diff_st), lwd = 2)
    })
    
    

    
    ######################## diff log ############################################## 
    
    output$plot10 <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      dlog_st <- diff(log(myData))
      plot(dlog_st, main=tsMainTitle, xlab=tsXlabel, ylab=tsYlabel, lwd = 2)
    })
    
    output$d1LogStACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      dlog_st <- diff(log(myData))
      plot(Acf(dlog_st), lwd = 2)
    })
    
    
    output$d1LogStPACF <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      dlog_st <- diff(log(myData))
      plot(Pacf(dlog_st), lwd = 2)
    })
    

    
  

    
    
    ###############################################################################
    ####  ARIMA p d q P D Q S###################################################### 
    ###############################################################################
    


    output$PrevisionsPlotpdq <- renderPlot({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
    
      
      model_fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      fcst <- forecast(model_fit,h=input$length)
      plot(fcst, lwd = 2)
      # autoplot(fcst, lwd = 2, include =2)
      
    })
    
    
    output$unitCerclepdq <- renderPlot({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      
      model_fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      #fcst <- forecast(model_fit,h=input$length)
      #plot(fcst, lwd = 2)
      plot(model_fit) 
      
    })
    
  #       
      
    output$textARIMApdq <- renderPrint({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      model_fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      # summary(model_fit)
      model_fit
    })
    
    output$textARIMApdq_pvalues <- renderPrint({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      model_fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      # summary(model_fit)
      cat("............................................................................\n") 
      cat("                     Testing the coefficients values                        \n")
      cat("............................................................................\n") 
      cat(" H0 : the coefficient = 0                                                   \n")
      cat(" Ha : the coefficient is different from 0                                   \n")
      cat("............................................................................\n") 
      cat(" p-value < 0.05 indicates that the corresponding coefficient is             \n")
      cat("                significantly different from 0                              \n")
      cat("............................................................................\n") 
      coeftest(model_fit)
    })
    
    
    output$plotACFRespdq <- renderPlot({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      model_fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)
      plot(Acf(model_fit$residuals), lwd = 2)
    })
    
    
    output$plotPACFRespdq <- renderPlot({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)
      plot(Pacf(fit$residuals), lwd = 2)
    })
    
    output$plotACFPACFRespdq <- renderPlot({
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }

      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = TRUE)
      nsais <- loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      mainTitle = paste('Residuals ARIMA(',input$ARIMAp,',',input$ARIMAd,',',input$ARIMAq,')','(',input$ARIMAps,',',input$ARIMAds,',',input$ARIMAqs,')','[',nsais,']')
      
      acf2(fit$residuals, lwd = 3, main=mainTitle) 
    })
    
    
    output$plot_ACF_PACF_Res_pdq <- renderPlot({
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)
      nsais <- loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      mainTitle = paste('Residuals ARIMA(',input$ARIMAp,',',input$ARIMAd,',',input$ARIMAq,')','(',input$ARIMAps,',',input$ARIMAds,',',input$ARIMAqs,')','[',nsais,']')
      
      acf2(fit$residuals, lwd = 3, main=mainTitle) 
    })
    
    output$chkResARIMApdq <- renderPlot({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      checkresiduals(fit)
    })
    
    
    
    
    output$SARIMAplot <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      
      nsais <- loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      sarima(myData, p = input$ARIMAp, d = input$ARIMAd, q = input$ARIMAq, P = input$ARIMAps, D = input$ARIMAds, Q = input$ARIMAqs, S = nsais, lwd = 2)
    })
    
    output$SARIMAplot2 <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      
      nsais <- loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      sarima(myData, p = input$ARIMAp, d = input$ARIMAd, q = input$ARIMAq, P = input$ARIMAps, D = input$ARIMAds, Q = input$ARIMAqs, S = nsais, lwd = 2)
    })
    
    
    
    ###########-----------------   
    
    
    
    output$SARIMAforcastplot2 <- renderPlot({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      
      nsais <- loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      
      #sarima.for(myData, n.ahead = input$length, p = input$ARIMAp, d = input$ARIMAd, q = input$ARIMAq, P = input$ARIMAps, D = input$ARIMAds, Q = input$ARIMAqs, S = nsais, lwd = 2)
      sarima.for(myData, n.ahead = input$length, arimaorder(fit),S = nsais, lwd = 2)
     
      
    })  
    
    
    ###########-----------------   
    
    
    
    
    output$SARIMAforcastplot <- renderPlot({
      
      if (input$driftYN == "TRUE") {
        nodriftConsideration =FALSE
      }
      else {
        nodriftConsideration =TRUE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      
      nsais <- loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$nSaison
      
      sarima.for(myData, n.ahead = input$length, p = input$ARIMAp, d = input$ARIMAd, q = input$ARIMAq, P = input$ARIMAps, D = input$ARIMAds, Q = input$ARIMAqs, S = nsais, no.constant=nodriftConsideration, lwd = 2, main=input$Main_title, xlab=input$lab_x)
      
       })
    
    
    output$tsdiagARIMApdq <- renderPlot({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      ggtsdiag(fit)
        # xlab(input$lab_x)+
        # ylab(input$lab_y) +
        # ggtitle(input$Main_title)
    })
    
    
    output$tsdiag2 <- renderPlot({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      qqnorm(resid(fit), main = "Normal Q-Q Plot, Residual", col = "darkgrey")
      qqline(resid(fit), col = "dodgerblue", lwd = 2)
   
    })

    
    output$ShapiroTest <- renderPrint({
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      ResudialData = resid(fit)
      cat("..........................................................................\n") 
      cat(" The Shapiro-Wilk test is a statistical test used to check if             \n")
      cat(" a continuous variable follows a normal distribution.                     \n")
      cat("..........................................................................\n") 
      cat(" (H0) states that the variable is normally distributed.                   \n")
      cat(" (H1) states that the variable is NOT normally distributed.               \n")
      cat("..........................................................................\n") 
      cat(" If p ≤ 0.05: then the null hypothesis can be rejected                    \n")
      cat("              (i.e. the variable is NOT normally distributed).            \n")
      cat(" If p > 0.05: then the null hypothesis cannot be rejected                 \n")
      cat("              (i.e. the variable MAY BE normally distributed).            \n")
      cat("..........................................................................\n") 
      
      shapiro.test(ResudialData)
    })
    
    
    ###########-----------------   
    
    output$sarima_Model <- renderPrint({
      # https://cran.r-project.org/web/packages/equatiomatic/vignettes/forecast-arima.html 

      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      simple_ts_mod<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      
      texmodel <- extract_eq(simple_ts_mod)
      
      TeX(texmodel)
      
     
      #plot(TeX(texmodel), cex=2, main="")
      
    })  
    
    
    output$sarima_Model_Plot <- renderPlot({
      # https://cran.r-project.org/web/packages/equatiomatic/vignettes/forecast-arima.html 
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      simple_ts_mod<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      
      texmodel <- extract_eq(simple_ts_mod)
      
      
      plot(TeX(texmodel))
      
    }) 
    
    ###########-----------------   
    
    
    output$FARIMApdq <- renderTable({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      pred <- forecast(fit, h=input$length)
      asdfpred <- as.data.frame(pred)
      dfpred <- data.frame(date = row.names(asdfpred), asdfpred)
      dfpred
    })
    
    

    
    
    output$testTrendMK2 <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2

      helpMK()
      
      MannKendall(myData) 
    })
    
    
    
    output$kpssTest2 <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2

      helpKPSS()
      
      kpss.test(myData, null = "Trend") 
    })
    
    
    output$DFGLS <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      cat(".........................................................................................\n") 
      cat("     DF-GLS Unit Root Test                                                               \n")
      cat(".........................................................................................\n") 
      cat("                                                                                         \n")
     
      summary(ur.ers(myData,  model = "trend",lag.max = 4)) 
    })
    
    
    
    
    output$testLBnARIMApdq <- renderPrint({
      
      if (input$driftYN == "TRUE") {
        driftConsideration =TRUE
      }
      else {
        driftConsideration =FALSE
      }
      
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      best_ARIMA <- Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration) 
      
      helpLjungBox()
      
      myDataResiduals <- best_ARIMA$resid
      
      Box.test(myDataResiduals, lag=input$lagorder1, type=input$typeBoxTest1)
      
    })
  

 
    
    output$teststationariteARIMApdq <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2

      helpADF()

      # adf.test(myData, alternative ="stationary", k=12) 
      
      adf.test(myData, alternative =input$altern2, k=input$LagOrderADF2)
      
    })
    
    
    
    #########################################################################
    ######### decomposition #################################################
    #########################################################################
    
    
    output$decompose <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      decompose_time_Series <- decompose(myData,input$model1)
      plot(decompose_time_Series, lwd = 2)
    })  
    
    
    output$decompose2 <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% decompose(type=input$model1) %>%
        autoplot() + 
        ggtitle("Title")
    })
    
    
    
    output$dFactors <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
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
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% seas(x11="") -> fit
      autoplot(fit) +
        ggtitle("X11 decomposition of ...............")
    })
    
    
    
    output$SEATSdecompose <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% seas() %>%
        autoplot() +
        ggtitle("SEATS decomposition of ..............")
    })
    
    
    output$SEATSFactors <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% seas() -> fit
      summary(fit)
    })
    
    output$X11Factors <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% seas(x11="") -> fit
      summary(fit)
    })
    
    output$STLdecompose <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      myData %>% stl(t.window=13, s.window="periodic", robust=TRUE) %>% autoplot()
    })
    
    
    output$STLFactors <- renderPrint({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
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
    
    
    
    output$Pslow <-
      renderPrint({
        myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
        
        model_fit <- auto.arima(myData,
                          max.p = input$maxp,
                          max.d = input$maxd,
                          max.q = input$maxq,
                          max.P = input$maxPs,
                          max.D = input$maxDs,
                          max.Q = input$maxQs,
                          max.order = input$maxorder,
                          stepwise=FALSE,
                          approximation=FALSE,
                          trace=TRUE,
                          allowdrift=TRUE,
                          test = c("kpss", "adf", "pp")
                    )

        model_fit
        
      })
    
    
    output$unitCercle <- renderPlot({
      myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2
      
      model_fit <- mm(
                      input$Model,
                      input$col,
                      input$time,
                      input$year,
                      as.numeric(input$month),
                      input$length
                    )$model
      
      plot(model_fit) 
      
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
  

  output$testLBn <- renderPrint({
    helpLjungBox()
    b<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2

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
    Box.test(myDataResiduals, lag=input$lagorder, type=input$typeBoxTest)
    

  })
  
  
  

  
  output$testTrendMK <- renderPrint({
    myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2

    helpMK()
    
    MannKendall(myData) 
    
  })
  
  output$kpssTest <- renderPrint({
    myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2

    helpKPSS()
    
    kpss.test(myData, null = "Trend") 
  })
  
  
  
  output$teststationarite <- renderPrint({
    myData<-loadData(input$Model,input$col,input$time,input$year,as.numeric(input$month),input$length)$tsdata2

    helpADF()
    
    print("..............................................................")
    
    # adf.test(myData)
    
    # adf.test(myData, alternative ="stationary", k=12) 
    
    adf.test(myData, alternative =input$altern, k=input$LagOrderADF)
    
    # adf.test(x, alternative = c("stationary", "explosive"), k = trunc((length(x)-1)^(1/3)))
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
    
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n") 
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                              University Abdelmalek Essaadi                                            \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n")
    cat("        d[1] (St) : simple difference of order 1                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("        D[1] (St) : seasonal difference of order 1                                                     \n")
    cat("                    change seasonality value                                                           \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                       Summary of rules for identifying ARIMA models                                   \n")
    cat("                                                                                                       \n")
    cat("   https://people.duke.edu/~rnau/arimrule.htm                                                          \n")
    cat("                                                                                                       \n")
    cat("-> Identifying the order of differencing and the constant:                                             \n")
    cat("   ------------------------------------------------------                                              \n")
    cat("                                                                                                       \n")
    cat("Rule 1: If the series has positive autocorrelations out to a high number of lags (say, 10 or more),    \n")
    cat("        then it probably needs a higher order of differencing.                                         \n")
    cat("                                                                                                       \n")
    cat("Rule 2: - If the lag-1 autocorrelation is zero or negative, or the autocorrelations are all small and  \n")
    cat("          patternless, then the series does not need a higher order of differencing.                   \n")
    cat("        - If the lag-1 autocorrelation is -0.5 or more negative, the series may be overdifferenced.    \n") 
    cat("        BEWARE OF OVERDIFFERENCING.                                                                    \n")
    cat("                                                                                                       \n")
    cat("Rule 3: The optimal order of differencing is often the order of differencing at which the standard     \n")
    cat("        deviation is lowest. (Not always, though. Slightly too much or slightly too little             \n")
    cat("        differencing can also be corrected with AR or MA terms. See rules 6 and 7.)                    \n")
    cat("                                                                                                       \n")
    cat("Rule 4: - A model with no orders of differencing assumes that the original series is stationary (among \n")
    cat("          other things, mean-revertin).                                                                \n")
    cat("        - A model with one order of differencing assumes that the original series has a constant       \n")
    cat("          average trend (e.g. a random walk or SES-type model, with or without growth).                \n")
    cat("        - A model with two orders of total differencing assumes that the original series has           \n")
    cat("          a time-varying trend (e.g. a random trend or LES-type model).                                \n")
    cat("                                                                                                       \n")
    cat("Rule 5: - A model with no orders of differencing normally includes a constant term (which allows for   \n")
    cat("          a non-zero mean value).                                                                      \n")
    cat("        - A model with two orders of total differencing normally does not includea constant term.      \n")
    cat("        - In a model with one order of total differencing, a constant term should be included if       \n")
    cat("          the series has a non-zero average trend.                                                     \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n")
    cat("-> Identifying the numbers of AR and MA terms:                                                         \n")
    cat("   ------------------------------------------                                                          \n")
    cat("                                                                                                       \n")
    cat("Rule 6: If the partial autocorrelation function (PACF) of the differenced series displays a sharp      \n") 
    cat("        cutoff and/or the lag-1 autocorrelation is positive--i.e., if the series appears slightly      \n") 
    cat("        'underdifferenced'--then consider adding one or more AR terms to the model. The lag beyond     \n")
    cat("        which the PACF cuts off is the indicated number of AR terms.                                   \n")
    cat("                                                                                                       \n")
    cat("Rule 7: If the autocorrelation function (ACF) of the differenced series displays a sharp cutoff        \n") 
    cat("        and/or the lag-1 autocorrelation is negative--i.e., if the series appears slightly             \n")
    cat("        'overdifferenced' --then consider adding an MA term to the model.                              \n") 
    cat("        The lag beyond which the ACF cuts off is the indicated number of MA terms.                     \n")
    cat("                                                                                                       \n")
    cat("Rule 8: It is possible for an AR term and an MA term to cancel each other's effects,                   \n") 
    cat("        so if a mixed AR-MA model seems to fit the data, also try a model with one fewer AR term       \n") 
    cat("        and one fewer MA term--particularly if the parameter estimates in the original model require   \n") 
    cat("        more than 10 iterations to converge.                                                           \n")
    cat("        BEWARE OF USING MULTIPLE AR TERMS AND MULTIPLE MA TERMS IN THE SAME MODEL.                     \n")
    cat("                                                                                                       \n")
    cat("Rule 9: If there is a unit root in the AR part of the model--i.e., if the sum of the AR coefficients   \n") 
    cat("        is almost exactly 1--you should reduce the number of AR terms by one and increase the order    \n")
    cat("        of differencing by one.                                                                        \n")
    cat("                                                                                                       \n")
    cat("Rule 10: If there is a unit root in the MA part of the model--i.e., if the sum of the MA coefficients  \n") 
    cat("        is almost exactly 1--you should reduce the number of MA terms by one and reduce                \n")
    cat("        the order of differencing by one.                                                              \n")
    cat("                                                                                                       \n")
    cat("Rule 11: If the long-term forecasts* appear erratic or unstable, there may be a unit root              \n")
    cat("        in the AR or MA coefficients.                                                                  \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n")
    cat("-> Identifying the seasonal part of the model:                                                         \n")
    cat("   ------------------------------------------                                                          \n")
    cat("                                                                                                       \n")
    cat("Rule 12: If the series has a strong and consistent seasonal pattern, then you must use an order of     \n") 
    cat("        seasonal differencing (otherwise the model assumes that the seasonal pattern will fade away    \n") 
    cat("        over time). However, never use more than one order of seasonal differencing or                 \n") 
    cat("        more than 2 orders of total differencing (seasonal+nonseasonal).                               \n")
    cat("                                                                                                       \n")
    cat("Rule 13: If the autocorrelation of the appropriately differenced series is positive at lag 's',        \n") 
    cat("        where 's' is the number of periods in a season, then consider adding an SAR term to the model. \n") 
    cat("        If the autocorrelation of the differenced series is negative at lag s, consider adding an SMA  \n") 
    cat("        term to the model. The latter situation is likely to occur if a seasonal difference has been   \n") 
    cat("        used, which should be done if the data has a stable and logical seasonal pattern.              \n") 
    cat("        The former is likely to occur if a seasonal differencehas not been used, which would only      \n") 
    cat("        be appropriate if the seasonal pattern is not stable over time.                                \n") 
    cat("        You should try to avoid using more than one or two seasonal parameters (SAR+SMA) in the        \n") 
    cat("        same model, as this is likely to lead to overfitting of the data and/or problems in estimation.\n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(" *A caveat about long-term forecasting in general:                                                     \n")
    cat("                                                                                                       \n")
    cat("        linear time series models such as ARIMA and exponential smoothing models predict the more      \n") 
    cat("     distant future by making a series of one-period-ahead forecasts and plugging them in for unknown  \n") 
    cat("     future values as they look farther ahead. For example, a 2-period-ahead forecast is computed by   \n") 
    cat("     treating the 1-period-ahead forecast as if it were data and then applying the same forecasting    \n") 
    cat("      equation.                                                                                        \n")
    
    cat("     This step can be repeated any number of times in order to forecast as far into the future         \n") 
    cat("     as you want, and the method also yields formulas for computing theoretically-appropriate          \n") 
    cat("     confidence intervalsaround the longer-term forecasts.                                             \n") 
    cat("        However, the models are identified and optimized based on their one-period-ahead forecasting   \n") 
    cat("     performance, and rigid extrapolation of them may not be the best way to forecast many periods     \n") 
    cat("     ahead (say, more than one year when working with monthly or quarterly business data),             \n") 
    cat("     particularly when the modeling assumptions are at best only approximately satisfied               \n") 
    cat("     -which is nearly always the case.                                                                 \n")
    
    cat("     If one of your objectives is to generate long-term forecasts, it would be good to also draw on    \n") 
    cat("     other sources of information during the model selection process and/or to optimize the parameter  \n") 
    cat("     estimates for multi-period forecasting if your software allows it and/or use an auxiliary model   \n") 
    cat("     (possibly one that incorporates expert opinion) for long-term forecasting.                        \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
  })
  

  output$AboutFr <- renderPrint({
    
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n") 
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                              Université Abdelmalek Essaadi                                            \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n")
    cat("        d[1] (St) : difference simple d'ordre 1                                                        \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("        D[1] (St) : difference saisonnière d'ordre 1                                                   \n")
    cat("                    changer la valeur de la saisonnalité                                               \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                       Règles d'identification des modèles ARIMA                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("-> Identifier l'ordre de différenciation et la constante:                                              \n")
    cat("   ------------------------------------------------------                                              \n")
    cat("                                                                                                       \n")
    cat("Règle 1 : si la série présente des autocorrélations positives avec un nombre élevé de décalages        \n")
    cat("  (par exemple, 10 ou plus), alors on a probablement besoin d'un ordre de différenciation plus élevé.  \n")
    cat("                                                                                                       \n")
    cat("Règle 2 : Si l'autocorrélation de lag-1 est nulle ou négative, ou si les autocorrélations sont toutes  \n")
    cat("       petites et  sans motif, alors la série n'a pas besoin d'un ordre de différenciation supérieur.  \n")
    cat("        - Si l'autocorrélation lag-1 est négative, -0,5 ou plus, la série peut être sur-différencié.   \n") 
    cat("        ATTENTION AUX DIFFÉRENCES EXCESSIVES.                                                          \n")
    cat("                                                                                                       \n")
    cat("Règle 3 : L'ordre de différenciation optimal est souvent l'ordre de différenciation auquel l'écart     \n")
    cat("      type est le plus faible. (Pas toujours, cependant. Un peu trop ou un peu trop peu de             \n")
    cat("      différenciation peut également être corrigé avec des termes AR ou MA. Voir les règles 6 et 7.)   \n")
    cat("                                                                                                       \n")
    cat("Règle 4 : Un modèle sans ordre de différenciation suppose que la série originale est stationnaire.     \n")
    cat("        Un modèle avec un seul ordre de différenciation suppose que la série originalea une tendance   \n")
    cat("        moyenne constante -par ex. un modèle de marche aléatoire ou de type SES,                       \n")
    cat("        avec ou sans croissance.                                                                       \n")
    cat("        - Un modèle avec deux ordres de différenciation  suppose que la série d'origine a une          \n")
    cat("       tendance variant dans le temps (par exemple, une tendance aléatoire ou un modèle de type LES).  \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("Règle 5 : Un modèle sans ordre de différenciation comprend normalement un terme constant - qui         \n")
    cat("          permet une valeur moyenne non nulle.                                                         \n")
    cat("        - Un modèle avec deux ordres de différence totale n'inclut normalement pas un terme constant.  \n")
    cat("        - Dans un modèle avec un ordre de différenciation totale, un terme constant doit être inclus   \n")
    cat("          si la série a une tendance moyenne non nulle.                                                \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n")
    cat("-> Identification des nombres de termes AR et MA :                                                     \n")
    cat("   ------------------------------------------                                                          \n")
    cat("                                                                                                       \n")
    cat("Règle 6 : Si la fonction d'autocorrélation partielle (PACF) de la série différenciée affiche           \n") 
    cat("          une forte le seuil de coupure et/ou l'autocorrélation de lag-1 est positive,                 \n") 
    cat("          c'est-à-dire si la série apparaît légèrement 'sous-différencié'--envisagez alors             \n")
    cat("          d'ajouter un ou plusieurs termes AR au modèle.                                               \n")
    cat("          Le décalage au-delà que le PACF coupe est le nombre indiqué de termes AR.                    \n")
    cat("                                                                                                       \n")
    cat("Règle 7 : Si la fonction d'autocorrélation (ACF) de la série différenciée affiche une coupure nette    \n") 
    cat("        et/ou  l'autocorrélation de lag-1 est négative, c'est-à-dire si la série apparaît légèrement   \n")
    cat("        'surdifférente' --envisagez alors d'ajouter un terme MA au modèle.                             \n") 
    cat("        Le décalage au-delà duquel l'ACF se coupe est le nombre indiqué de termes de MA.               \n")
    cat("                                                                                                       \n")
    cat("Règle 8 : Il est possible qu'un terme AR et un terme MA s'annulent mutuellement,                       \n") 
    cat("        donc si un modèle mixte AR-MA semble correspondre aux données, essayez également               \n") 
    cat("        un modèle avec un terme AR de moins et un terme MA de moins                                    \n") 
    cat("        - en particulier si les estimations des paramètres dans le modèle d'origine nécessitent        \n")
    cat("         plus de 10 itérations pour converger. more than 10 iterations to converge.                    \n")
    cat("        MÉFIEZ-VOUS D'UTILISER PLUSIEURS TERMES AR ET PLUSIEURS TERMES MA DANS LE MÊME MODÈLE.         \n")
    cat("                                                                                                       \n")
    cat("Règle 9 : S'il y a une racine unitaire dans la partie AR du modèle, c'est-à-dire si la somme           \n") 
    cat("        des coefficients AR est presque exactement 1                                                   \n")
    cat("    - vous devez réduire le nombre de termes AR de un et augmenter l'ordre de différenciation de un.   \n")
    cat("                                                                                                       \n")
    cat("Règle 10 : S'il y a une racine unitaire dans la partie MA du modèle, c'est-à-dire si la somme des      \n") 
    cat("         coefficients MA est presque exactement 1                                                      \n")
    cat("    - vous devez réduire le nombre de termes MA de un et réduire l'ordre de différenciation de un.     \n")
    cat("                                                                                                       \n")
    cat("Règle 11 : Si les prévisions à long terme* apparaissent erratiques ou instables, il peut y avoir       \n")
    cat("          une racine unitaire dans les coefficients AR ou MA.                                          \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n")
    cat("-> Identification de la partie saisonnière du modèle :                                                 \n")
    cat("   ------------------------------------------                                                          \n")
    cat("                                                                                                       \n")
    cat("Règle 12 : Si la série a un modèle saisonnier fort et cohérent, vous devez utiliser un ordre de        \n") 
    cat("         différenciation saisonnière (sinon le modèle suppose que le modèle saisonnier s'estompera     \n") 
    cat("         avec le temps). Cependant, n'utilisez jamais plus d'une commande de différenciation           \n") 
    cat("         saisonnière ou plus de 2 commandes de total différenciation (saisonnier+non saisonnier).      \n")
    cat("                                                                                                       \n")
    cat(" Règle 13 : Si l'autocorrélation de la série correctement différenciée est positive au décalage 's',   \n") 
    cat("        où 's' est le nombre de périodes dans une saison,  puis envisagez d'ajouter un terme SAR       \n")
    cat("        au modèle.                                                                                     \n")
    cat("        Si l'autocorrélation de la série différenciée est négatif au décalage 's', envisagez           \n")
    cat("        d'ajouter un terme SMA au modèle.                                                              \n") 
    cat("        Le dernier situation est susceptible de se produire si une différence saisonnière a été        \n")
    cat("        utilisée,ce qui devrait être fait si les données ont été une saisonnalité stable et logique.   \n")
    cat("        Le premier est susceptible de se produire si une différence saisonnière n'a pas été utilisé,   \n") 
    cat("        ce qui ne serait approprié que si le profil saisonnier n'est pas stable dans le temps.         \n")
    cat("        Vous devriez essayer d'éviter d'utiliser plus d'un ou deux paramètres saisonniers (SAR + SMA)  \n")
    cat("        dans le même modèle, car cela est susceptible d'entraîner un surajustement des données et/ou   \n") 
    cat("        des problèmes d'estimation.                                                                    \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(" *Une mise en garde sur les prévisions à long terme en général :                                       \n")
    cat("                                                                                                       \n")
    cat("      les modèles de séries chronologiques linéaires tels que ARIMA et les modèles de lissage          \n") 
    cat("     exponentiel prédisent l'avenir le plus lointain en faisant une série de prévisions à une période  \n") 
    cat("     et en les branchant pour des valeurs futures inconnues à mesure qu'ils regardent plus loin.       \n") 
    cat("     Par exemple, une prévision à 2 périodes est calculée en traitant la prévision à 1 période comme   \n") 
    cat("     s'il s'agissait de données, puis en appliquant la même équation de prévision. Cette étape peut    \n") 
    cat("     être répétée un certain nombre de fois afin de prévoir aussi loin dans le futur que vous          \n") 
    cat("     le souhaitez, et la méthode donne également des formules pour calculer des intervalles de         \n") 
    cat("     confiance théoriquement appropriés autour des prévisions à plus long terme.                       \n") 
    cat("     Cependant, les modèles sont identifiés et optimisés en fonction de leurs performances             \n") 
    cat("     de prévision pour une période à l'avance, et leur extrapolation rigide peut ne pas être           \n") 
    cat("     la meilleure façon de prévoir de nombreuses périodes à l'avance (par exemple, plus d'un an        \n") 
    cat("     lorsque l'on travaille avec des données commerciales mensuelles ou trimestrielles ),              \n") 
    cat("     en particulier lorsque les hypothèses de modélisation ne sont au mieux qu'approximativement       \n")
    cat("     satisfaites, ce qui est presque toujours le cas.                                                  \n")
    cat("     Si l'un de vos objectifs est de générer des prévisions à long terme, il serait bon de puiser      \n") 
    cat("     également dans d'autres sources d'informations lors du processus de sélection du modèle et/ou     \n") 
    cat("     d'optimiser les estimations des paramètres pour la prévision multipériode si votre logiciel       \n")
    cat("     le permet et/ou utiliser un modèle auxiliaire pour les prévisions à long terme.                   \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n") 
  })
  
  
  
  helpLjungBoxFr <- function(){
    cat(".........................................................................................\n") 
    cat(" La statistique de Ljung-Box (prononcé Young) permet de tester l'hypothèse d'indépendance\n")
    cat(" sérielle d'une série (ou que la série est bruit blanc). Plus spécifiquement cette       \n")
    cat(" statistique teste l'hypothèse que les m coefficients d'autocorrélation sont nuls.       \n")
    cat("                                                                                         \n")
    cat(".........................................................................................\n") 
    cat("                 (H0) il n'y a pas auto-corrélation des erreurs d'ordre 1 à r.           \n") 
    cat("                 (H1) il y a auto-corrélation des erreurs d'ordre 1 à r.                 \n")
    cat(".........................................................................................\n") 
    cat(" Idéalement, nous aimerions ne pas rejeter l'hypothèse nulle.                            \n") 
    cat(" Autrement dit,                                                                          \n") 
    cat(" nous aimerions que la valeur p du test soit supérieure à 0,05 car                       \n") 
    cat(" cela signifie que les résidus de notre modèle de série chronologique sont indépendants, \n") 
    cat(" ce qui est souvent une hypothèse que nous faisons lors de la création d'un modèle.      \n") 
    cat(".........................................................................................\n")
    cat("  Changer la valeur du 'lag' puis cliquer sur 'Soumettre' pour exectuter un autre test   \n") 
    cat(".........................................................................................\n")
  }
  
  
  helpLjungBox <- function(){
    cat(".........................................................................................\n") 
    cat(" The Ljung (pronounced Young) Box test ( or just the Box test)                           \n")
    cat(" is a way to test for the absence of serial autocorrelation, up to a specified lag k.    \n")
    cat("                                                                                         \n")
    cat(" The test determines whether or not errors are iid (i.e. white noise) or whether there   \n")
    cat(" is something more behind them; whether or not the autocorrelations for the errors       \n")
    cat(" or residuals are non zero. Essentially, it is a test of lack of fit: if the             \n")
    cat(" autocorrelations of the residuals are very small, we say that the model doesn’t show    \n")
    cat(" ‘significant lack of fit’.                                                              \n")
    cat(".........................................................................................\n") 
    cat("   (H0) The residuals are independently distributed.                                     \n") 
    cat("        (i.e. the correlations in the population from which the sample is taken are 0,   \n") 
    cat("        so that any observed correlations in the data result from randomness             \n") 
    cat("        of the sampling process).                                                        \n") 
    cat("   (H1) The residuals are not independently distributed; they exhibit serial correlation.\n")
    cat(".........................................................................................\n") 
    cat(" Ideally, we would like to fail to reject the null hypothesis.                           \n")
    cat(" That is, we would like to see the p-value of the test be greater than 0.05 because      \n")
    cat(" this means the residuals for our time series model are independent,                     \n")
    cat(" which is often an assumption we make when creating a model.                             \n")
    cat(".........................................................................................\n")
    cat("  Change the 'lag' value then click 'Submit' to run another test                         \n") 
    cat(".........................................................................................\n")
  }
  
  
  helpADF <- function(){  
    cat(".......................................................................\n") 
    cat("               Augmented Dickey-Fuller Test                            \n")
    cat(".......................................................................\n") 
    cat("  Augmented Dickey-Fuller Test is used to check whether a given        \n")
    cat("  time series is at rest.                                              \n")
    cat("                                                                       \n")
    cat("  A given time series can be called stationary if:                     \n")     
    cat("  - it doesn’t have any trend                                          \n")     
    cat("  - depicts a constant variance over time                              \n")     
    cat("  - and follows autocorrelation structure over a period constantly.    \n")     
    cat(".......................................................................\n") 
    cat("  (H0)  The series has a unit root, so it is not stationary.           \n") 
    cat("  (Ha)  The series is stationary.                                      \n")
    cat(".......................................................................\n")
    cat("  p-value < 0.05 indicates the Time Series is stationary               \n")
    cat(".......................................................................\n")
  }
  
  helpADF2 <- function(){  
    cat(".............................................................\n") 
    cat("               Augmented Dickey-Fuller Test                  \n")
    cat(".............................................................\n") 
    cat("  (H0)  The series has a unit root, so it is not stationary. \n") 
    cat("  (Ha)  The series is stationary.                            \n")
    cat(".............................................................\n")
    cat("  p-value < 0.05 indicates the Time Series is stationary     \n")
    cat(".............................................................\n")
  }
  
  helpKPSS <- function(){  
    cat("....................................................................................................\n") 
    cat("                                                                                                    \n")
    cat("  One way to determine whether differencing is required is to use a unit root test.                 \n")
    cat("....................................................................................................\n") 
    cat("     Kwiatkowski-Phillips-Schmidt-Shin (KPSS)                                                       \n")
    cat("....................................................................................................\n") 
    cat("     A KPSS test can be used to determine if a time series is trend stationary.                     \n") 
    cat("                                                                                                    \n")
    cat("  (H0) : The time series is trend stationary. (the data is stationary)                              \n") 
    cat("  (Ha) : The time series is not trend stationary. (the data is not stationary)                      \n") 
    cat("                                                                                                    \n")
    cat("  If the p-value of the test is less than some significance level (e.g. α = .05) then we            \n") 
    cat("  reject the null hypothesis and conclude that the time series is not trend stationary.             \n") 
    cat("....................................................................................................\n") 
    cat("  A major disadvantage for the KPSS test is that it has a high rate of Type I errors                \n")
    cat("  (it tends to reject the null hypothesis too often).                                               \n")
    cat("  One way to deal with the potential for high Type I errors is to combine the KPSS with an ADF test.\n")
    cat("  If the result from both tests suggests that the time series in stationary, then it probably is.   \n")
    cat("....................................................................................................\n") 
    cat("                                                                                                    \n")
    
  }
  
  
  helpMK <- function(){  
    cat("....................................................................................................\n") 
    cat("  A Mann-Kendall trend test is used to determine whether or not there is a trend in the             \n") 
    cat("  time series data.                                                                                 \n") 
    cat("  It is a nonparametric test, which means that no underlying assumptions                            \n") 
    cat("  are made about the normality of the data.  It does require that there is no autocorrelation.      \n") 
    cat("                                                                                                    \n") 
    cat("  (H0) : There is no trend in the series                                                            \n") 
    cat("  (Ha) : There is a trend in the series                                                             \n") 
    cat("....................................................................................................\n") 
    cat("  Un test de tendance de Mann-Kendall est utilisé pour déterminer s'il existe ou non                \n") 
    cat("  une tendance dans les données de séries chronologiques.                                           \n") 
    cat("  Il s'agit d'un test non paramétrique, ce qui signifie qu'aucune hypothèse sous-jacente            \n") 
    cat("  n'est faite quant à la normalité des données.                                                     \n") 
    cat("                                                                                                    \n")
    cat("  (H0) : Il n'y a pas de tendance dans la série                                                     \n") 
    cat("  (Ha) : Il existe une tendance dans la série                                                       \n") 
    cat("....................................................................................................\n")
    cat("  p-value < 0.05 indicates the Time Series is not stationary, there is trend in the time series.    \n")
    cat("....................................................................................................\n") 
    cat("                                                                                                    \n")
    cat("     Mann-Kendall trend Test                                                                        \n")
    cat("                                                                                                    \n")
    
    
    }
  
  
  output$Plot_Type_Help <- renderPrint({
    cat(".......................................................................\n") 
    cat("                            Plot_Type_Help                             \n")
    cat(".......................................................................\n") 
    cat("                                                                       \n")
    cat("  Plots a time series along with its acf and either its                \n")
    cat("  pacf, histogram, lagged scatterplot or spectrum.                     \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat(".......................................................................\n")
    cat("                                                                       \n")
    cat(".......................................................................\n")
  })

  output$LatexCode <- renderPrint({
    cat(".......................................................................\n")
    cat("                            Latex code                                 \n")
    cat(".......................................................................\n")
    cat("                                                                       \n")
    cat("\\documentclass[12pt,landscape]{article}                               \n")
    cat("\\usepackage[a4paper]{geometry}                                        \n")
    cat("\\usepackage[utf8]{inputenc}                                           \n")
    cat("\\usepackage[T1]{fontenc}                                              \n")
    cat("                                                                       \n")
    cat("\\newcommand{\\operatorname}[1]{{#1}}                                  \n")
    cat("                                                                       \n")
    cat("\\begin{document}                                                      \n")
    cat("\\Huge                                                                 \n")
    cat("                                                                       \n")
    cat("%------------------------------------------------                      \n")
    cat("%   Formula goes between the dollar signs                             \n")
    cat("%------------------------------------------------                      \n")
    cat("                                                                       \n")
    cat("$                                                                      \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("$                                                                      \n")
    cat("                                                                       \n")
    cat("\\end{document}                                                        \n")
    cat("                                                                       \n")
    cat(".......................................................................\n")
    cat(" P.S.                                                                  \n")
    cat(" You can use mathjax to generate the formula at:                       \n")
    cat("   https://www.mathjax.org/#demo                                       \n")
    cat(" copy the formula and include a $ sign at the beginning and at the end \n")
    cat(".......................................................................\n")
  })

  
  
})
