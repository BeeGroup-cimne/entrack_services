HourlyMeasurementAndVerificationModel <- function(df, buildingId, buildingSubject, identifier, settings, tz, eems, buildingGrossFloorArea, 
                                                  measuredProperty, plots=T, results=NULL){
  
  suppressMessages(suppressWarnings(library(ggplot2)))
  suppressMessages(suppressWarnings(library(htmlwidgets)))
  suppressMessages(suppressWarnings(library(plotly)))
  suppressMessages(suppressWarnings(library(carrier)))
  suppressMessages(suppressWarnings(library(mlflow)))
  
  modelName <- "HourlyMeasurementAndVerificationModel"
  
  dir.create(settings$OutputDataDirectory,F)
  if(plots){
    dir.create(paste(settings$OutputDataDirectory,"plots",sep="/"),F)
  }
  
  # MLFlow configuration, fetch the experimentId and last registered model, if available.
  Sys.setenv(MLFLOW_PYTHON_BIN=settings$PYTHON3_BIN)
  Sys.setenv(MLFLOW_TRACKING_URI=settings$MLFlow$TrackingUri)
  Sys.setenv(MLFLOW_VERBOSE=FALSE)
  Sys.setenv(MLFLOW_BIN=settings$MLFlow$MLFLOW_BIN)
  
  harmonised_results <- results
  
  ####
  # TRAIN AND PREDICT ----
  ####
  
  write("## Training process is starting",stderr())
  
  if(plots){
    ts_p <- ggplot(
      reshape2::melt( df %>% select(time, Qe, temperature, starts_with("eemProject")) %>% suppressMessages(pad()), 
                      "time")
    ) + 
      geom_line(
        aes(time,value)
      ) + 
      facet_wrap(~variable, scales = "free_y", ncol=1) +
      theme_bw()
    saveWidget(ggplotly(ts_p), 
               paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                     "raw_consumption_temperature.html",sep="/"),
               selfcontained = T)
  }
  
  ####
  # Calendar features and filtering of holidays and special periods ----
  ####
  
  # Detect the COVID influenced period if data is over COVID lockdown & return-to-normality period
  # with a minimum of one-year period.
  
  if( as.Date(quantile(df$time[is.finite(df$Qe)],0.8), tz=tz) > (
    ( as.Date(min(df$time[is.finite(df$Qe)]), tz=tz) +
      months(settings$DataCleaning$CheckForDisruption$minInitialMonths)
    )
  ) ||
  ( as.Date(quantile(df$time[is.finite(df$Qe)],0.8), tz=tz) >= 
    as.Date(settings$DataCleaning$CheckForDisruption$minIniDate)
  ) ||
  ( ( as.Date(min(df$time[is.finite(df$Qe)]), tz=tz) +
      months(settings$DataCleaning$CheckForDisruption$minInitialMonths) 
  ) <= as.Date(settings$DataCleaning$CheckForDisruption$maxEndDate) 
  )
  ) {
    
    write("* Detecting the time period with affectance by SARS-CoV2 lockdowns",stderr())
    
    minIniDate = max(min(as.Date(df$time[is.finite(df$Qe)], tz=tz)) +
                       months(settings$DataCleaning$CheckForDisruption$minInitialMonths) ,
                     as.Date(settings$DataCleaning$CheckForDisruption$minIniDate))
    maxIniDate = max(min(as.Date(df$time[is.finite(df$Qe)], tz=tz)) +
                       months(settings$DataCleaning$CheckForDisruption$minInitialMonths) ,
                     as.Date(settings$DataCleaning$CheckForDisruption$maxIniDate))
    minEndDate = max(maxIniDate,
                     min(as.Date(quantile(df$time[is.finite(df$Qe)],0.8), tz=tz),
                         as.Date(settings$DataCleaning$CheckForDisruption$minEndDate)))
    maxEndDate = max(minEndDate,
                     min(as.Date(quantile(df$time[is.finite(df$Qe)],0.8), tz=tz),
                         as.Date(settings$DataCleaning$CheckForDisruption$maxEndDate)))
    covid_affected_period <- detect_disruptive_period(
      data=df, consumptionColumn="Qe",
      temperatureColumn="temperature", timeColumn = "time", tz=tz,
      minIniDate = minIniDate, maxIniDate = maxIniDate,
      minEndDate = minEndDate, maxEndDate = maxEndDate, 
      checkFor=settings$DataCleaning$CheckForDisruption$checkFor,
      minDecrementPercentualAffectation = settings$DataCleaning$CheckForDisruption$minDecrementPercentualAffectation,
      minIncrementPercentualAffectation = settings$DataCleaning$CheckForDisruption$minIncrementPercentualAffectation)
    
    covid_affected_period <- tryCatch(
      seq.Date(covid_affected_period$minDate,covid_affected_period$maxDate,by = "days"),
      error = function(e)NULL)
  } else {
    covid_affected_period <- c()
  }
  
  # Detect holidays
  write("* Detecting the holidays",stderr())
  holidaysDates <- detect_holidays_in_tertiary_buildings(
    data = df, 
    consumptionColumn = "Qe", 
    timeColumn = "time",
    tz=tz,
    ignoreDates = covid_affected_period,
    plotDensity = F)
  
  # Add the calendar components
  df <- df %>% calendar_components(
    localTimeZone = tz,
    holidays = holidaysDates,
    inplace=T
  )
  
  if(plots){
    h <- ggplot(df[df$time>min(df$time[is.finite(df$Qe)],na.rm=T),]) + geom_line(aes(time,Qe)) +
      geom_point(data=df[df$isHolidays==T,],mapping = aes(time,Qe),col="red")
    saveWidget(ggplotly(h), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "holidays.html",sep="/"), 
               selfcontained = T)
  }
  
  ####
  # Outliers detection ----
  ####
  
  write("* Detecting the outliers",stderr())
  if(all(c("value","window") %in% colnames(df)))
    df <- df %>% select(-value, -window)
  df <- 
    df %>%
    select(!(contains("outliers") | contains("upperPredCalendarModel") | 
               contains("lowerPredCalendarModel"))) %>%
    left_join(
      detect_ts_calendar_model_outliers(
        data = .,
        localTimeColumn = "localtime",
        valueColumn = "Qe", 
        calendarFeatures = settings$DataCleaning$OutliersDetection$calendarFeatures$PT1H,
        mode = settings$DataCleaning$OutliersDetection$mode,
        upperModelPercentile = settings$DataCleaning$OutliersDetection$upperModelPercentile,
        lowerModelPercentile = settings$DataCleaning$OutliersDetection$lowerModelPercentile,
        upperPercentualThreshold = settings$DataCleaning$OutliersDetection$upperPercentualThreshold,
        lowerPercentualThreshold = settings$DataCleaning$OutliersDetection$lowerPercentualThreshold,
        window = settings$DataCleaning$OutliersDetection$window$PT1H,
        outputPredictors = T,
        holidaysCalendar = holidaysDates,
        daysThatAreOutliers = covid_affected_period,
        logValueColumn = F,
        autoDetectProfiled = T),
      by = "localtime"
    )
  abnormalDays <- sort(df %>% 
                         group_by(date) %>% summarise(out=sum(outliers)) %>%
                         filter(out>0) %>% select(date) %>% unlist(.) %>% as.Date(.))
  df$abnormalDay <- df$date %in% abnormalDays
  
  if(plots){
    g <- ggplot(df[,c("localtime","Qe","outliers","upperPredCalendarModel","lowerPredCalendarModel")]) +
      geom_line(aes(localtime,Qe)) +
      geom_ribbon(aes(localtime,ymax=upperPredCalendarModel,ymin=lowerPredCalendarModel),
                  col="blue",alpha=0.5)
    if(!all(is.na(df$outliers) | df$outliers==F)) g <- g + geom_point(aes(localtime,ifelse(outliers,Qe,NA)),col="yellow")
    saveWidget(ggplotly(g), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "outliers_plot.html", sep="/"), selfcontained = T)
  }
  
  for (eemProject in colnames(df)[startsWith(colnames(df),"eemProject")]){
    
    # eemProject="eemProject.1"
    
    identifier_case <- paste0(identifier,"~",eemProject)
    
    df_by_eem <- df[is.finite(df[,eemProject]),]
  
    ####
    # Clustering and classification of daily load curves ----
    ####
    
    write(sprintf("* [%s] Detecting the most common daily load curves",eemProject),stderr())
    maxDateForClustering <- as.Date(max(df_by_eem$time[df_by_eem[,eemProject]==0]))
    clusteringResults <- clustering_dlc(
      data = df_by_eem,
      consumptionFeature = "Qe", 
      outdoorTemperatureFeature = "temperature", 
      localTimeZone = tz,
      kMax = settings$DailyLoadCurveClustering$kMax,
      kMin = settings$DailyLoadCurveClustering$kMin,
      nNeighboursAffinity = settings$DailyLoadCurveClustering$nNeighboursAffinity,
      inputVars = settings$DailyLoadCurveClustering$inputVars,
      loadCurveTransformation = settings$DailyLoadCurveClustering$loadCurveTransformation,
      balanceOutdoorTemperatures = settings$DailyLoadCurveClustering$balanceOutdoorTemperatures,
      ignoreDates =
        df_by_eem %>% group_by(date) %>% summarise(outliers=sum(outliers)>0) %>% filter(
          if(is.null(maxDateForClustering)){
            outliers==T
          } else {
            outliers==T | (date >= maxDateForClustering)
          }) %>% select(date) %>% 
        unlist %>% as.Date,
      holidaysDates = holidaysDates,
      nDayParts = settings$DailyLoadCurveClustering$nDayParts,
      normalisationMethod = "range01"
    )
    
    if("s" %in% colnames(df_by_eem))
      df_by_eem <- df_by_eem %>% select(-s)
    df_by_eem <- df_by_eem %>% left_join(clusteringResults$dailyClassification, by="date")
    
    classif <- classification_dlc(
      data = df_by_eem, 
      consumptionFeature = "Qe",
      outdoorTemperatureFeature = "temperature", 
      localTimeZone = tz,
      holidaysDatesFeature = "holidaysDate",
      abnormalDaysFeature = "abnormalDay",
      clustering = clusteringResults,
      methodNormalDays = "clusteringCentroids",
      methodAbnormalDays = "classificationModel"
    )
    df_by_eem <- df_by_eem %>% left_join(classif$dailyClassification,by="date")
    df_by_eem$s <- ifelse(!is.na(df_by_eem$s.x),df_by_eem$s.x,df_by_eem$s.y)
    df_by_eem$s_origin <- ifelse(!is.na(df_by_eem$s.x),"clustering","classification")
    df_by_eem$s.x <- NULL
    df_by_eem$s.y <- NULL
    # df_by_eem <- df_by_eem[!is.na(df_by_eem$s),]
    
    # ggplot(df_by_eem[df_by_eem$s_origin=="clustering",] %>%
    #          group_by(date) %>%
    #          summarise(Qe=sum(Qe),
    #                    temperature=mean(temperature,na.rm=T),
    #                    s=first(s))) +
    #   geom_point(aes(temperature,Qe,col=s)) +
    #   xlab(bquote("temperature (ºC)")) + 
    #   ylab("consumption (kWh)")+ theme_bw()
    
    if(plots){
      p <- ggplot(df_by_eem[df_by_eem$s_origin=="clustering",] %>%
                    group_by(date) %>%
                    summarise(Qe=sum(Qe),
                              temperature=mean(temperature,na.rm=T),
                              s=first(s))) +
        geom_point(aes(temperature,Qe,col=s)) +
        xlab(bquote("temperature (ºC)")) + 
        ylab("consumption (kWh)")+ theme_bw()
      saveWidget(ggplotly(p), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                    "clustering_signature.html",sep="/"), 
                 selfcontained = T)
      p <- ggplot(df_by_eem) +
        geom_line(
          aes(hour, Qe, group=date, col=s_origin),
          alpha=0.1
        ) +
        xlab("hour of the day") +
        facet_wrap(~s) +
        theme_bw()
      saveWidget(ggplotly(p), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                    "clustering_dlc.html",sep="/"), 
                 selfcontained = T)
      p_ts <- ggplot(df_by_eem %>% group_by(date) %>% 
                       summarise("s"=first(s),
                                 "Qe"=sum(Qe),
                                 "s_origin"=first(s_origin))) + 
        geom_point(
          aes(date, Qe, col=s, shape=s_origin),
          alpha=0.7
        ) + 
        xlab("time") +
        theme_bw()
      saveWidget(ggplotly(p_ts), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "clustering_dlc_ts.html",sep="/"), 
                 selfcontained = T)
    }
    
    ###
    # Check weather dependence by group of daily load curves ----
    ###
    write(sprintf("* [%s] Detecting if exists any weather dependence in energy consumption",eemProject),stderr())
    if(plots){
      pdf(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                "clustering_dlc_wdep.pdf",sep="/"),4,3)
    }
    weatherDependenceByCluster <- get_change_point_temperature_v2(
      consumptionData = df_by_eem[ df_by_eem$s_origin=="clustering",c("time","Qe","s")],#df_by_eem$s==x &
      weatherData =  df_by_eem[ df_by_eem$s_origin=="clustering",c("time","temperature")],
      consumptionFeature = "Qe",
      temperatureFeature = "temperature",
      consumptionGroupFeature = "s",
      localTimeZone = tz,
      plot=plots
    )
    
    if(plots){ dev.off() }
    df_by_eem$wdeph <- factor(df_by_eem$s, levels=sort(unique(df_by_eem$s)))
    levels(df_by_eem$wdeph) <- weatherDependenceByCluster$heating[order(levels(df_by_eem$wdeph))]
    df_by_eem$wdeph <- as.numeric(as.logical(as.character(df_by_eem$wdeph)))
    df_by_eem$wdepc <- factor(df_by_eem$s, levels=sort(unique(df_by_eem$s)))
    levels(df_by_eem$wdepc) <- weatherDependenceByCluster$cooling[order(levels(df_by_eem$wdepc))]
    df_by_eem$wdepc <- as.numeric(as.logical(as.character(df_by_eem$wdepc)))
    
    
    ###
    # Setting the model parameters and transformations to be done during the model training ----
    ###
    
    write("* Setting model parameters and transformations",stderr())
    generalParams <- list(
      "nhar"=list(
        "datatype"="integer",
        "nlevels"=6,
        "min"=4,
        "max"=10
      ),
      "alpha"=list(
        "datatype"="discrete",
        "levels"=c(0.7,0.85,0.9,0.95,0.99)
      ),
      "tbalh_hysteresis"=list(
        "datatype"="discrete",
        "levels"=c(0,1,2,3)
      ),
      "tbalc_hysteresis"=list(
        "datatype"="discrete",
        "levels"=c(0,1,2,3)
      )
    )
    generalTransformationSentences <- list(
      # Classify the daily load curves in case it was not predicted
      "s" = "
        if(exists('s',mode = 'character')){
          factor(
            s,
            levels=rownames(clusteringResults$absoluteLoadCurvesCentroids),
            labels=rownames(clusteringResults$absoluteLoadCurvesCentroids)
          )
        } else {
          factor(classification_dlc(
            ..., 
            consumptionFeature = 'Qe',
            outdoorTemperatureFeature = 'temperature', 
            localTimeZone = tz,
            holidaysDatesFeature = 'holidaysDate',
            abnormalDaysFeature = 'abnormalDay',
            clustering = clusteringResults,
            methodNormalDays = 'classificationModel',
            methodAbnormalDays = 'classificationModel'
          )$sRaw,
          levels=rownames(clusteringResults$absoluteLoadCurvesCentroids),
          labels=rownames(clusteringResults$absoluteLoadCurvesCentroids))
        }",
      
      # Avoid errors when there is no holidays detected
      "isHolidays" = "
        if(length(unique(as.character(isHolidays)))==1){
          rep(1,isHolidays)
        } else {
          isHolidays
        }",
      
      # Fourier series components of the hour of the day by clustering seasonalities detected. 
      "daily_seasonality" = c(
        "fs_components(...,featuresName='hour',nHarmonics=param$nhar,inplace=F)",
        #"fs_components(...,featuresName='dayYear',nHarmonics=4,inplace=F)",
        "s"
      ),
      
      # Fourier series components of the hour of the day by weekdays and weekends. 
      "weekly_seasonality" = c(
        "fs_components(...,featuresName='weekdayNum',nHarmonics=2,inplace=F)"#,
        #"s"
      ),
      
      # Fourier series components of the hour of the day by weekdays and weekends. 
      "yearly_seasonality" = c(
        "month",#,
        "s"
      ),
      
      # Fill some gaps in the outdoor temperature time series.
      "temperature" = "na.locf(
                          na.locf(
                            na.approx(temperature,na.rm = F),
                            fromLast = T,na.rm = T
                          ),
                          na.rm=T)",
      
      # Low Pass Filtered (LPF) outdoor temperature
      "tlpf" = "lpf_ts(...,featuresNames='temperature',smoothingTimeScaleParameter=param$alpha,
                       inplace=F)",
      
      # Depending the cluster and the weather dependence analysis, define activations of the HVAC system
      "onOffHeating" = "as.numeric(as.character(
                          factor(s,levels=weatherDependenceByCluster$s,
                                 labels = weatherDependenceByCluster$heating)
                        ))",
      "onOffCooling" = "as.numeric(as.character(
                          factor(s,levels=weatherDependenceByCluster$s,
                                 labels = weatherDependenceByCluster$cooling)
                        ))",
      "temperatureBalanceHeating" = "as.numeric(as.character(
                          factor(s,levels=weatherDependenceByCluster$s,
                                 labels = ifelse(is.finite(weatherDependenceByCluster$tbalh),
                                                 weatherDependenceByCluster$tbalh-param$tbalh_hysteresis,18))
                          ))",
      "temperatureBalanceCooling" = "as.numeric(as.character(
                          factor(s,levels=weatherDependenceByCluster$s,
                                 labels = ifelse(is.finite(weatherDependenceByCluster$tbalc),
                                                 weatherDependenceByCluster$tbalc+param$tbalc_hysteresis,24))
                          ))",
      
      # Estimate the heating degrees based on a heating balance temperature 
      # and the LPF temperature series
      "heatingLpf" = "degree_raw(...,featuresName='tlpf',
                        baseTemperature=NULL,
                        baseTemperatureName='temperatureBalanceHeating',
                        mode='heating',outputFeaturesName='heatingLpf',
                        hysteresisBaseTemperature = 0,
                        inplace=F)",
      
      # Estimate the cooling degrees based on a cooling balance temperature 
      # and the LPF temperature series
      "coolingLpf" = "degree_raw(...,featuresName='tlpf',
                        baseTemperature=NULL,
                        baseTemperatureName='temperatureBalanceCooling',
                        mode='cooling',outputFeaturesName='coolingLpf',
                        hysteresisBaseTemperature = 0,
                        inplace=F)",
      
      # Squared versions of the heating and cooling degrees
      "heatingLpf2" = "heatingLpf^2",
      "coolingLpf2" = "coolingLpf^2",
      
      # Check if exists heating or cooling degrees at every timestep 
      "heatingLpfBool" = "ifelse(heatingLpf>0,1,0)",
      "coolingLpfBool" = "ifelse(coolingLpf>0,1,0)",
      
      # Regression components for the heating degrees depending on the hour of the day 
      #and weekday/weekend
      "th"=c("heatingLpf",
             "fs_components(...,featuresName='hour',
                            nHarmonics=ceiling(param$nhar),inplace=F)",
             "s"),
      
      # Regression components for the cooling degrees depending on the hour of the day 
      # and weekday/weekend
      "tc"=c("coolingLpf",
             "fs_components(...,featuresName='hour',
                            nHarmonics=ceiling(param$nhar),inplace=F)",
             "s"),
      
      # Regression components for the heating degrees depending on the hour of the day 
      #and weekday/weekend
      "th2"=c("heatingLpf2",
              "fs_components(...,featuresName='hour',
                             nHarmonics=ceiling(param$nhar),inplace=F)",
              "s"),
      
      # Regression components for the cooling degrees depending on the hour of the day 
      # and weekday/weekend
      "tc2"=c("coolingLpf2",
              "fs_components(...,featuresName='hour',
                             nHarmonics=ceiling(param$nhar),inplace=F)",
              "s"
      ),
      
      # Regression components for the heating intercept depending on the hour 
      # of the day
      "thint"=c("heatingLpfBool",
                "fs_components(...,featuresName='hour',
                             nHarmonics=ceiling(param$nhar),inplace=F)",
                "s"
      ),
      
      # Regression components for the cooling intercept depending on the hour 
      # of the day
      "tcint"=c("coolingLpfBool",
                "fs_components(...,featuresName='hour',
                             nHarmonics=ceiling(param$nhar),inplace=F)",
                "s"
      ),
      
      # Regression components for the heating intercept depending on the time of the year
      "thinty"=c("heatingLpfBool",
                "month",
                "s"
      ),
      
      # Regression components for the cooling intercept depending on the time of the year
      "tcinty"=c("coolingLpfBool",
                "month",
                "s"
      )
    )
    
    trControl <- trainControl(method="none")
    
    ###
    # Model training ----
    ###
    
    write("* Training of the model and loading process to MLFlow",stderr())
    
    model <- function(df, weatherDependenceByCluster, clusteringResults, generalParams, 
                      generalTransformationSentences, best_params=NULL){
      
      wdep <- as.list(setNames(matrixStats::colMeans2(
        apply(as.data.frame(weatherDependenceByCluster) %>% select(-s),1:2,as.numeric),
        na.rm = T),c("tbalh","tbalc","heating","cooling")))
      wdep$heating <- wdep$heating > 0
      wdep$cooling <- wdep$cooling > 0
      
      # Choose between heating and cooling model, heating model, cooling model or calendar model.
      if(wdep$heating && wdep$cooling){
        write("   Model type: Heating and cooling",stderr())
        params <- generalParams[c("nhar","alpha","tbalc_hysteresis","tbalh_hysteresis")]
        transformationSentences <- generalTransformationSentences[
          c("s","daily_seasonality","weekly_seasonality","yearly_seasonality",
            "temperature","tlpf","temperatureBalanceHeating","temperatureBalanceCooling",
            "onOffHeating","onOffCooling","heatingLpf","coolingLpf",
            "heatingLpf2","coolingLpf2","heatingLpfBool","coolingLpfBool",
            "th","tc","th2","tc2","tcint","thint","tcinty","thinty")]
        HC_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ daily_seasonality + yearly_seasonality + tc + th + tcinty + thinty ,
            data=df,
            method = GLM(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params),
            trControl = trControl,
            minPredictionValue = 0,
            maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
            familyGLM = quasipoisson(link="log"),
            transformationSentences = args$transformationSentences,
            weatherDependenceByCluster = args$weatherDependenceByCluster,
            clusteringResults = args$clusteringResults
          )
        }
        if(is.null(best_params)){
        best_params <- hyperparameters_tuning(
          opt_criteria = "minimise",
          opt_function = function(X, df, ...) {
            mod <- HC_model(X, df, ...)
            expected <- df$Qe
            obtained <- rentrack::predict.train(
              object = mod,
              newdata = df,
              forceGlobalInputFeatures = NULL
            )
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 2,
          popSize = 8,
          df = df,
          parallel = F,
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clusteringResults
        )}
        mod <- HC_model(best_params, df = df,
                        transformationSentences = transformationSentences,
                        weatherDependenceByCluster = weatherDependenceByCluster,
                        clusteringResults = clusteringResults
        )
        # plot((mod$finalModel$model$Qe),type="l");lines((mod$finalModel$fitted.values),col="red")
      } else if (wdep$heating){
        write("   Model type: Only heating",stderr())
        params <- generalParams[c("nhar","alpha","tbalh_hysteresis")]#,"tbalh"
        transformationSentences <- generalTransformationSentences[
          c("s","daily_seasonality","weekly_seasonality","yearly_seasonality",
            "temperature","tlpf","temperatureBalanceHeating","onOffHeating",
            "heatingLpf","heatingLpf2",
            "heatingLpfBool","th","th2","thint","thinty")]
        minMonthsTraining <- 4
        H_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ daily_seasonality + yearly_seasonality + th + thint + thinty,
            data = df,
            method = GLM(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params),
            trControl = trControl,
            minPredictionValue = 0,
            maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
            familyGLM = quasipoisson(link="log"),
            transformationSentences = args$transformationSentences,
            weatherDependenceByCluster = args$weatherDependenceByCluster,
            clusteringResults = args$clusteringResults
          )
        }
        if(is.null(best_params)){
        best_params <- hyperparameters_tuning(
          opt_criteria = "minimise",
          opt_function = function(X, df, ...) {
            mod <- H_model(X, df, ...)
            expected <- df$Qe
            obtained <- rentrack::predict.train(
              object = mod,
              newdata = df,
              forceGlobalInputFeatures = NULL
            )
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 2,
          popSize = 8,
          parallel = F,
          df = df,
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clusteringResults
        )}
        mod <- H_model(best_params, df = df,
                       transformationSentences = transformationSentences,
                       weatherDependenceByCluster = weatherDependenceByCluster,
                       clusteringResults = clusteringResults
        )
      } else if (wdep$cooling){
        write("   Model type: Only Cooling",stderr())
        params <- generalParams[c("nhar","alpha","tbalc_hysteresis")]#,"tbalc"
        transformationSentences <- generalTransformationSentences[
          c("s","daily_seasonality","weekly_seasonality","yearly_seasonality",
            "temperature","tlpf","temperatureBalanceCooling","onOffCooling",
            "coolingLpf","coolingLpf2",
            "coolingLpfBool","tc","tc2","tcint","tcinty")]
        minMonthsTraining <- 4
        C_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ daily_seasonality + yearly_seasonality + tc + tcint + tcinty,
            data = df,
            method = GLM(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params),
            trControl = trControl,
            minPredictionValue = 0,
            maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
            familyGLM = quasipoisson(link="log"),
            transformationSentences = args$transformationSentences,
            weatherDependenceByCluster = args$weatherDependenceByCluster,
            clusteringResults = args$clusteringResults
          )
        }
        if(is.null(best_params)){
        best_params <- hyperparameters_tuning(
          opt_criteria = "minimise",
          opt_function = function(X, df, ...) {
            mod <- C_model(X, df, ...)
            expected <- df$Qe
            obtained <- rentrack::predict.train(
              object = mod,
              newdata = df,
              forceGlobalInputFeatures = NULL
            )
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 2,
          popSize = 8,
          parallel = F,
          df = df,
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clusteringResults
        )}
        mod <- C_model(best_params, df = df,
                       transformationSentences = transformationSentences,
                       weatherDependenceByCluster = weatherDependenceByCluster,
                       clusteringResults = clusteringResults
        )
      } else {
        write("   Model type: Not weather dependence",stderr())
        params <- generalParams[c("nhar","alpha")]
        transformationSentences <- generalTransformationSentences[
          c("s","daily_seasonality","weekly_seasonality","yearly_seasonality")
        ]
        minMonthsTraining <- 1
        CAL_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ daily_seasonality + yearly_seasonality,
            data=df,
            method = GLM(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params),
            trControl = trControl,
            minPredictionValue = 0,
            maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
            familyGLM = quasipoisson(link="log"),
            transformationSentences = args$transformationSentences,
            clusteringResults = args$clusteringResults
          )
        }
        if(is.null(best_params)){
        best_params <- hyperparameters_tuning(
          opt_criteria = "minimise",
          opt_function = function(X, df, ...) {
            mod <- CAL_model(X, df, ...)
            expected <- df$Qe
            obtained <- rentrack::predict.train(
              object = mod,
              newdata = df,
              forceGlobalInputFeatures = NULL
            )
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 2,
          popSize = 8,
          parallel = F,
          df = df,
          transformationSentences = transformationSentences,
          clusteringResults = clusteringResults
        )}
        mod <- CAL_model(best_params, df = df,
                         transformationSentences = transformationSentences,
                         clusteringResults = clusteringResults
        )
      }
      return(list("mod"=mod,"best_params"=best_params))
    }
    
    baseline <- model(
      df = df_by_eem[df_by_eem$outliers==F & !is.na(df_by_eem$outliers) & df_by_eem[,eemProject]==0,],
      weatherDependenceByCluster = weatherDependenceByCluster,
      clusteringResults = clusteringResults,
      generalParams = generalParams,
      generalTransformationSentences = generalTransformationSentences)
    trained_models <- list(
      "baseline" = baseline$mod,
      "after_eem" = model(
        df = df_by_eem[df_by_eem$outliers==F & !is.na(df_by_eem$outliers) & df_by_eem[,eemProject]==1,],
        weatherDependenceByCluster = weatherDependenceByCluster,
        clusteringResults = clusteringResults,
        generalParams = generalParams,
        generalTransformationSentences = generalTransformationSentences, 
        best_params = baseline$best_params)$mod
    )
    
    # Generate the predictor objects
    predictor <- carrier::crate(function(x, baseline, forceGlobalInputFeatures = NULL, predictionIntervals = F){
      mod <- !!trained_models
      rentrack::predict.train(
        object = mod[[if(baseline){"baseline"}else{"after_eem"}]],
        newdata = x, 
        forceGlobalInputFeatures = forceGlobalInputFeatures,
        predictionIntervals = predictionIntervals
      )
    })
    
    counterfactual <- weather_dependence_disaggregator(
      predictor = predictor, 
      df = df_by_eem[df_by_eem[,eemProject] %in% c(0,1),], 
      forceNoCooling = list("coolingLpf2"=0, "coolingLpf"=0, "coolingLpfBool"=0),
      forceNoHeating = list("heatingLpf2"=0, "heatingLpf"=0, "heatingLpfBool"=0),
      baseline = T
    )
    after_eem <- weather_dependence_disaggregator(
      predictor = predictor, 
      df = df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),], 
      forceNoCooling = list("coolingLpf2"=0, "coolingLpf"=0, "coolingLpfBool"=0),
      forceNoHeating = list("heatingLpf2"=0, "heatingLpf"=0, "heatingLpfBool"=0),
      baseline = F
    )
    results <- after_eem %>% 
      left_join(counterfactual, by="time", suffix=c(".after_eem",".counterfactual")) %>%
      left_join(df_by_eem, by="time")
    results_ <- df_by_eem %>% 
      left_join(counterfactual, by="time", suffix=c("",".y"))
    
    ###
    ### Load to MLFlow if needed ----
    ###
    
    if(settings$MLFlow$StoreModels){
      
      experimentId <- tryCatch(
        mlflow::mlflow_create_experiment(identifier_case),
        error = function(e){
          experiment <- mlflow::mlflow_get_experiment(name=identifier_case)
          if (experiment$lifecycle_stage!="active") mlflow::mlflow_restore_experiment(experiment$experiment_id)
          experiment$experiment_id
        }
      )
      
      with(
        mlflow::mlflow_start_run(experiment_id = experimentId), {
           
           if(plots){
             # Actual vs. predicted
             p <- ggplotly(
               ggplot(counterfactual) + 
                 geom_line(aes(time,real), col="black") + 
                 geom_line(aes(time,predicted),col="red",alpha=0.5))
             saveWidget(p, paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                 "series_comparison_counterfactual.html",sep="/"), 
                        selfcontained = T)
             
             p <- ggplotly(
               ggplot(results) + 
                 geom_line(aes(time,Qe), col="black") + 
                 geom_line(aes(time,predicted.after_eem),col="red",alpha=0.5))
             saveWidget(p, paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                 "series_after_eem_project.html",sep="/"), 
                        selfcontained = T)
             
             # Residuals plot
             p <- ggplotly(
               ggplot(results) + 
                 geom_line(aes(time,Qe-predicted.after_eem), col="black") + 
                 ggtitle(paste0("Average: ",round(mean(results$Qe-results$predicted.after_eem,na.rm=T),1))))
             saveWidget(p, paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                 "residuals.html",sep="/"), 
                        selfcontained = T)
           }
           
           # Generate the MLflow instance
           mlflow::mlflow_log_param("Model",modelName)
           
           # Store the params
           settings$FinalModel <- list("Parameters" = list())
           for (i in 1:ncol(trained_models[["baseline"]]$bestTune)){
             settings$FinalModel$Parameters[[colnames(trained_models[["baseline"]]$bestTune)[i]]] <-
               as.character(trained_models[["baseline"]]$bestTune[1,i])
             mlflow::mlflow_log_param(paste0("Baseline - ",colnames(trained_models[["baseline"]]$bestTune)[i]),as.character(trained_models[["baseline"]]$bestTune[1,i]))
           }
           for (i in 1:ncol(trained_models[["after_eem"]]$bestTune)){
             settings$FinalModel$Parameters[[colnames(trained_models[["after_eem"]]$bestTune)[i]]] <-
               as.character(trained_models[["after_eem"]]$bestTune[1,i])
             mlflow::mlflow_log_param(paste0("AfterEEM - ",colnames(trained_models[["after_eem"]]$bestTune)[i]),as.character(trained_models[["after_eem"]]$bestTune[1,i]))
           }
           mlflow::mlflow_log_param("MinTrainingDatetime",parsedate::format_iso_8601(min(df_by_eem$time)))
           settings$FinalModel$Parameters[["MinTrainingDatetime"]] <- parsedate::format_iso_8601(min(df_by_eem$time))
           mlflow::mlflow_log_param("MaxTrainingDatetime",parsedate::format_iso_8601(max(df_by_eem$time)))
           settings$FinalModel$Parameters[["MaxTrainingDatetime"]] <- parsedate::format_iso_8601(max(df_by_eem$time))
           
           # Store the errors
           MAEmetric <- MAE(results_$Qe[results_[,eemProject]==0 & results_$outliers==F & !is.na(results_$outliers)],
                            results_$predicted[results_[,eemProject]==0 & results_$outliers==F & !is.na(results_$outliers)],na.rm = T)
           RMSEmetric <- RMSE(results_$Qe[results_[,eemProject]==0 & results_$outliers==F & !is.na(results_$outliers)],
                              results_$predicted[results_[,eemProject]==0 & results_$outliers==F & !is.na(results_$outliers)],na.rm = T)
           CVRMSEmetric <- RMSEmetric / mean(results_$Qe[results_[,eemProject]==0 & results_$outliers==F & !is.na(results_$outliers)],na.rm=T)
           r2metric <- cor(results_$Qe[results_[,eemProject]==0 & results_$outliers==F & !is.na(results_$outliers)],
                           results_$predicted[results_[,eemProject]==0 & results_$outliers==F & !is.na(results_$outliers)],
                           use = "na.or.complete")^2
           settings$FinalModel$Metrics <- list(
             "MAE" = MAEmetric,
             "RMSE" = RMSEmetric,
             "CVRMSE" = CVRMSEmetric,
             "r2" = r2metric)
           mlflow::mlflow_log_metric("MAE", MAEmetric)
           mlflow::mlflow_log_metric("RMSE", RMSEmetric)
           mlflow::mlflow_log_metric("CVRMSE", CVRMSEmetric)
           mlflow::mlflow_log_metric("r2", r2metric)
           
           # Store the artifacts
           if(plots){
             mlflow::mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "raw_consumption_temperature.html",sep="/"))
             mlflow::mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "series_after_eem_project.html",sep="/"))
             mlflow::mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "series_comparison_counterfactual.html",sep="/"))
             mlflow::mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "residuals.html",sep="/"))
             mlflow::mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "outliers_plot.html",sep="/"))
             mlflow::mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "clustering_signature.html",sep="/"))
             mlflow::mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "clustering_dlc.html",sep="/"))
             mlflow::mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "clustering_dlc_ts.html",sep="/"))
             mlflow::mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "clustering_dlc_wdep.pdf",sep="/"))
           }
           
           # Store the model
           mlflow::mlflow_log_model(predictor,"model")
           
           # Register the model
           tryCatch(mlflow::mlflow_create_registered_model(paste0(identifier_case,"~",modelName)),error=function(e){})
           modelId <- mlflow::mlflow_get_run()$run_uuid[1]
           modelSubject <- sprintf("runs:/%s/model",modelId)
           registeredModelName <- paste0(identifier_case,"~",modelName)
           mlflow::mlflow_create_model_version(
             name = registeredModelName, 
             source = modelSubject,
             run_link = modelSubject,
             description= toJSON(settings,auto_unbox = T)
           )
           mlflow::mlflow_log_param("modelId",modelId)
           mlflow::mlflow_log_param("modelSubject",modelSubject)
           mlflow::mlflow_log_param("registeredModelName",registeredModelName)
           
           settings$FinalModel <- NULL
        })
    } else {
      modelId <- NULL
      modelSubject <- NULL
    }

    # ggplotly(ggplot()+
    #            geom_line(aes(time, -predicted), data=counterfactual, col="black", alpha=0.3) +
    #            geom_line(aes(time, -real), data=after_eem, col="red", alpha=0.3) +
    #            geom_line(aes(time, baseload), data=counterfactual, col="grey", alpha=0.3) +
    #            geom_line(aes(time, baseloadR), data=after_eem, col="darkgrey", alpha=0.3) +
    #            geom_line(aes(time, heating), data=counterfactual, col="red", alpha=0.3) +
    #            geom_line(aes(time, heatingR), data=after_eem, col="darkred", alpha=0.3) +
    #            geom_line(aes(time, cooling), data=counterfactual, col="blue", alpha=0.3) +
    #            geom_line(aes(time, coolingR), data=after_eem, col="darkblue", alpha=0.3))
    # 
    # time_aggregate_results <- function(results, timeColumn, outputFrequency, funAggregation, estimate=F){
    #   times <- results[,timeColumn]
    #   results[,timeColumn] <- NULL
    #   results[,"time"] <- times
    #   cols <- colnames(results)[colnames(results) != "time"]
    #   results <- results %>% {
    #     if(is.null(outputFrequency)){
    #       group_by(.,
    #                time = first(time)
    #       )
    #     } else {
    #       group_by(.,
    #                time = lubridate::floor_date(time, lubridate::period(outputFrequency),
    #                                             week_start = getOption("lubridate.week.start", 1)))
    #     }} %>%
    #     summarise(
    #       across(cols, function(value){
    #         if(funAggregation=="SUM"){
    #           if(estimate){
    #             mean(value,na.rm=T)*length(value)
    #           } else {
    #             sum(value,na.rm=T)
    #           }
    #         } else if(funAggregation=="MEAN"){
    #           mean(value,na.rm=T)
    #         }
    #       })
    #     ) %>%
    #     ungroup()
    #   return(results)
    # }
    # results__ <- data.frame(
    #   "baseload" = results[,"baseloadR.after_eem"] - results[,"baseload.counterfactual"],
    #   "heating" = results[,"heatingR.after_eem"] - results[,"heating.counterfactual"],
    #   "cooling" = results[,"coolingR.after_eem"] - results[,"cooling.counterfactual"],
    #   "predicted" = results[,"real.after_eem"] - results[,"predicted.counterfactual"],
    #   "time" = results$time)
    # results__ <- time_aggregate_results(results__, timeColumn = "time",
    #                                     outputFrequency = "P1M", funAggregation = "SUM",
    #                                     estimate=T)
    # ggplotly(ggplot(reshape2::melt(results__[,c("baseload","heating","cooling","time")],"time")) +
    #            geom_col(aes(time, value, fill=variable), alpha=0.8,position = "stack") +
    #            scale_fill_manual(values = c("baseload" = "black", "heating" = "red", "cooling" = "blue")) +
    #            geom_line(aes(time, predicted), data = results__))
    
    measuredPropertyComponentsMapping = list(
      "counterfactual" = c(
        "Heating" = "heating",
        "Cooling" = "cooling",
        "Baseload" = "baseload",
        "Total" = "predicted"
      ),
      "after_eem" = c(
        "Heating" = "heatingR",
        "Cooling" = "coolingR",
        "Baseload" = "baseloadR",
        "Total" = "real"
      )
    )
    
    for (measuredPropertyComponent in c("Heating","Cooling","Baseload","Total")){
      if(!any(eems[eems$eemProjectId==strsplit(eemProject,"\\.")[[1]][2],
                  paste0("MeasuredPropertyComponent_",measuredPropertyComponent)])){
        next
      }
      harmonised_results <- generate_eem_assessment_indicators(
        data = results, 
        indicators = unlist(settings$Indicators[c("Energy","Cost","Emissions")]), 
        indicatorsNotAggregableByTime = if(measuredPropertyComponent=="Total"){settings$Indicators$NotAggregableByTime}else{NULL},
        measuredProperty = measuredProperty, 
        measuredPropertyComponent = measuredPropertyComponent, 
        frequencies = settings$Frequencies,
        buildingId = buildingId, 
        buildingSubject = buildingSubject, 
        timeColumn = "time", 
        localTimeZone = tz, 
        consumptionColumn = 
          paste0(measuredPropertyComponentsMapping[["after_eem"]][measuredPropertyComponent],".after_eem"),
        indicatorsUnitsSubjects = settings$IndicatorsUnitsSubjects,
        indicatorsTimeAggregationFunctions = settings$IndicatorsTimeAggregationFunctions,
        baselineConsumptionColumn = 
          paste0(measuredPropertyComponentsMapping[["counterfactual"]][measuredPropertyComponent],".counterfactual"), 
        buildingGrossFloorArea = buildingGrossFloorArea, 
        eemProjectDf = eems[eems$eemProjectId==strsplit(eemProject,"\\.")[[1]][2],],
        forceAssessmentSingleEEM = settings$EEMAssessmentConditions$ForceAssessmentBySingleEEM,
        carbonEmissionsColumn = "Qe_emissionsFactor", 
        energyPriceColumn = "Qe_price", 
        modelName = modelName, 
        modelId = modelId, 
        modelLocation = modelSubject, 
        modelStorageInfrastructureSubject = "ENTRACK:MLFlow", 
        estimateWhenAggregate = T, 
        prevResults = harmonised_results
      )
    }
  }
  
  write("* Estimation successful!",stderr())
  
  return(harmonised_results)
}
