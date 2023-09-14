HourlyDynamicModel <- function(df, identifier, ignoredDates = c(), settings, tz, plots=T){
  
  suppressMessages(suppressWarnings(library(ggplot2)))
  suppressMessages(suppressWarnings(library(plotly)))
  suppressMessages(suppressWarnings(library(carrier)))
  suppressMessages(suppressWarnings(library(mlflow)))
  suppressMessages(suppressWarnings(library(gridExtra)))
  
  modelName <- "HourlyDynamicModel"
  
  dir.create(settings$OutputDataDirectory,F)
  if(plots){
    dir.create(paste(settings$OutputDataDirectory,"plots",sep="/"),F)
  }
  
  # MLFlow configuration, fetch the experimentId and last registered model, if available.
  Sys.setenv(MLFLOW_PYTHON_BIN=settings$PYTHON3_BIN)
  Sys.setenv(MLFLOW_TRACKING_URI=settings$MLFlow$TrackingUri)
  Sys.setenv(MLFLOW_VERBOSE=FALSE)
  Sys.setenv(MLFLOW_BIN=settings$MLFlow$MLFLOW_BIN)
  experimentId <- tryCatch(
    mlflow_create_experiment(identifier),
    error = function(e){
      experiment <- mlflow_get_experiment(name=identifier)
      if (experiment$lifecycle_stage!="active") mlflow_restore_experiment(experiment$experiment_id)
      experiment$experiment_id
    }
  )
  if(settings$MLFlow$ForceModelTraining==F){
    lastRegisteredModel <- tryCatch(mlflow_get_registered_model(
      identifier)$latest_versions[[1]],
      error = function(e)NULL)
    lastRegisteredModel <- if(!is.null(lastRegisteredModel)){
      data.frame(
        "modelSubject"= lastRegisteredModel$run_link,
        "modelId"= gsub("runs:/|/model","", lastRegisteredModel$run_link),
        "modelName"= lastRegisteredModel$name,
        "time"=as.POSIXct(lastRegisteredModel$last_updated_timestamp/1000,
          format="%s",origin=as.POSIXct("1970-01-01 00:00:00",tz="UTC"),tz="UTC"),
        "description"= lastRegisteredModel$description)
    }
    if(is.null(lastRegisteredModel)){
      lastFittedModel <- NULL
    } else {
      lastFittedModel <- mlflow_load_model(lastRegisteredModel$modelSubject)
      write("",stderr())
    }
  }
  needsToBeTrained <- if(settings$MLFlow$ForceModelTraining || is.null(lastRegisteredModel)){
      TRUE
    } else {
      (now(tz="UTC") - lastRegisteredModel$time) >= 
        as.period(settings$MLFlow$ModelTrainingPeriodicity)
    }
  
  if(needsToBeTrained){
    
    ####
    # TRAIN AND PREDICT ----
    ####
    
    write("## Training process is starting",stderr())
    
    if(plots){
      ts_p <- ggplot( 
        reshape2::melt( 
          df %>% 
            select(time, Qe, temperature) %>% 
            suppressMessages(pad())
          ,"time") %>% mutate(
            variable = factor(variable,
                              levels=unique(variable),
                              labels=mapply(function(x){
                                if(x=="Qe"){ 'plain(consumption~(kWh))'
                                } else if(x=="temperature"){'plain(outdoor~temperature)~plain((degree*C))'
                                }},unique(variable)))
          )
      ) + 
        geom_line(aes(time,value)) +
        ylab("") +
        facet_wrap(~variable, scales = "free_y", ncol=1, labeller = label_parsed) +
        theme_bw() + theme(axis.text.x = element_text(hjust=1))
      ggsave(paste(settings$OutputDataDirectory,"plots","raw_consumption_temperature.pdf",sep="/"),
             ts_p, width=7, height=3.5)
    }
    
    ####
    # Calendar features and filtering of holidays and special periods ----
    ####
    
    # Detect holidays
    write("* Detecting the holidays",stderr())
    holidaysDates <- detect_holidays_in_tertiary_buildings(
      data = df, 
      consumptionColumn = "Qe", 
      timeColumn = "time",
      tz=tz,
      ignoreDates = ignoredDates,
      plotDensity = F)
    
    # Add the calendar components
    df <- df %>% calendar_components(
      localTimeZone = tz,
      holidays = holidaysDates,
      inplace=T
    )
    
    if(plots){
      h <- ggplot(
        df %>% select(time, Qe, isHolidays) %>% 
          group_by(date=as.Date(time,tz=tz)) %>% 
          summarise(Qe=sum(Qe),isHolidays=any(as.logical(isHolidays)))
      ) + 
        geom_line(aes(date,Qe),size=0.1,alpha=0.5) +
        geom_point(aes(date,Qe,col=isHolidays), size=0.1) +
        scale_color_manual("",values=c("TRUE"="red","FALSE"="black"),labels=c("TRUE"="Holiday","FALSE"="Not holiday")) +
        theme_bw() + theme(axis.text.x = element_text(hjust=1), legend.position = "top") +
        ylab("consumption (kWh)") + xlab("time")
      ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"holidays.pdf",sep="/"),
             h, width=7, height=3)
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
          daysThatAreOutliers = ignoredDates,
          logValueColumn = T,
          autoDetectProfiled = T),
        by = "localtime"
      )
    abnormalDays <- sort(df %>% 
      group_by(date) %>% summarise(out=sum(outliers)) %>%
      filter(out>0) %>% select(date) %>% unlist(.) %>% as.Date(.))
    df$abnormalDay <- df$date %in% abnormalDays
    
    if(plots){
      g <- ggplot(df %>% select(localtime, Qe, outliers) %>% 
                    group_by(date=as.Date(localtime,tz=tz)) %>% 
                    summarise(Qe=sum(Qe),outliers=any(as.logical(outliers))) ) +
        geom_line(aes(date,Qe),size=0.1,alpha=0.5) +
        theme_bw() + theme(axis.text.x = element_text(hjust=1), legend.position = "top") +
        ylab("consumption (kWh)") + xlab("time")
      if(!all(is.na(df$outliers) | df$outliers==F)){
        g <- g + geom_point(aes(date,Qe,col=outliers),size=0.1) + 
          scale_color_manual("",values=c("TRUE"="red","FALSE"="black"),labels=c("TRUE"="Day with outliers","FALSE"="Normal day"))
      }
      ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"outliers_plot.pdf",sep="/"),
             g, width=7, height=3)
    }
    
    ####
    # Clustering and classification of daily load curves ----
    ####
    
    write("* Detecting the most common daily load curves",stderr())
    if(is.null(settings$DailyLoadCurveClustering$maxTrainingMonths)){
      maxDateForClustering <- NULL
    } else {
      ht <- hourly_timesteps(720*settings$DailyLoadCurveClustering$maxTrainingMonths,detect_time_step(df$time))
      maxDateForClustering <- sort(df$date[df$outliers==F & !is.na(df$outliers)],)[
        if(ht > length(df$date[df$outliers==F & !is.na(df$outliers)])){
          length(df$date[df$outliers==F & !is.na(df$outliers)])
        } else {ht}
      ]
    }
    clust <- clustering_dlc(
      data = df,
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
        df %>% group_by(date) %>% summarise(outliers=sum(outliers)>0) %>% filter(
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
    
    if("s" %in% colnames(df))
      df <- df %>% select(-s)
    df <- df %>% left_join(clust$dailyClassification, by="date")
    
    classif <- classification_dlc(
      data = df, 
      consumptionFeature = "Qe",
      outdoorTemperatureFeature = "temperature", 
      localTimeZone = tz,
      holidaysDatesFeature = "holidaysDate",
      abnormalDaysFeature = "abnormalDay",
      clustering = clust,
      methodNormalDays = "classificationModel",
      methodAbnormalDays = "classificationModel"
    )
    df <- df %>% left_join(classif$dailyClassification,by="date")
    df$s <- ifelse(!is.na(df$s.x),df$s.x,df$s.y)
    df$s_origin <- ifelse(!is.na(df$s.x),"clustering","classification")
    df$s.x <- NULL
    df$s.y <- NULL
    # df <- df[!is.na(df$s),]
    
    if(plots){
      p <- ggplot(df) +
          geom_line(
            aes(hour, Qe, group=date, col=s_origin),
            alpha=0.1
          ) + scale_color_discrete(name="group daily\nload curves") +
        xlab(bquote("hour of the day")) + 
        ylab("consumption (kWh)")+
          facet_wrap(~s,ncol=2) +
          theme_bw()
      ggsave(filename = paste(settings$OutputDataDirectory,"plots/clustering_dlc.pdf",sep="/"), p,
             width=7,height=0.5+ceiling(length(unique(df$s))/2)*2.5)
      
      p_ts <- ggplot(df %>% group_by(date) %>% 
                       summarise("s"=first(s),
                                 "Qe"=sum(Qe),
                                 "s_origin"=first(s_origin))) + 
        geom_point(
          aes(date, Qe, col=s, shape=s_origin),
          alpha=0.5, size=2
        ) + 
        scale_color_discrete(name="group daily\nload curves") +
        scale_shape_discrete(name="estimated\nby") +
        xlab("time") + 
        ylab("consumption (kWh)") +
        theme_bw()
      ggsave(filename = paste(settings$OutputDataDirectory,"plots/clustering_dlc_ts.pdf",sep="/"), p_ts,
             width=7,height=3)
    }
    #df$s = "01"
    ###
    # Check weather dependence by group of daily load curves ----
    ###
    write("* Detecting if exists any weather dependence in energy consumption",stderr())
    if(plots){
      pdf(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                "clustering_dlc_wdep.pdf",sep="/"),6,6)
    }
    weatherDependenceByCluster <- get_change_point_temperature(
      consumptionData = df[ df$s_origin=="clustering",c("time","Qe","s")],#df$s==x &
      weatherData =  df[ df$s_origin=="clustering",c("time","temperature")],
      consumptionFeature = "Qe",
      temperatureFeature = "temperature",
      consumptionGroupFeature = "s",
      localTimeZone = tz,
      plot=plots
    )
    
    # weatherDependenceByCluster <- do.call(rbind,lapply(FUN = function(x){
    #   data.frame("s"=x,t(unlist(get_change_point_temperature(
    #     consumptionData = df[df$s==x & df$s_origin=="clustering",c("time","Qe")],#
    #     weatherData =  df[df$s==x & df$s_origin=="clustering",c("time","temperature")],
    #     consumptionFeature = "Qe",
    #     temperatureFeature = "temperature",
    #     localTimeZone = tz,
    #     plot=plots
    #   ))))
    # }, sort(unique(df$s))))
    if(plots){ dev.off() }
    wdep <- as.list(setNames(matrixStats::colMeans2(
      apply(as.data.frame(weatherDependenceByCluster) %>% select(-s),1:2,as.numeric),
      na.rm = T),c("tbalh","tbalc","heating","cooling")))
    df$wdeph <- factor(df$s, levels=sort(unique(df$s)))
    levels(df$wdeph) <- weatherDependenceByCluster$heating[order(levels(df$wdeph))]
    df$wdeph <- as.numeric(as.logical(as.character(df$wdeph)))
    df$wdepc <- factor(df$s, levels=sort(unique(df$s)))
    levels(df$wdepc) <- weatherDependenceByCluster$cooling[order(levels(df$wdepc))]
    df$wdepc <- as.numeric(as.logical(as.character(df$wdepc)))
    wdep$heating <- wdep$heating > 0
    wdep$cooling <- wdep$cooling > 0
    
    ###
    # Setting the model parameters and transformations to be done during the model training ----
    ###
    
    write("* Setting model parameters and transformations",stderr())
    generalParams <- list(
      "nhar"=list(
        "datatype"="integer",
        "nlevels"=4,
        "min"=6,
        "max"=10
      ),
      # "maxh"=list(
      #   "datatype"="float",
      #   "nlevels"=3,
      #   "min"=10,
      #   "max"=40
      # ),
      # "maxc"=list(
      #   "datatype"="float",
      #   "nlevels"=3,
      #   "min"=10,
      #   "max"=40
      # ),
      "alpha"=list(
        "datatype"="discrete",
        "levels"=c(0.6,0.8,0.9,0.95,0.975,0.985,0.99,0.995)
      ),
      "lambda"=list(
        "datatype"="discrete",
        "levels"=c(
          get_lpf_smoothing_time_scale(data.frame("time"=df$time),
            if(wdep$heating && wdep$cooling){
             6*31*24
            } else if (wdep$heating || wdep$cooling) {
             4*31*24
            } else {
             2*31*24
            })
        )
      )
    )
    generalTransformationSentences <- list(
      # Classify the daily load curves in case it was not predicted
      "s" = sprintf("
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
            localTimeZone = '%s',
            holidaysDatesFeature = 'holidaysDate',
            abnormalDaysFeature = 'abnormalDay',
            clustering = clusteringResults,
            methodNormalDays = 'classificationModel',
            methodAbnormalDays = 'classificationModel'
          )$sRaw,
          levels=rownames(clusteringResults$absoluteLoadCurvesCentroids),
          labels=rownames(clusteringResults$absoluteLoadCurvesCentroids))
        }",tz),
      
      # Avoid errors when there is no holidays detected
      "isHolidays" = "
        if(length(unique(as.character(isHolidays)))==1){
          rep(1,isHolidays)
        } else {
          isHolidays
        }",
      
      # Fourier series components of the hour of the day by weekdays and weekends. 
      "daily_seasonality" = c(
        "fs_components(...,featuresName='hour',nHarmonics=param$nhar,inplace=F)"
        ,"weekday"
      ),
      
      # Fourier series components of the hour of the day by weekdays and weekends. 
      "weekly_seasonality" = c(
        "fs_components(...,featuresName='weekdayNum',nHarmonics=2,inplace=F)"#,
        #"s"
      ),
      
      # Fourier series components of the hour of the day by weekdays and weekends. 
      "yearly_seasonality" = c(
        "fs_components(...,featuresName='monthInt',nHarmonics=2,inplace=F)"#,
        #"s"
      ),
      
      # Fill some gaps in the outdoor temperature time series.
      "temperature" = "na.locf(
                          na.locf(
                            na.approx(temperature,na.rm = F),
                            fromLast = T, na.rm = F
                          ),
                          na.rm=F)",
      
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
                                                 weatherDependenceByCluster$tbalh,18))
                          ))",
      "temperatureBalanceCooling" = "as.numeric(as.character(
                          factor(s,levels=weatherDependenceByCluster$s,
                                 labels = ifelse(is.finite(weatherDependenceByCluster$tbalc),
                                                 weatherDependenceByCluster$tbalc,24))
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
      # of the day and weekday/weekend
      "thint"=c("heatingLpfBool",
                "s"
      ),
      
      # Regression components for the cooling intercept depending on the hour 
      # of the day and weekday/weekend
      "tcint"=c("coolingLpfBool",
                "s"
      )
    )
    
    trControl <- trainControl(method="none")
    logOutput <- F
    
    ###
    # Model training ----
    ###
    
    write("* Training of the model",stderr())
    
    if(wdep$heating && wdep$cooling){
      write("   Model type: Heating and cooling",stderr())
      params <- generalParams[c("nhar","alpha","lambda")]#,"tbalc","tbalh"
      transformationSentences <- generalTransformationSentences[
        c("s","daily_seasonality","weekly_seasonality","yearly_seasonality",
          "temperature","tlpf","temperatureBalanceHeating","temperatureBalanceCooling",
          "onOffHeating","onOffCooling","heatingLpf","coolingLpf",
          "heatingLpf2","coolingLpf2","heatingLpfBool","coolingLpfBool",
          "th","tc","th2","tc2","tcint","thint")]
      minMonthsTraining <- 9
      HC_model <- function(params, df, ...) {
        args <- list(...)
        train(
          Qe ~ daily_seasonality + tc + th + tcint + thint,
          data=df,
          method = RLS(
            data.frame(parameter = names(params),
                       class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
          ),
          tuneGrid = expand.grid(params),
          trControl = trControl,
          maxPredictionValue = max(df[df$outliers==F & !is.na(df$outliers),"Qe"],na.rm=T) * 1.1,
          minPredictionValue = min(df[df$outliers==F & !is.na(df$outliers),"Qe"],na.rm=T),
          logOutput = logOutput,
          minMonthsTraining = minMonthsTraining,
          continuousTime = T,
          transformationSentences = args$transformationSentences,
          weatherDependenceByCluster = args$weatherDependenceByCluster,
          clusteringResults = args$clusteringResults
        )
      }
      best_params <- hyperparameters_tuning(
        opt_criteria = "minimise",
        opt_function = function(X, df, ...) {
          mod <- HC_model(X, df, ...)
          expected <- df$Qe
          obtained <- rentrack::predict.train(
            object = mod,
            newdata = df,
            forceGlobalInputFeatures = NULL,
            predictionHorizonInHours = 0,
            modelWindow = NULL,
            modelSelection = NULL
          )
          RMSE(expected, obtained, na.rm = T)
        },
        features = params,
        maxiter = 3,
        popSize = 8,
        df = df[df$outliers==F & !is.na(df$outliers),],
        parallel = F,
        transformationSentences = transformationSentences,
        weatherDependenceByCluster = weatherDependenceByCluster,
        clusteringResults = clust
      )
      mod <- HC_model(best_params, df[df$outliers==F & !is.na(df$outliers),],
                      transformationSentences = transformationSentences,
                      weatherDependenceByCluster = weatherDependenceByCluster,
                      clusteringResults = clust
      )
    } else if (wdep$heating){
      write("   Model type: Only heating",stderr())
      params <- generalParams[c("nhar","alpha","lambda")]#,"tbalh"
      transformationSentences <- generalTransformationSentences[
        c("s","daily_seasonality","weekly_seasonality","yearly_seasonality",
          "temperature","tlpf","temperatureBalanceHeating","onOffHeating",
          "heatingLpf","heatingLpf2",
          "heatingLpfBool","th","th2","thint")]
      minMonthsTraining <- 4
      H_model <- function(params, df, ...) {
        args <- list(...)
        train(
          Qe ~ daily_seasonality + th + thint,
          data = df,
          method = RLS(
            data.frame(parameter = names(params),
                       class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
          ),
          tuneGrid = expand.grid(params),
          trControl = trControl,
          maxPredictionValue = max(df[df$outliers==F & !is.na(df$outliers),"Qe"],na.rm=T) * 1.1,
          minPredictionValue = min(df[df$outliers==F & !is.na(df$outliers),"Qe"],na.rm=T),
          logOutput = logOutput,
          minMonthsTraining = minMonthsTraining,
          continuousTime = T,
          transformationSentences = args$transformationSentences,
          weatherDependenceByCluster = args$weatherDependenceByCluster,
          clusteringResults = args$clusteringResults
        )
      }
      best_params <- hyperparameters_tuning(
        opt_criteria = "minimise",
        opt_function = function(X, df, ...) {
          mod <- H_model(X, df, ...)
          expected <- df$Qe
          obtained <- rentrack::predict.train(
            object = mod,
            newdata = df,
            forceGlobalInputFeatures = NULL,
            predictionHorizonInHours = 0,
            modelWindow = NULL,
            modelSelection = NULL
          )
          RMSE(expected, obtained, na.rm = T)
        },
        features = params,
        maxiter = 3,
        popSize = 8,
        parallel = F,
        df = df[df$outliers==F & !is.na(df$outliers),],
        transformationSentences = transformationSentences,
        weatherDependenceByCluster = weatherDependenceByCluster,
        clusteringResults = clust
      )
      mod <- H_model(best_params, df[df$outliers==F & !is.na(df$outliers),],
                     transformationSentences = transformationSentences,
                     weatherDependenceByCluster = weatherDependenceByCluster,
                     clusteringResults = clust
      )
    } else if (wdep$cooling){
      write("   Model type: Only Cooling",stderr())
      params <- generalParams[c("nhar","alpha","lambda")]#,"tbalc"
      transformationSentences <- generalTransformationSentences[
        c("s","daily_seasonality","weekly_seasonality","yearly_seasonality",
          "temperature","tlpf","temperatureBalanceCooling","onOffCooling",
          "coolingLpf","coolingLpf2",
          "coolingLpfBool","tc","tc2","tcint")]
      minMonthsTraining <- 4
      C_model <- function(params, df, ...) {
        args <- list(...)
        train(
          Qe ~ daily_seasonality + tc + tcint,
          data = df,
          method = RLS(
            data.frame(parameter = names(params),
                       class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
          ),
          tuneGrid = expand.grid(params),
          trControl = trControl,
          maxPredictionValue = max(df[df$outliers==F & !is.na(df$outliers),"Qe"],na.rm=T) * 1.1,
          minPredictionValue = min(df[df$outliers==F & !is.na(df$outliers),"Qe"],na.rm=T),
          logOutput = logOutput,
          minMonthsTraining = minMonthsTraining,
          continuousTime = T,
          transformationSentences = args$transformationSentences,
          weatherDependenceByCluster = args$weatherDependenceByCluster,
          clusteringResults = args$clusteringResults
        )
      }
      best_params <- hyperparameters_tuning(
        opt_criteria = "minimise",
        opt_function = function(X, df, ...) {
          mod <- C_model(X, df, ...)
          expected <- df$Qe
          obtained <- rentrack::predict.train(
            object = mod,
            newdata = df,
            forceGlobalInputFeatures = NULL,
            predictionHorizonInHours = 0,
            modelWindow = NULL,
            modelSelection = NULL
          )
          RMSE(expected, obtained, na.rm = T)
        },
        features = params,
        maxiter = 3,
        popSize = 8,
        parallel = F,
        df = df[df$outliers==F & !is.na(df$outliers),],
        transformationSentences = transformationSentences,
        weatherDependenceByCluster = weatherDependenceByCluster,
        clusteringResults = clust
      )
      mod <- C_model(best_params, df[df$outliers==F & !is.na(df$outliers),],
                     transformationSentences = transformationSentences,
                     weatherDependenceByCluster = weatherDependenceByCluster,
                     clusteringResults = clust
      )
    } else {
      write("   Model type: Not weather dependence",stderr())
      params <- generalParams[c("nhar","lambda")]
      transformationSentences <- generalTransformationSentences[
        c("s","daily_seasonality","weekly_seasonality","yearly_seasonality")
      ]
      minMonthsTraining <- 1
      CAL_model <- function(params, df, ...) {
        args <- list(...)
        train(
          Qe ~ daily_seasonality,
          data=df,
          method = RLS(
            data.frame(parameter = names(params),
                       class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
          ),
          tuneGrid = expand.grid(params),
          trControl = trControl,
          maxPredictionValue = max(df[df$outliers==F & !is.na(df$outliers),"Qe"],na.rm=T) * 1.1,
          minPredictionValue = min(df[df$outliers==F & !is.na(df$outliers),"Qe"],na.rm=T),
          logOutput = logOutput,
          minMonthsTraining = minMonthsTraining,
          continuousTime = T,
          transformationSentences = args$transformationSentences,
          clusteringResults = args$clusteringResults
        )
      }
      best_params <- hyperparameters_tuning(
        opt_criteria = "minimise",
        opt_function = function(X, df, ...) {
          mod <- CAL_model(X, df, ...)
          expected <- df$Qe
          obtained <- rentrack::predict.train(
            object = mod,
            newdata = df,
            forceGlobalInputFeatures = NULL,
            predictionHorizonInHours = 0,
            modelWindow = NULL,
            modelSelection = NULL
          )
          RMSE(expected, obtained, na.rm = T)
        },
        features = params,
        maxiter = 3,
        popSize = 8,
        parallel = F,
        df = df[df$outliers==F & !is.na(df$outliers),],
        transformationSentences = transformationSentences,
        clusteringResults = clust
      )
      mod <- CAL_model(best_params, df[df$outliers==F & !is.na(df$outliers),],
                       transformationSentences = transformationSentences,
                       clusteringResults = clust
      )
    }
    
    write("* Loading process to MLFlow",stderr())
    
    with(mlflow_start_run(experiment_id = experimentId), {

      # Generate the predictor object
      predictor <- crate(function(x, forceGlobalInputFeatures = NULL,predictionHorizonInHours=0,
                                  modelWindow="%Y-%m-%d", modelSelection="rmse"){
        rentrack::predict.train(
          object = !!mod,
          newdata = x,
          forceGlobalInputFeatures = forceGlobalInputFeatures,
          predictionHorizonInHours = predictionHorizonInHours,
          modelWindow = modelWindow,
          modelSelection = modelSelection
        )
      })

      df_result <- df[df$time >= (min(df$time)+months(minMonthsTraining)),]
      df_result$predicted <- predictor(df_result)
      
      if(plots){
        # Actual vs. predicted
        p <- ggplot(df_result %>% pad(.,by = "time")) + 
          geom_line(aes(time,Qe), col="black") + 
          geom_line(aes(time,predicted),col="red",alpha=0.5) +
          ylim(c(0,max(df_result$predicted,na.rm=T)*1.1)) +
          theme_bw() + xlab("time") + ylab("consumption (kWh)")
        
        ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"validation.pdf",sep="/"), 
               p, width=7, height=3)
      }
      
      # Generate the MLflow instance
      mlflow_log_param("Model",modelName)
      
      # Store the params
      settings$FinalModel <- list("Parameters" = list())
      for (i in 1:ncol(mod$bestTune)){
        settings$FinalModel$Parameters[[colnames(mod$bestTune)[i]]] <-
          as.character(mod$bestTune[1,i])
        mlflow_log_param(colnames(mod$bestTune)[i],as.character(mod$bestTune[1,i]))
      }
      mlflow_log_param("MinTrainingDatetime",format_iso_8601(min(df$time)))
      settings$FinalModel$Parameters[["MinTrainingDatetime"]] <- format_iso_8601(min(df$time))
      mlflow_log_param("MaxTrainingDatetime",format_iso_8601(max(df$time)))
      settings$FinalModel$Parameters[["MaxTrainingDatetime"]] <- format_iso_8601(max(df$time))
      
      # Store the errors
      MAEmetric <- MAE(df_result$Qe[df_result$outliers==F],
                      df_result$predicted[df_result$outliers==F],na.rm = T)
      RMSEmetric <- RMSE(df_result$Qe[df_result$outliers==F],
           df_result$predicted[df_result$outliers==F],na.rm = T)
      CVRMSEmetric <- RMSEmetric / mean(df_result$Qe[df_result$outliers==F],na.rm=T)
      r2metric <- cor(df_result$Qe[df_result$outliers==F],
          df_result$predicted[df_result$outliers==F],
          use = "na.or.complete")^2
      settings$FinalModel$Metrics <- list(
        "MAE" = MAEmetric,
        "RMSE" = RMSEmetric,
        "CVRMSE" = CVRMSEmetric,
        "r2" = r2metric)
      mlflow_log_metric("MAE", MAEmetric)
      mlflow_log_metric("RMSE", RMSEmetric)
      mlflow_log_metric("CVRMSE", CVRMSEmetric)
      mlflow_log_metric("r2", r2metric)
      
      # Store the artifacts
      if(plots){
        mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                  "raw_consumption_temperature.pdf",sep="/"))
        mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                  "validation.pdf",sep="/"))
        mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                  "holidays.pdf",sep="/"))
        mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                  "outliers_plot.pdf",sep="/"))
        mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                  "clustering_dlc.pdf",sep="/"))
        mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                  "clustering_dlc_ts.pdf",sep="/"))
        mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                  "clustering_dlc_wdep.pdf",sep="/"))
      }
      
      # Store the model
      mlflow_log_model(predictor,"model")
      
      # Register the model
      tryCatch(mlflow_create_registered_model(identifier),error=function(e){})
      modelId <- mlflow_get_run()$run_uuid[1]
      modelSubject <- sprintf("runs:/%s/model",modelId)
      mlflow_create_model_version(
        name = identifier, 
        source = modelSubject,
        run_link = modelSubject,
        description= toJSON(settings[!(names(settings) %in% c("MongoConnection"))],auto_unbox = T)
      )
      mlflow_log_param("modelId",modelId)
      mlflow_log_param("modelSubject",modelSubject)
      mlflow_log_param("identifier",identifier)
      
      settings$FinalModel <- NULL
    })
    
  } else if (needsToBeTrained == F){
    
    ####
    # PREDICT ----
    ####
    
    write("## Reusing the last trained model",stderr())
    
    predictor <- lastFittedModel
    modelSubject <- lastRegisteredModel$modelSubject
    modelId <- lastRegisteredModel$modelId
    descriptionItems <- fromJSON(lastRegisteredModel$description)
    
    ####
    # Filter training datetime range in the new data to predict
    ####
    df <- df[df$time>(parse_iso_8601(descriptionItems$FinalModel$Parameters$MaxTrainingDatetime)-(15*24*3600)),]
    df <- df[!is.na(df$temperature),]
    
    df_result <-
      if(nrow(df)==0){
        ####
        # If there's no more data to predict
        ####
        df
      } else {
        ####
        # Calendar features and filtering of holidays and special periods ----
        ####
        
        # Detect holidays
        write("* Detecting the holidays",stderr())
        holidaysDates <- detect_holidays_in_tertiary_buildings(
          data = df, 
          consumptionColumn = "Qe", 
          timeColumn = "time",
          tz=tz,
          ignoreDates = ignoredDates,
          plotDensity = F)
        
        # Add the calendar components
        df <- df %>% calendar_components(
          localTimeZone = tz,
          holidays = holidaysDates,
          inplace=T
        )
        
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
              daysThatAreOutliers = ignoredDates,
              logValueColumn = T,
              autoDetectProfiled = T),
            by = "localtime"
          )
        abnormalDays <- sort(df %>% 
                               group_by(date) %>% summarise(out=sum(outliers)) %>%
                               filter(out>0) %>% select(date) %>% unlist(.) %>% as.Date(.))
        df$abnormalDay <- df$date %in% abnormalDays
        df
      }
  }
  
  if(nrow(df_result)==0){

    write("* There is no new data to predict, considering the indicators already estimated during model training",stderr())
    write("Success!",stderr())
    write("",stderr())
    
    return(NULL)
  
  } else {
    
    ####
    # CONSUMPTION PREDICTIONS - Total, baseload, heating and cooling ----
    ####
    # predictor <<- predictor
    # df_result <<- df_result
    disaggregated_df <- weather_dependence_disaggregator(
      predictor = predictor, 
      predictionHorizonInHours = settings$DynamicModelPredictionHorizonInHours,
      df = df_result, 
      forceNoCooling = list("coolingLpf2"=0, "coolingLpf"=0, "coolingLpfBool"=0),
      forceNoHeating = list("heatingLpf2"=0, "heatingLpf"=0, "heatingLpfBool"=0)
    )
    df_result <- df_result[,!(colnames(df_result) %in% colnames(disaggregated_df)[colnames(disaggregated_df)!="time"])] %>% 
      left_join(disaggregated_df,by="time")
    
    write("Success!",stderr())
    write("",stderr())
    
    return(
      list(
        data = df_result,
        subject = modelSubject,
        id = modelId,
        name = modelName
      )
    )
  }
}
