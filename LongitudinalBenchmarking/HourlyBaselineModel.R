HourlyBaselineModel <- function(df, identifier, ignoredDates=c(), settings, tz, plots=T){
  
  suppressMessages(suppressWarnings(library(ggplot2)))
  suppressMessages(suppressWarnings(library(plotly)))
  suppressMessages(suppressWarnings(library(carrier)))
  suppressMessages(suppressWarnings(library(mlflow)))
  
  modelName <- "HourlyBaselineModel"
  
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
    
    ###
    # Setting the model parameters and transformations to be done during the model training ----
    ###
    
    write("* Setting model parameters and transformations",stderr())
    generalTransformationSentences <- list(
      # Cast to numeric of the holidays feature
      "holidays" = "ifelse(isHolidays==T,2,1)",
      
      # Fill some gaps in the outdoor temperature time series.
      "temperature" = "na.locf(
                          na.locf(
                            na.approx(temperature,na.rm = F),
                            fromLast = T, na.rm = F
                          ),
                          na.rm=F)"
    )
    
    trControl <- trainControl(method="none")
    
    ###
    # Model training ----
    ###
    
    write("* Training of the model",stderr())
    
    HC_model <- function(df, ...) {
      args <- list(...)
      train(
        Qe ~ hour + dayYear + temperature + weekdayNum + holidays,
        data=df,
        method = RandomForest(NULL),
        trControl = trControl,
        maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
        minPredictionValue = 0,
        transformationSentences = args$transformationSentences
      )
    }
    mod <- HC_model(df = df[df$outliers==F & !is.na(df$outliers) & df$year %in% settings$BaselineYears,],
                    transformationSentences = generalTransformationSentences)
    
    # Generate the predictor object
    predictor <- crate(function(x, forceGlobalInputFeatures = NULL){
      rentrack::predict.train(
        object = !!mod,
        newdata = x,
        forceGlobalInputFeatures = forceGlobalInputFeatures
      )
    })
    
    df_result <- df
    df_result$predicted <- predictor(df_result)
    
    results_tbal <- compute_tbal_using_model_predictions(df_result, predictor, 
      dep_vars = c("hour","weekdayNum","dayYear","isHolidays"),simplify = T)
    df_result <- results_tbal$df
    
    if(plots){
      results_tbal$casesPredicted$weekdayNum <- 
        factor(results_tbal$casesPredicted$weekdayNum,
               levels=1:7,labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
      results_tbal$casesPredicted$isHolidays <- 
        factor(results_tbal$casesPredicted$isHolidays,
               levels=c(T,F),labels=c("Holiday","-"))
      results_tbal$casesPredicted$hour <- 
        factor(results_tbal$casesPredicted$hour,
               levels=0:23,labels=sprintf("%02i:00",c(0:23)))
      gs <- ggplot(results_tbal$casesPredicted[order(results_tbal$casesPredicted$dayYear),]) +
        geom_line(aes(temperature,value,group=case,col=dayYear),alpha=0.5) +
        scale_color_gradientn("day of\nthe year",
                              colours = c("red4","lightpink","lightblue","darkblue","lightblue","lightpink","red4")) +
        facet_grid(weekdayNum+isHolidays~hour) +
        ylab("consumption (kWh)") + xlab(bquote("outdoor temperature ("*degree*"C)")) + 
        theme_bw()
      ggsave(
        paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"energy_signatures.pdf", sep="/"),
        gs, width=18, height = 12
      )
    }
    
    write("* Loading process to MLFlow",stderr())
    
    with(mlflow_start_run(experiment_id = experimentId), {
      
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
      mlflow_log_param("BaselineYears",paste(settings$BaselineYears,collapse=","))
      
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
                                          "energy_signatures.pdf",sep="/"))
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
    
    if(!("tbal" %in% colnames(df_result))){
      df_result <- compute_tbal_using_model_predictions(df_result, predictor, dep_vars = c("hour","weekdayNum","dayYear","isHolidays"),simplify = T)$df
    }
    df_result$temperature <- na.locf(
      na.locf(
        na.approx(df_result$temperature,na.rm = F),
        fromLast = T, na.rm = F
      ),
      na.rm=F)
      
    disaggregated_df <- weather_dependence_disaggregator(
      predictor = predictor,
      df = df_result,
      forceNoCooling = list("temperature"=ifelse(df_result$temperature < df_result$tbal, 
                                                 df_result$temperature, df_result$tbal)),
      forceNoHeating = list("temperature"=ifelse(df_result$temperature > df_result$tbal, 
                                                 df_result$temperature, df_result$tbal)),
      forceNoCoolingAndHeating = list("temperature"=df_result$tbal)
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
