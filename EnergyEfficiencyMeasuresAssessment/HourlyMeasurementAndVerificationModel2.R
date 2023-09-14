HourlyMeasurementAndVerificationModel <- function(df, buildingId, buildingSubject, identifier, settings, tz, eems, buildingGrossFloorArea, 
                                                  measuredProperty, ignoredDates, plots=T, results=list()){
  
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
      reshape2::melt( 
        df %>% 
          select(time, Qe, temperature, starts_with("eemProject")) %>% 
          suppressMessages(pad())
        ,"time") %>% mutate(
          variable = factor(variable,
            levels=unique(variable),
            labels=mapply(function(x){
              if(x=="Qe"){ 'plain(consumption~(kWh))'
              } else if(x=="temperature"){'plain(outdoor~temperature)~plain((degree*C))'
              } else {paste0("plain(",gsub("eemProject.","Energy~Efficiency~Measures~project~",x,fixed = T),
                             ")~(0==training~1==evaluation)")}},unique(variable)))
        )
      ) + 
      geom_line(aes(time,value)) +
      ylab("") +
      facet_wrap(~variable, scales = "free_y", ncol=1, labeller = label_parsed) +
      theme_bw() + theme(axis.text.x = element_text(hjust=1))
    ggsave(paste(settings$OutputDataDirectory,"plots","raw_consumption_temperature.pdf",sep="/"),
           ts_p, width=7, height=3.5+sum(colnames(df) %>% startsWith("eemProject")))
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
        logValueColumn = F,
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
  
  for (eemProject in colnames(df)[startsWith(colnames(df),"eemProject")]){
    
    # eemProject="eemProject.1"
    
    identifier_case <- paste0(identifier,"~",eemProject)
    df_by_eem <- df[is.finite(df[,eemProject]),]
    
    # Assess the EEM only when a minimum number of days with non-outlier data are available inside the 
    # min-max range of days after the EEM (all of them, defined in settings/EEMAssessmentConditions)
    if(sum(df_by_eem$outliers==F & !is.na(df_by_eem$outliers) & df_by_eem[,eemProject]==1) <= 
       hourly_timesteps(settings$EEMAssessmentConditions$MinDaysWithoutOutliers*24, detect_time_step(df_by_eem$time))) {
    
      write(sprintf(
        "* %s wasn't assessed because there are not enough valid days to make the estimation", eemProject) ,stderr())
    
    } else {
      
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
      if(length(unique(df$isHolidays))==1){
        generalTransformationSentences <- generalTransformationSentences["temperature"]
      }
      
      trControl <- trainControl(method="none")
      
      
      ###
      # Model training ----
      ###
      
      write("* Training of the model and loading process to MLFlow",stderr())
      
      
      model <- function(df, generalTransformationSentences){
        HC_model <- function(df, ...) {
          args <- list(...)
          if(length(unique(df$isHolidays))==1){
            train(
              Qe ~ hour + dayYear + temperature + weekdayNum,
              data=df,
              method = RandomForest(NULL),
              trControl = trControl,
              maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
              minPredictionValue = 0,
              transformationSentences = args$transformationSentences)
            } else {
              train(
                Qe ~ hour + dayYear + temperature + weekdayNum + holidays,
                data=df,
                method = RandomForest(NULL),
                trControl = trControl,
                maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
                minPredictionValue = 0,
                transformationSentences = args$transformationSentences)
            }
        }
        mod <- HC_model(df = df,
                        transformationSentences = generalTransformationSentences)
        return(mod)
      }
      
      trained_models <- list(
        "baseline" = model(
          df = df_by_eem[df_by_eem$outliers==F & !is.na(df_by_eem$outliers) & df_by_eem[,eemProject]==0,],
          generalTransformationSentences = generalTransformationSentences),
        "after_eem" = model(
          df = df_by_eem[df_by_eem$outliers==F & !is.na(df_by_eem$outliers) & df_by_eem[,eemProject]==1,],
          generalTransformationSentences = generalTransformationSentences)
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
      
      # Calculate the balanced temperature for each timestep 
      results_tbal <- compute_tbal_using_model_predictions(
        df = df_by_eem, predictor = predictor, 
        dep_vars = c("hour","weekdayNum","dayYear","isHolidays"), baseline=T)
      df_by_eem <- results_tbal$df
      
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
      
      df_by_eem$temperature <- na.locf(
        na.locf(
          na.approx(df_by_eem$temperature,na.rm = F),
          fromLast = T, na.rm = F
        ),
        na.rm=F)
      
      counterfactual <- weather_dependence_disaggregator(
        predictor = predictor, 
        df = df_by_eem[df_by_eem[,eemProject] %in% c(0,1),], 
        forceNoCooling = list("temperature"=ifelse(
          df_by_eem[df_by_eem[,eemProject] %in% c(0,1),"temperature"] < 
            df_by_eem[df_by_eem[,eemProject] %in% c(0,1),"tbal"], 
          df_by_eem[df_by_eem[,eemProject] %in% c(0,1),"temperature"],
          df_by_eem[df_by_eem[,eemProject] %in% c(0,1),"tbal"])),
        forceNoHeating = list("temperature"=ifelse(
          df_by_eem[df_by_eem[,eemProject] %in% c(0,1),"temperature"] >
            df_by_eem[df_by_eem[,eemProject] %in% c(0,1),"tbal"], 
          df_by_eem[df_by_eem[,eemProject] %in% c(0,1),"temperature"],
          df_by_eem[df_by_eem[,eemProject] %in% c(0,1),"tbal"])),
        forceNoCoolingAndHeating = list(
          "temperature"=df_by_eem[df_by_eem[,eemProject] %in% c(0,1),"tbal"]),
        baseline = T
      )
      after_eem <- weather_dependence_disaggregator(
        predictor = predictor, 
        df = df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),], 
        forceNoCooling = list("temperature"=ifelse(
          df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),"temperature"] < 
            df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),"tbal"], 
          df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),"temperature"],
          df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),"tbal"])),
        forceNoHeating = list("temperature"=ifelse(
          df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),"temperature"] >
            df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),"tbal"], 
          df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),"temperature"],
          df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),"tbal"])),
        forceNoCoolingAndHeating = list(
          "temperature"=df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),"tbal"]),
        baseline = F
      )
      
      # after_eem$time <- with_tz(after_eem$time,"UTC")
      # counterfactual$time <- with_tz(counterfactual$time,"UTC")
      # df_by_eem$time <- with_tz(df_by_eem$time,"UTC")
      results <- after_eem %>% 
        left_join(counterfactual, by="time", suffix=c(".after_eem",".counterfactual")) %>%
        left_join(df_by_eem, by="time")
      results_ <- df_by_eem %>% 
        left_join(counterfactual, by="time", suffix=c("",".y"))
      # after_eem$time <- with_tz(after_eem$time,tz)
      # counterfactual$time <- with_tz(counterfactual$time,tz)
      # df_by_eem$time <- with_tz(df_by_eem$time,tz)
      
      # Plot the energy savings
      if(plots){
        time_aggregate_results <- function(results, timeColumn, outputFrequency, funAggregation, estimate=F){
          times <- results[,timeColumn]
          results[,timeColumn] <- NULL
          results[,"time"] <- times
          cols <- colnames(results)[colnames(results) != "time"]
          results <- results %>% {
            if(is.null(outputFrequency)){
              group_by(.,
                       time = first(time)
              )
            } else {
              group_by(.,
                       time = lubridate::floor_date(time, lubridate::period(outputFrequency),
                                                    week_start = getOption("lubridate.week.start", 1)))
            }} %>%
            summarise(
              across(all_of(cols), function(value){
                if(funAggregation=="SUM"){
                  if(estimate){
                    mean(value,na.rm=T)*length(value)
                  } else {
                    sum(value,na.rm=T)
                  }
                } else if(funAggregation=="MEAN"){
                  mean(value,na.rm=T)
                }
              })
            ) %>%
            ungroup()
          return(results)
        }
        results__ <- data.frame(
          "baseload" = -results[,"baseloadR.after_eem"] + results[,"baseload.counterfactual"],
          "heating" = -results[,"heatingR.after_eem"] + results[,"heating.counterfactual"],
          "cooling" = -results[,"coolingR.after_eem"] + results[,"cooling.counterfactual"],
          "predicted" = -results[,"real.after_eem"] + results[,"predicted.counterfactual"],
          "time" = results$time)
        results__ <- time_aggregate_results(results__, timeColumn = "time",
                                            outputFrequency = "P1M", funAggregation = "SUM",
                                            estimate=T)
        g <- ggplot(reshape2::melt(results__[,c("baseload","heating","cooling","time")],"time")) +
                   geom_col(aes(time, value, fill=variable), alpha=0.3,position = "stack") +
                   scale_fill_manual("consumption\ncomponents", 
                                     values = c("baseload" = "black", "heating" = "darkred", "cooling" = "blue3")) +
                   labs(y=expression(atop(plain("monthly consumption savings (kWh)"),
                                          atop(plain("negative means more consumption versus counterfactual"))))) +
                   geom_line(data=results__, aes(time, predicted), col="black") +
                   geom_point(data=results__, aes(time, predicted), col="black") + 
                   geom_hline(aes(yintercept=0), size=0.3, linetype = "dashed") + 
                   theme_bw()
        ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"savings.pdf",sep="/"),
               g, width=7, height=4)
      }
      
      # Plot the predicted time series with both models
      if(plots){
        p <- ggplot(counterfactual %>% pad(.,by = "time")) + 
          geom_line(aes(time,real), col="black") + 
          geom_line(aes(time,predicted),col="red",alpha=0.5) +
          ylim(c(0,max(counterfactual$predicted,na.rm=T)*1.1)) +
          geom_area(data=
                      df %>% 
                      filter( time >= min(counterfactual$time) & time<=max(counterfactual$time) ) %>%
                      select(time, matches(eemProject)) %>%
                      rename(all_of(c(time="time",eem=eemProject))),
                    aes(time, eem * max(c(counterfactual$predicted),1.1, na.rm=T)),
                    col="blue",size=0.1,alpha=0.2) +
          theme_bw() + xlab("time") + ylab("consumption (kWh)")
        
        ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"counterfactual.pdf",sep="/"), 
               p, width=7, height=3)
        
        eemDate <- min((df %>% 
                          filter( time >= min(counterfactual$time) & time<=max(counterfactual$time) ) %>%
                          select(time, matches(eemProject)) %>%
                          rename(all_of(c(time="time",eem=eemProject))) %>% filter(eem==1))$time)
        p <-
          ggplot(reshape2::melt(
            results %>% select(time, real.after_eem, predicted.after_eem, predicted.counterfactual) %>% 
              pad(.,by="time"),"time")) +
            geom_line(aes(time,value,col=variable,alpha=variable),size=0.3) +
            geom_point(aes(eemDate,0),size=0.5) +
            geom_vline(aes(xintercept=eemDate),size=0.5)+ 
            geom_text(aes(eemDate,0),label=sprintf("%s is operating",gsub("eemProject.","EEM project ",eemProject)), 
                      check_overlap = TRUE, angle=90, hjust=-0.15, vjust=1.3, alpha=0.8) +
            scale_alpha_manual(values=c("real.after_eem"=0.4,"predicted.after_eem"=0.3, "predicted.counterfactual"=0.3),guide = "none") +
            scale_color_manual(
              name="type of\nconsumption",values=c("real.after_eem"="black","predicted.after_eem"="green", "predicted.counterfactual"="red"),
              labels=c("real.after_eem"="real\n","predicted.after_eem"="predicted by\nafter EEM model\n", 
                       "predicted.counterfactual"="predicted by\ncounterfactual model")) +
            theme_bw() + xlab("time") + ylab("consumption (kWh)")
        ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"validation.pdf",sep="/"),
               p, width=9, height=4)
      }
      
      
      ###
      ### Load to MLFlow if needed ----
      ###
      
      if(settings$MLFlow$StoreModels){
        
        experimentId <- tryCatch(
          mlflow_create_experiment(identifier_case),
          error = function(e){
            experiment <- mlflow_get_experiment(name=identifier_case)
            if (experiment$lifecycle_stage!="active") mlflow_restore_experiment(experiment$experiment_id)
            experiment$experiment_id
          }
        )
        
        with(
          mlflow_start_run(experiment_id = experimentId), {
            
            # Generate the MLflow instance
            mlflow_log_param("Model",modelName)
            
            # Store the params
            settings$FinalModel <- list("Parameters" = list())
            for (i in 1:ncol(trained_models[["baseline"]]$bestTune)){
              settings$FinalModel$Parameters[[colnames(trained_models[["baseline"]]$bestTune)[i]]] <-
                as.character(trained_models[["baseline"]]$bestTune[1,i])
              mlflow_log_param(paste0("Baseline - ",colnames(trained_models[["baseline"]]$bestTune)[i]),as.character(trained_models[["baseline"]]$bestTune[1,i]))
            }
            for (i in 1:ncol(trained_models[["after_eem"]]$bestTune)){
              settings$FinalModel$Parameters[[colnames(trained_models[["after_eem"]]$bestTune)[i]]] <-
                as.character(trained_models[["after_eem"]]$bestTune[1,i])
              mlflow_log_param(paste0("AfterEEM - ",colnames(trained_models[["after_eem"]]$bestTune)[i]),as.character(trained_models[["after_eem"]]$bestTune[1,i]))
            }
            mlflow_log_param("MinTrainingDatetime",parsedate::format_iso_8601(min(df_by_eem$time)))
            settings$FinalModel$Parameters[["MinTrainingDatetime"]] <- parsedate::format_iso_8601(min(df_by_eem$time))
            mlflow_log_param("MaxTrainingDatetime",parsedate::format_iso_8601(max(df_by_eem$time)))
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
                                                "counterfactual.pdf",sep="/"))
              mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                                "holidays.pdf",sep="/"))
              mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                                "outliers_plot.pdf",sep="/"))
              mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                                "energy_signatures.pdf",sep="/"))
              mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                                "savings.pdf",sep="/"))
            }
            
            # Store the model
            mlflow_log_model(predictor,"model")
            
            # Register the model
            tryCatch(mlflow_create_registered_model(paste0(identifier_case,"~",modelName)),error=function(e){})
            modelId <- mlflow_get_run()$run_uuid[1]
            modelSubject <- sprintf("runs:/%s/model",modelId)
            registeredModelName <- paste0(identifier_case,"~",modelName)
            mlflow_create_model_version(
              name = registeredModelName, 
              source = modelSubject,
              run_link = modelSubject,
              description= toJSON(settings,auto_unbox = T)
            )
            mlflow_log_param("modelId",modelId)
            mlflow_log_param("modelSubject",modelSubject)
            mlflow_log_param("registeredModelName",registeredModelName)
            
            settings$FinalModel <- NULL
          })
      } else {
        modelId <- NULL
        modelSubject <- NULL
      }
      
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
          indicatorsAggregatableByTime = 
            if(measuredProperty %in% settings$Indicators$AggregatableByTime$AllowedMeasuredProperties){
              settings$Indicators$AggregatableByTime$Name } else { NULL }, 
          indicatorsNonAggregatableByTime = 
            if(measuredPropertyComponent=="Total" &&
               measuredProperty %in% settings$Indicators$NonAggregatableByTime$AllowedMeasuredProperties){
              settings$Indicators$NonAggregatableByTime$Name } else { NULL },
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
  }
  
  write("* Estimation finalised!",stderr())
  
  return(harmonised_results)
}

