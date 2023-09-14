longitudinal_benchmarking<- function(buildingSubject, buildingsRdf, timeseriesObject, settings, libraryPath=".", updateHadoopStatus=F){
  
  suppressMessages(suppressWarnings(library(rentrack)))
  
  source(paste(libraryPath,"HourlyDynamicModel.R", sep="/"))
  source(paste(libraryPath,"HourlyBaselineModel.R", sep="/"))
  source(paste(libraryPath,"MonthlyDynamicModel.R", sep="/"))
  source(paste(libraryPath,"MonthlyBaselineModel.R", sep="/"))
  source(paste(libraryPath,"YearlyDynamicModel.R", sep="/"))
  
  dir.create(settings$OutputDataDirectory,showWarnings = F)

  buildingId <- get_building_identifiers(buildingSubject)
  write("",stderr())
  write("###################################",stderr())
  write("#### Longitudinal benchmarking ####",stderr())
  write("###################################",stderr())
  write(sprintf("   Building: %s", buildingSubject),stderr())
  write("",stderr())

  tz <- get_tz_building(buildingsRdf, buildingSubject)
  buildingArea <- get_area_building(buildingsRdf, buildingSubject)
    
  building_results <- list()

  for (j in 1:length(settings$MeasuredPropertiesDeviceAggregators)){
      #j=1
    tryCatch({
      deviceAggregator <- names(settings$MeasuredPropertiesDeviceAggregators)[[j]]
      write(sprintf("* Device Aggregator name: %s", deviceAggregator),stderr())
      measuredPropertyOutput <- settings$MeasuredPropertiesDeviceAggregators[[j]]
      measuredPropertiesInput <- c(settings$MeasuredPropertiesDeviceAggregators[[j]],"Temperature")

      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: RUNNING %s %s", buildingSubject, measuredPropertyOutput), stderr())
      }

      buildingData <- get_device_aggregators(
        buildingsRdf,
        timeseriesObject, #If character, data directory is provided;
        #if list of dataframes, the timeseries are provided.
        allowedBuildingSubjects=buildingSubject,
        allowedMeasuredProperties=measuredPropertiesInput,
        useEstimatedValues=F, ratioCorrection=F)

      # If the measured property is not available in buildingData, deprecate this case
      if(!any(mapply(function(x) regexpr(measuredPropertyOutput,x),
                     colnames(buildingData[[buildingSubject]]$df))>1)) {
        write("* No data related with the measuredProperty",stderr())
        write(
          paste(
            unique(
              paste(
                buildingData[[buildingSubject]]$metadata$deviceAggregatorName,
                buildingData[[buildingSubject]]$metadata$deviceAggregatorFormula,
                sep=": "
              )
            ),
            collapse=", "
          ),
          stderr()
        )
        next
      }

      write("",stderr())
      write("* Reading and aligning the input data",stderr())
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: READING DATA %s %s", buildingSubject, measuredPropertyOutput), stderr())
      }

      df <- data.frame(
        "time" = buildingData[[buildingSubject]]$df$time,
        "temperature" = buildingData[[buildingSubject]]$df[,paste0("outdoorTemperature",".AVG_Temperature")],
        "Qe" = buildingData[[buildingSubject]]$df[,paste0(deviceAggregator,".SUM_",measuredPropertyOutput)],
        "Qe_cost" = buildingData[[buildingSubject]]$df[,paste0(deviceAggregator,".SUM_EnergyCost")],
        "Qe_emissions" = buildingData[[buildingSubject]]$df[,paste0(deviceAggregator,".SUM_EnergyEmissions")],
        "Qe_price" = buildingData[[buildingSubject]]$df[,paste0(deviceAggregator,".AVG_",measuredPropertyOutput,"_EnergyPrice")],
        "Qe_emissionsFactor" = buildingData[[buildingSubject]]$df[,paste0(deviceAggregator,".AVG_",measuredPropertyOutput,"_EnergyEmissionsFactor")]
      )

      df <- df[min(which(is.finite(df$Qe))):(nrow(df)+1-min(which(is.finite(rev(df$Qe))))),]
      
      
      # Detect the SARS-CoV2 lockdown influenced period, and ignore those days in the training phase of models
      # Default period
      covid_affected_period <- seq.Date(
        as.Date(settings$DataCleaning$CheckForDisruption$defaultPeriod$minDate,tz=tz),
        as.Date(settings$DataCleaning$CheckForDisruption$defaultPeriod$maxDate,tz=tz),
        by="days"
      )
      if(as.period(detect_time_step(df$time))<=as.period("P1D") &&
         # Detect it using the time series data when frequency is greater or equal than daily (e.g. daily, hourly, quarterhourly...)
         ( as.Date(quantile(df$time[is.finite(df$Qe)],0.8), tz=tz) > (
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
         ) ) {
        
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
        covid_affected_period_inferred <- detect_disruptive_period(
          data=df, consumptionColumn="Qe",
          temperatureColumn="temperature", timeColumn = "time", tz=tz,
          minIniDate = minIniDate, maxIniDate = maxIniDate,
          minEndDate = minEndDate, maxEndDate = maxEndDate, 
          checkFor=settings$DataCleaning$CheckForDisruption$checkFor,
          minDecrementPercentualAffectation = settings$DataCleaning$CheckForDisruption$minDecrementPercentualAffectation,
          minIncrementPercentualAffectation = settings$DataCleaning$CheckForDisruption$minIncrementPercentualAffectation)
        
        covid_affected_period_inferred <- tryCatch(
          seq.Date(covid_affected_period$minDate,covid_affected_period$maxDate,by = "days"),
          error = function(e)NULL)
      }
      if(length(covid_affected_period_inferred)>0){covid_affected_period <- covid_affected_period_inferred}
      
      # Detect the type of models to test
      datePeriod <- as.duration(as.Date(max(df$time[is.finite(df$Qe)]), tz=tz) - as.Date(min(df$time[is.finite(df$Qe)]), tz=tz))
      datePeriodInBaselineYear <- tryCatch({
        sum(
          (year(df$time) %in% settings$BaselineYears) *
            hourly_timesteps(as.numeric(as.duration(detect_time_step(df$time)))/3600,"P1Y"),
          na.rm=T
        ) >=
          (8760 * settings$MinRatioBaseline * length(settings$BaselineYears))
      },error=function(e){FALSE})
      
      write("* Launcher modes: ",stderr())

      launcherModes <-
        # Less than one year of data
        if(datePeriod < as.duration("P1Y")){
          c("NoModel")
          # Between one year and three years of data
        } else if (datePeriod < as.duration("P3Y") && datePeriodInBaselineYear){
          if(as.period(detect_time_step(df$time)) <= as.period("PT1H") ){
            c("HourlyBaselineModel")
          } else if (as.period(detect_time_step(df$time)) <= as.period("P1M") ){
            c("MonthlyBaselineModel")
          } else {
            c("NoModel")
          }
          # Yearly data frequency and more than 6 years
        } else if ( datePeriod >= as.duration("P6Y") &&
                    as.period(detect_time_step(df$time)) >= as.period("P1Y") ){
          c("YearlyDynamicModel")
          # More than three years of data and complete baseline year (> min ratio in settings %)
        } else if (datePeriodInBaselineYear){
          if(as.period(detect_time_step(df$time)) <= as.period("PT1H") ){
            c("HourlyBaselineModel", "HourlyDynamicModel")
          } else if (as.period(detect_time_step(df$time)) <= as.period("P1M") ){
            c("MonthlyBaselineModel", "MonthlyDynamicModel")
          } else {
            c("NoModel")
          }
          # Between one year and three years of data, no baseline year available, and monthly data frequency to beyond.
        } else {
          if(as.period(detect_time_step(df$time)) <= as.period("PT1H") ){
            c("HourlyDynamicModel")
          } else if (as.period(detect_time_step(df$time)) <= as.period("P1M") ){
            c("MonthlyDynamicModel")
          } else {
            c("NoModel")
          }
        }

      write(paste0("    ",paste(launcherModes,collapse=", ")),stderr())

      ####
      # Generate the indicators ----
      ####

      write("* Generating the real indicators",stderr())

      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: GENERATING REAL INDICATORS %s %s", buildingSubject, measuredPropertyOutput), stderr())
      }

      ## Indicators based on real data
      results <- generate_longitudinal_benchmarking_indicators(
        data = df,
        indicators = unlist(settings$PerformanceIndicators),
        indicatorsUnitsSubjects = settings$IndicatorsUnitsSubjects,
        measuredPropertyComponent = "Total",
        measuredProperty = measuredPropertyOutput,
        frequencies = settings$Frequencies[
          as.period(settings$Frequencies)>=as.period(detect_time_step(df$time))],
        buildingId = buildingId,
        buildingSubject = buildingSubject,
        localTimeZone = tz,
        timeColumn = "time",
        consumptionColumn = "Qe",
        energyPriceColumn = "Qe_price",
        carbonEmissionsColumn = "Qe_emissionsFactor",
        buildingGrossFloorArea = buildingArea,
        outdoorTemperatureColumn = "temperature",
        heatingDegreeDays18Column = if(sum(colnames(df) %in% c("HDD18"))>0) {
          colnames(df)[colnames(df) %in% c("HDD18")]
        } else {NULL},
        coolingDegreeDays21Column = if(sum(colnames(df) %in% c("CDD21"))>0) {
          colnames(df)[colnames(df) %in% c("CDD21")]
        } else {NULL},
        # carbonEmissionsColumn = ,
        # energyPriceColumn = ,
        # dailyFixedPriceColumn =,
        modelName = NULL,
        modelId = NULL,
        modelLocation = NULL,
        modelTypeSubject = NULL,
        modelStorageInfrastructureSubject = NULL,
        estimateWhenAggregate = T,
        minRatioToTimeAggregateIndicators = settings$MinRatioToTimeAggregateIndicators,
        prevResults = NULL
      )

      # Indicators based on models
      for(modelName in launcherModes[(launcherModes %in% c("HourlyDynamicModel","HourlyBaselineModel"))]){
        #modelName = "HourlyDynamicModel"
        modelType <- if(grepl("DynamicModel",modelName)){"DynamicModel"} else {"BaselineModel"}
        write(sprintf("* Generating the estimated indicators based on %s",modelName),stderr())
        if(updateHadoopStatus==T){
            write(sprintf("reporter:status: GENERATING ESTIMATED INDICATORS %s %s", buildingSubject, measuredPropertyOutput), stderr())
        }

        # Evaluate the model, if needed, and predict the indicators
        call <- paste0(
          modelName,
          paste0("(df=df, identifier=paste(buildingId,deviceAggregator,modelName,sep='~'),
                   ignoredDates = covid_affected_period,
                   settings=settings, tz=tz, plots = T)")
        )
        model <- eval(parse(text=call))
        
        if(!is.null(model)){
          energyComponentsColumns <- c(
            "Total"="Qe",
            "Baseload"="baseloadR",
            "Heating"="heatingR",
            "Cooling"="coolingR",
            "HeatingNorm"="heatingR",
            "CoolingNorm"="coolingR"
          )
          baselineEnergyComponentsColumns <- c(
            "Total"="predicted",
            "Baseload"="baseload",
            "Heating"="heating",
            "Cooling"="cooling",
            "HeatingNorm"="heating",
            "CoolingNorm"="cooling"
          )

          # ggplot(model$data) +
          #   geom_line(aes(time,baseload+cooling),col="blue",alpha=0.7) +
          #   geom_line(aes(time,baseload+heating),col="red",alpha=0.7) +
          #   geom_line(aes(time,baseload),col="black",alpha=0.7)
          # ggplot(model$data) +
          #   geom_line(aes(time,baseloadR+coolingR),col="blue",alpha=0.7) +
          #   geom_line(aes(time,baseloadR+heatingR),col="red",alpha=0.7) +
          #   geom_line(aes(time,baseloadR),col="black",alpha=0.7)

          results_data <- model$data
          for (e in 1:length(energyComponentsColumns)){
            if(!(energyComponentsColumns[e] %in% colnames(results_data))){next}
            indicator_units <- settings$IndicatorsUnitsSubjects
            # Compute the weather normalized consumption if needed
            if(names(energyComponentsColumns)[e] %in% c("HeatingNorm","CoolingNorm")){
              results_data <-
                results_data[,!(colnames(results_data)=="rateByDD")] %>%
                left_join(
                  data.frame(time = as.Date(results_data$time, tz = tz),
                             temperature = results_data$temperature
                  ) %>%
                    group_by(time) %>%
                    summarise(temperature = mean(temperature)) %>%
                    ungroup() %>%
                    mutate(
                      degree_days(
                        ., "temperature", tz,
                        if (names(energyComponentsColumns)[e] == "HeatingNorm") {21} else {18},
                        if (names(energyComponentsColumns)[e] == "HeatingNorm") {"heating"} else {"cooling"},
                        outputFrequency = "P1D",
                        outputFeaturesName = "rateByDD",
                        fixedOutputFeaturesName = T
                      ),
                      date = as.Date(time, tz=tz)#,
                      #time = as.POSIXct(paste(format(time,"%Y-%m-%d"),"00:00:00"),tz=tz)
                    ) %>%
                    select(date,rateByDD), by="date") %>%
                mutate(
                  across(
                    energyComponentsColumns[e],
                    as.formula(sprintf("~ifelse(rateByDD>3,%s/rateByDD,0)",energyComponentsColumns[e]))
                  ),
                  across(
                    baselineEnergyComponentsColumns[e],
                    as.formula(sprintf("~ifelse(rateByDD>3,%s/rateByDD,0)",baselineEnergyComponentsColumns[e]))
                  )
                )
              # Change to the weather normalised units
              for (wnu in names(indicator_units)){
                indicator_units[[wnu]] <-
                  if(startsWith(indicator_units[[wnu]],"unit:") &&
                     !(e %in% c("HeatingDegreeDays", "CoolingDegreeDays"))){
                    paste0(gsub("unit:","ENTRACK:",indicator_units[[wnu]]),"-DEG_C")
                  } else if (!(wnu %in% c("HeatingDegreeDays", "CoolingDegreeDays"))){
                    paste0(indicator_units[[wnu]],"-DEG_C")
                  } else {
                    indicator_units[[wnu]]
                  }
              }
            }
            # Load the results to RDF and JSON format
            results <- generate_longitudinal_benchmarking_indicators(
              data = results_data,
              indicators = unlist(settings$TrendIndicators),
              indicatorsUnitsSubjects = indicator_units,
              measuredPropertyComponent = names(energyComponentsColumns)[e],
              measuredProperty = measuredPropertyOutput,
              frequencies = settings$Frequencies[
                as.period(settings$Frequencies) >= as.period(detect_time_step(results_data$time))],
              buildingId = buildingId,
              buildingSubject = buildingSubject,
              localTimeZone = tz,
              timeColumn = "time",
              consumptionColumn = energyComponentsColumns[e],
              energyPriceColumn = "Qe_price",
              carbonEmissionsColumn = "Qe_emissionsFactor",
              baselineConsumptionColumn = baselineEnergyComponentsColumns[e],
              buildingGrossFloorArea = buildingArea,
              modelName = model$name,
              modelId = model$id,
              modelLocation = model$subject,
              modelTypeSubject = sprintf("ENTRACK:%s",modelType),
              modelStorageInfrastructureSubject = "ENTRACK:MLFlow",
              estimateWhenAggregate = T,
              minRatioToTimeAggregateIndicators = settings$MinRatioToTimeAggregateIndicators,
              prevResults = results
            )
          }
        }
        gc(reset=T)
      }
      building_results[[j]] <- results
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: SUCCESS %s %s", buildingSubject, measuredPropertyOutput), stderr())
      }
    }, error=function(e){
      write(sprintf("###### Error ###### %s",e),stderr())
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: ERROR %s %s", buildingSubject, measuredPropertyOutput), stderr())
      }
      building_results[[j]] <- as.character(e)
    })
  }
  return(building_results)
}
