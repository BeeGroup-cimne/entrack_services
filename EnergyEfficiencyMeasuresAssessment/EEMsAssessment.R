EEMs_assessment <- function(buildingSubject, buildingsRdf, timeseriesObject, settings, libraryPath=".", updateHadoopStatus=F){

  library(rentrack)
  
  source(paste(libraryPath,"HourlyMeasurementAndVerificationModel2.R", sep="/"))
  source(paste(libraryPath,"MonthlyMeasurementAndVerificationModel.R", sep="/"))
  
  dir.create(settings$OutputDataDirectory,showWarnings = F)
  
  buildingId <- get_building_identifiers(buildingSubject)
  
  write("",stderr())
  write("#############################################",stderr())
  write("#### Measurement and verification of EEM ####",stderr())
  write("#############################################",stderr())
  write(sprintf("   Building: %s", buildingSubject),stderr())
  write("",stderr())
  
  tz <- get_tz_building(buildingsRdf, buildingSubject)
  buildingArea <- get_area_building(buildingsRdf, buildingSubject)
  
  results <- list()
  
  cases <- lapply(1:length(settings$MeasuredPropertiesDeviceAggregators), function(j){
    list(
      deviceAggregators = names(settings$MeasuredPropertiesDeviceAggregators)[[j]],
      measuredPropertyOutput = settings$MeasuredPropertiesDeviceAggregators[[j]],
      energyMeasuredPropertiesInput = c(settings$MeasuredPropertiesDeviceAggregators[[j]]),
      otherMeasuredPropertiesInput = c("Temperature"),
      defaultFactors = NULL,
      transformToAggregatableMeasuredProperty = F)
  })
  if(length(settings$TransformToAggregatableMeasuredProperties)>0){
    for (ovmp in names(settings$TransformToAggregatableMeasuredProperties)){
      cases[[length(cases)+1]] <- list(
          deviceAggregators = settings$TransformToAggregatableMeasuredProperties[[ovmp]]$DeviceAggregatorsToAggregate,
          measuredPropertyOutput = ovmp,
          energyMeasuredPropertiesInput = unlist(settings$MeasuredPropertiesDeviceAggregators[
            settings$TransformToAggregatableMeasuredProperties[[ovmp]]$DeviceAggregatorsToAggregate]),
          otherMeasuredPropertiesInput = c("Temperature"),
          defaultFactorsByMeasuredProperty = settings$TransformToAggregatableMeasuredProperties[[ovmp]]$DefaultFactorsByMeasuredProperty,
          transformToAggregatableMeasuredProperty = T)
    }
  }
  
  for (case in cases){
    tryCatch({
      # case=cases[[3]]
      deviceAggregator <- case$deviceAggregators
      measuredPropertyOutput <- case$measuredPropertyOutput
      transformToAggregatableMeasuredProperty <- case$transformToAggregatableMeasuredProperty
      measuredPropertiesInput <- c(case$energyMeasuredPropertiesInput,case$otherMeasuredPropertiesInput)
      energyMeasuredPropertiesInput <- case$energyMeasuredPropertiesInput
      defaultFactorsByMeasuredProperty <- case$defaultFactorsByMeasuredProperty
      
      write(sprintf("* Measured property for this iteration: %s", measuredPropertyOutput),stderr())
      
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: RUNNING %s %s", buildingSubject, measuredPropertyOutput), stderr())
      }
      
      buildingData <- get_device_aggregators(
        buildingsRdf, 
        timeseriesObject, #If character, data directory is provided; 
        #if list of dataframes, the timeseries are provided.
        allowedBuildingSubjects=buildingSubject, 
        allowedMeasuredProperties=measuredPropertiesInput,
        useEstimatedValues = F, ratioCorrection = T, containsEEMs = T,
        alignGreaterThanHourlyFrequencyToYearly = F,
        transformToAggregatableMeasuredProperty = transformToAggregatableMeasuredProperty,
        aggregatableMeasuredPropertyName = measuredPropertyOutput,
        measuredPropertiesToAggregate = energyMeasuredPropertiesInput,
        defaultFactorsByMeasuredProperty = defaultFactorsByMeasuredProperty
        )
      if(transformToAggregatableMeasuredProperty) deviceAggregator <- measuredPropertyOutput
      
      # If the measured property is not available in buildingData, deprecate this case
      if(!any(grepl(measuredPropertyOutput,colnames(buildingData[[buildingSubject]]$df)))) {
        write("* No data related with the measured property. Theoretically, this measured property was associated with the\n  following device aggregators and measurement identifiers:",stderr())
        write(paste("   ",unique(
          paste(buildingData[[buildingSubject]]$metadata$deviceAggregatorName,
                buildingData[[buildingSubject]]$metadata$deviceAggregatorFormula,sep=": ")),collapse="\n"),stderr())
        next
      }
      
      write("",stderr())
      write("* Reading and aligning the input data",stderr())
      
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: READING DATA %s %s", buildingSubject, measuredPropertyOutput), stderr())
      }
      
      # Outdoor temperature data in degree days format
      if(any(grepl("HDD|CDD",colnames(buildingData[[buildingSubject]]$df),fixed=F))){
        df <- cbind(
          data.frame(
            "time" = buildingData[[buildingSubject]]$df$time,
            "Qe" = buildingData[[buildingSubject]]$df[,
                                                      paste0(deviceAggregator,".SUM_",measuredPropertyOutput)],
            "Qe_cost" = buildingData[[buildingSubject]]$df[,
                                                           paste0(deviceAggregator,".SUM_EnergyCost")],
            "Qe_emissions" = buildingData[[buildingSubject]]$df[,
                                                                paste0(deviceAggregator,".SUM_EnergyEmissions")],
            "Qe_price" = buildingData[[buildingSubject]]$df[,
                                                            paste0(deviceAggregator,".AVG_",measuredPropertyOutput,"_EnergyPrice")],
            "Qe_emissionsFactor" = buildingData[[buildingSubject]]$df[,
                                                                      paste0(deviceAggregator,".AVG_",measuredPropertyOutput,"_EnergyEmissionsFactor")]
          ),
          setNames(buildingData[[buildingSubject]]$df[,grepl("HDD|CDD",colnames(buildingData[[buildingSubject]]$df),fixed=F)],
                   gsub("outdoorTemperature.|_Temperature","",colnames(buildingData[[buildingSubject]]$df)[
                     grepl("HDD|CDD",colnames(buildingData[[buildingSubject]]$df),fixed=F)]))
        )
        # Outdoor temperature data in raw format
      } else {
        df <- data.frame(
          "time" = buildingData[[buildingSubject]]$df$time,
          "temperature" = buildingData[[buildingSubject]]$df[,paste0("outdoorTemperature",".AVG_Temperature")],
          "Qe" = buildingData[[buildingSubject]]$df[,
                                                    paste0(deviceAggregator,".SUM_",measuredPropertyOutput)],
          "Qe_cost" = buildingData[[buildingSubject]]$df[,
                                                         paste0(deviceAggregator,".SUM_EnergyCost")],
          "Qe_emissions" = buildingData[[buildingSubject]]$df[,
                                                              paste0(deviceAggregator,".SUM_EnergyEmissions")],
          "Qe_price" = buildingData[[buildingSubject]]$df[,
                                                          paste0(deviceAggregator,".AVG_",measuredPropertyOutput,"_EnergyPrice")],
          "Qe_emissionsFactor" = buildingData[[buildingSubject]]$df[,
                                                                    paste0(deviceAggregator,".AVG_",measuredPropertyOutput,"_EnergyEmissionsFactor")]
        )
      }
      
      # Filter dataframe between the minimum and maximum time with the consumption data
      # df <- df[min(which(is.finite(df$Qe))):(nrow(df)+1-min(which(is.finite(rev(df$Qe))))),]
      df <- df[!is.na(df$Qe),]
      
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
      } else {
        covid_affected_period_inferred <- c()
      }
      if(length(covid_affected_period_inferred)>0){covid_affected_period <- covid_affected_period_inferred}
      
      # Add the EEMs in buildingData
      eems <- get_building_eems(buildingsRdf,buildingSubject)
      eems_details <- get_eem_details(buildingsRdf,eems$eemSubject)
      eems_details <- eems_details %>% left_join(eems,by="eemSubject")
      eems_details$Date <- as.Date(eems_details$Date, tz=tz)
      eems_details <- eems_details[order(eems_details$Date,decreasing = F),]
      eems_details$AffectationShare <- ifelse(is.finite(eems_details$AffectationShare),
                                              eems_details$AffectationShare, settings$EEMDefaultShareOfAffectedElement)
      eems_details$Lifespan <- get_eem_lifespan(lifespans = settings$EEMLifespan,
                                                eemTypes = eems_details$Type)
      eems_details <- cbind(eems_details, 
                            get_eem_measured_property_components(componentsPerEem = settings$EEMMeasuredPropertyComponents,
                                                                 eemTypes = eems_details$Type,
                                                                 prefixColumns = "MeasuredPropertyComponent_"))
      eems_details$DiscountRate <- settings$EEMDiscountRate
      df$daysEquivalent <- as.period(detect_time_step(df$time)) / as.period("P1D")
      eems_details$DateNext <- dplyr::lead(eems_details$Date,1)
      eems_details$DateNext <- as.Date(ifelse(is.na(eems_details$DateNext),as.Date(max(df$time),tz=tz)+1,eems_details$DateNext),tz=tz)
      eems_details$DateLast <- dplyr::lag(eems_details$Date,1)
      eems_details$DateLast <- as.Date(ifelse(is.na(eems_details$DateLast),as.Date(min(df$time),tz=tz)-1,eems_details$DateLast),tz=tz)
      eems_details$TrainingDays <- mapply(function(x){
        d <- eems_details$Date[x]
        sum(df$daysEquivalent[as.Date(df$time,tz=tz)<d &
            !(as.Date(df$time, tz=tz) %in% covid_affected_period)])}, 1:nrow(eems_details))
      eems_details$TrainingDataTimesteps <- mapply(function(x){
        d <- eems_details$Date[x]
        sum(as.Date(df$time,tz=tz)<d & 
            !(as.Date(df$time, tz=tz) %in% covid_affected_period))}, 1:nrow(eems_details))
      eems_details$NextMeasureDays <- mapply(function(x){
        d <- eems_details$Date[x]
        dn <- eems_details$DateNext[x]
        sum(df$daysEquivalent[as.Date(df$time,tz=tz)>=d & as.Date(df$time,tz=tz)<dn &
                                !(as.Date(df$time, tz=tz) %in% covid_affected_period)])}, 1:nrow(eems_details))
      eems_details$ValidationDays <- mapply(function(x){
        d <- eems_details$Date[x]
        sum(df$daysEquivalent[as.Date(df$time,tz=tz)>=d &
                              !(as.Date(df$time, tz=tz) %in% covid_affected_period)])}, 1:nrow(eems_details))
      eems_details_unfiltered <- eems_details
      eems_details <- eems_details %>% filter(
        (eems_details$TrainingDays > settings$EEMAssessmentConditions$MinimumDaysBeforeEEM) &
          (eems_details$TrainingDataTimesteps > settings$EEMAssessmentConditions$MinimumTimestepsBeforeEEM) &
          (eems_details$ValidationDays > settings$EEMAssessmentConditions$MinimumDaysAfterEEM)
      )
      # Get the EEM projects already defined
      eems_project <- get_eem_projects(buildingsRdf, buildingSubject)
      if(nrow(eems_details)>0 && !is.null(eems_project)){
        eems_details <- eems_details %>% 
          left_join(eems_project[,c("eemSubject","eemProjectSubject","eemProjectId")],by="eemSubject")
        if(settings$EEMAssessmentConditions$ForceConsiderEEMProjectInvestment){
          eem_proj_mult <- data.frame("eemProjectSubject" = unique(eems_project$eemSubject), 
                                    "multiplier" = table(eems_project$eemSubject)) %>% 
            left_join(eems_project[,c("eemProjectSubject","Investment")], by="eemProjectSubject") %>%
            mutate(Investment = Investment / multiplier)
          eems_details <- eems_details %>% select(-Investment) %>% 
            left_join(eem_proj_mult, by="eemProjectSubject")
        }
      } else if(nrow(eems_details)>0){
        eems_details$eemProjectId <- 1
        eemProjectId <- 1
      }
      if(nrow(eems_details)>0){
        if(is.null(eems_project) || settings$EEMAssessmentConditions$ForceAssignedEEMProjects==F){
          for(g in 1:nrow(eems_details)){
            if(eems_details$NextMeasureDays[g] > settings$EEMAssessmentConditions$MinimumDaysAfterEEM){
              eems_details$eemProjectId[g] <- eemProjectId
              eemProjectId <- eemProjectId + 1
            } else {
              eems_details$eemProjectId[g] <- eemProjectId
            }
          }
        }
        for(eemProjectId in unique(eems_details$eemProjectId)) {
          eems_in_project <- eems_details[eems_details$eemProjectId==eemProjectId,]
          days_max_after_eem <- settings$EEMAssessmentConditions$MaximumDaysAfterEEM
          if(length(covid_affected_period)>0){
            days_max_after_eem <- days_max_after_eem +
              length(covid_affected_period[covid_affected_period>=max(eems_in_project$Date) & 
                                             covid_affected_period<max(eems_in_project$DateNext)])
          }
          if(as.period(detect_time_step(df$time))<=as.period("PT1H")){
            df[,paste0("eemProject.",eemProjectId)] <- 
              ifelse(as.Date(df$time,tz=tz) < min(eems_in_project$DateLast), NA,
                     ifelse(
                       as.Date(df$time,tz=tz) < min(eems_in_project$Date),0,
                       ifelse(as.Date(df$time,tz=tz) >= max(eems_in_project$Date) & 
                                as.Date(df$time,tz=tz) < max(eems_in_project$DateNext),
                                1, NA)
                       )
              )
            df[,paste0("eemProject.",eemProjectId)] <- 
              ifelse(as.Date(df$time,tz=tz)<=(max(eems_in_project$Date) + days_max_after_eem),
                     df[,paste0("eemProject.",eemProjectId)], NA)
          } else {
            df_aux <- data.frame("time"= seq(min(df$time), max(df$time) + as.period(detect_time_step(df$time)), by="hours"))
            df_aux[,paste0("eemProject.",eemProjectId)] <- ifelse(df_aux$time<min(eems_in_project$Date),0,
                                                                  ifelse(df_aux$time>=max(eems_in_project$Date),1,NA))
            df_aux[,paste0("eemProject.",eemProjectId)] <- 
              ifelse(as.Date(df_aux$time,tz=tz) < min(eems_in_project$DateLast), NA,
                     ifelse(
                       as.Date(df_aux$time,tz=tz) < min(eems_in_project$Date),0,
                       ifelse(as.Date(df_aux$time,tz=tz) >= max(eems_in_project$Date) & 
                                as.Date(df_aux$time,tz=tz) < max(eems_in_project$DateNext),
                              1, NA)
                     )
              )
            df_aux[,paste0("eemProject.",eemProjectId)] <- 
              ifelse(as.Date(df_aux$time,tz=tz)<=(max(eems_in_project$Date) + days_max_after_eem),
                     df_aux[,paste0("eemProject.",eemProjectId)], NA)
            df_aux$agg <- cumsum(df_aux$time %in% df$time)
            df_aux <- df_aux %>% group_by(agg) %>% 
              summarise(time=first(time),across(paste0("eemProject.",eemProjectId),mean)) %>% 
              ungroup() %>% select(-agg)
            df <- df %>% left_join(df_aux, by="time")
          }
        }
        
      } else {
        write("* Any EEM project overlap the minimum range considered in settings. The savings can't be estimated. ",stderr())
        #write(jsonlite::toJSON(as.list(eems_details_unfiltered),pretty = T,auto_unbox = T),stderr())
        next
      }
      
      modelName <- if(detect_time_step(df$time) <= as.duration("PT1H")){
        "HourlyMeasurementAndVerificationModel"
      } else if(detect_time_step(df$time) == as.duration("P1M")) {
        "MonthlyMeasurementAndVerificationModel"
      } else {
        ""
      }
      
      # Do not allow models that are not yet available
      if(!(modelName %in% c("HourlyMeasurementAndVerificationModel"))){
        write(sprintf("* Sorry, %s is not yet available to assess EEMs.",modelName),stderr())
        next
      }
      
      # Evaluate the model, if needed, and estimate the savings
      write(sprintf("* Estimating the savings of EEMs based on %s",modelName),stderr())

      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: ESTIMATING SAVINGS: %s, %s", buildingSubject, measuredPropertyOutput), stderr())
      }
      
      call <- paste0(
        modelName, paste0("(df = df, buildingId = buildingId, buildingSubject = buildingSubject,
                            identifier = paste(buildingId,deviceAggregator,'EEM',sep='~'),
                            settings = settings, tz = tz,
                            plots = T, ignoredDates = covid_affected_period,
                            eems = eems_details, measuredProperty = measuredPropertyOutput,
                            buildingGrossFloorArea = buildingArea, results = results)"))
      results <- eval(parse(text=call))
      
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: SUCCESS %s %s", buildingSubject, measuredPropertyOutput), stderr())
      }
      
    }, error=function(e){
      write(sprintf("###### Error ###### %s",e),stderr())
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: ERROR %s %s", buildingSubject, measuredPropertyOutput), stderr())
      }
    })
  }
  return(results)
}
