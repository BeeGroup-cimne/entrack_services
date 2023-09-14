
generate_cross_sectional_benchmarking_indicator <- function(
    data, isReal, indicator, frequency, groupSubject, prefixUtcTimeColumns,
    indicatorsUnitsSubjects, prevResults = NULL){
  
  groupNamespace <- paste0(strsplit(groupSubject,"#")[[1]][1],"#")
  groupId <- strsplit(groupSubject,"#")[[1]][2]
  
  if (is.null(prevResults)) {
    prevResults <- list(results_rdf=rdf(), results_ts=list()) 
  }
  
  obj <- prevResults$results_rdf
  results_ts <- prevResults$results_ts
  
  distinct_local_tz <- 
    gsub(prefixUtcTimeColumns,"",colnames(data)[grepl(prefixUtcTimeColumns,colnames(data))])
  value_columns <- colnames(data)[!grepl(paste0("localtime|",prefixUtcTimeColumns),colnames(data))]
  data$localtime <- NULL
  
  for (local_tz in distinct_local_tz){
    indDfAux <- data.frame(
        "start" = data[,paste0("utctime_",local_tz)],
        "value" = mapply(function(i){
          toJSON(as.list(data[i, !grepl(prefixUtcTimeColumns,colnames(data))]),
                 auto_unbox = T)}, 
          1:nrow(data)), 
        "isReal" = isReal)
    if (as.period(frequency) >= as.period("P1D")) {
      indDfAux$end <- with_tz(with_tz(indDfAux$start, local_tz) + 
                                iso8601_period_to_timedelta(frequency) - 
                                seconds(1), "UTC")
    } else {
      indDfAux$end <- indDfAux$start + iso8601_period_to_timedelta(frequency) - 
        seconds(1)
    }
    keyPerformanceIndicatorName <- indicator
    keyPerformanceIndicatorSubject <- paste("ENTRACK:KPI", 
                                            keyPerformanceIndicatorName, sep = "-")
    aggKPISubject <- paste(paste0(groupNamespace,"AggregatedKPI"), 
                           groupId, keyPerformanceIndicatorName, isReal, frequency, sep = "-")
    aggKPISubjectHash <- digest(namespace_integrator(aggKPISubject, 
                                                        ENTRACK_namespaces), "sha256", serialize = T)
    aggKPIPointSubject <- paste0(groupNamespace,aggKPISubjectHash)
    obj %>% add_item_to_rdf(
      subject = aggKPISubject, 
      classes = c("ENTRACK:AggregatedKPIAssessment", "ENTRACK:KPIAssessment", 
                  "ENTRACK:TimeSeriesList"), 
      dataProperties = list(`ENTRACK:timeSeriesIsRegular` = T, 
                            `ENTRACK:timeSeriesIsOnChange` = F, 
                            `ENTRACK:timeSeriesIsCumulative` = F, 
                            `ENTRACK:timeSeriesStart` = min(indDfAux$start, na.rm = T), 
                            `ENTRACK:timeSeriesEnd` = max(indDfAux$end,  na.rm = T), 
                            `ENTRACK:timeSeriesFrequency` = frequency, 
                            `ENTRACK:localTimeZone` = local_tz,
                            `ENTRACK:timeSeriesTimeAggregationFunction` = "AVG"), 
      objectProperties = 
        list(`ENTRACK:hasKPIUnit` = indicatorsUnitsSubjects[[indicator]], 
             `ENTRACK:hasAggregatedKPIPoint` = aggKPIPointSubject, 
             `ENTRACK:quantifiesKPI` = keyPerformanceIndicatorSubject), 
      namespaces = ENTRACK_namespaces)
    obj %>% add_item_to_rdf(subject = groupSubject, 
                            objectProperties = list(`ENTRACK:assessesAggregatedKPI` = aggKPISubject), 
                            namespaces = ENTRACK_namespaces)
    indDfAux$start <- parsedate::format_iso_8601(indDfAux$start)
    indDfAux$end <- parsedate::format_iso_8601(indDfAux$end)
    
    results_ts[[aggKPISubjectHash]]<-indDfAux
  }
  return(list(results_rdf=obj, results_ts=results_ts))
}
