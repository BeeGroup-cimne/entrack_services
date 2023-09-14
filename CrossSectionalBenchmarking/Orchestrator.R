library(rentrack)
library(data.table)
library(jsonlite)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(plotly)
library(padr)
library(htmlwidgets)
library(carrier)
library(mlflow)
library(fs)
library(tidyr)
library(digest)

home <- "Documents/GitHub/BC1_pipelines/"
# source(path_home(paste0(home,"CrossSectionalBenchmarking/DetectSimilars.R")))
# source(path_home(paste0(home,"CrossSectionalBenchmarking/Aggregator.R")))

settings <- fromJSON(path_home(
  paste0(home,"CrossSectionalBenchmarking/Settings.json")), simplifyDataFrame = F)
settings$InputDataDirectory <- as.character(path_home(paste0(home,settings$InputDataDirectory)))
settings$OutputDataDirectory <- as.character(path_home(paste0(home,settings$OutputDataDirectory)))
dir.create(settings$OutputDataDirectory,showWarnings = F)

ttl_ <- c()
json_ <- c()
for(inp in settings$InputDataDirectory){
  if(!dir.exists(inp)){
    unzip(paste0(inp,".zip"),exdir = inp,junkpaths = T)
  }
  ttl_ <- c(ttl_,list.files(inp,".ttl",full.names = T))
  json_ <- c(json_,list.files(inp,".json",full.names = T))
}
buildingsRdf <- do.call(c,lapply(ttl_,function(i){
  suppressMessages(rdf_parse(i, format = "turtle"))} ))
timeseriesObject <- json_

groups_matrix <- get_analytical_groups_by_building_subject(buildingsRdf)
groups <- colnames(groups_matrix)[!(colnames(groups_matrix) %in% "buildingSubject")]

for (group in groups){
  
  tryCatch({
    write("",stderr())
    write("######################################",stderr())
    write("#### Cross-Sectional benchmarking ####",stderr())
    write("######################################",stderr())
    write("",stderr())
    
    write(sprintf("Calculating results for group: %s",
                  group),stderr())
    
    buildingSubjects <- groups_matrix$buildingSubject[groups_matrix[,group]>0]
    tz <- get_tz_building(buildingsRdf, buildingSubjects)
    objResults <- list(results_rdf=rdf(), results_ts=list())
    
    for(IOcase in settings$InputOutputDescription){
      
      write(sprintf("\n                        indicator: %s",
                    IOcase$AggregatedKPIOutput$Name),stderr())
      
      for (frequency in settings$Frequencies){
        
        write(sprintf("\n                        frequency: %s",
              frequency),stderr())
        
        KPIresults <- lapply(buildingSubjects,function(buildingSubject){
          timeseriesKPI <- get_KPI_by_building(
            buildingsRdf = buildingsRdf,
            timeseriesObject = timeseriesObject,
            buildingSubject = buildingSubject,
            KPI = IOcase$SingleKPIInput,
            frequency = frequency,
            localTz = tz[buildingSubject],
          )
        })
        KPIresults <- KPIresults[mapply(function(i)!is.null(i),KPIresults)]
        results <- if(length(KPIresults)==1){
            KPIresults[[1]]
          } else if(length(KPIresults)>1){
            Reduce(function(df1,df2){
              df1 %>% full_join(
                df2[!(colnames(df2) %in% colnames(df1)) | 
                      grepl("localtime",colnames(df2))],
                by="localtime")
              },KPIresults)
          } else {
            NULL
          }
        if(!is.null(results)){
          for (tz_i in unique(tz)){
            results[,paste0("utctime_",tz_i)] <- with_tz(as.POSIXct(results[,"localtime"],tz_i),"UTC")
          }
          results_time <- results %>% select(
            grep("utctime|localtime",colnames(results)))
          results <- results %>% select(
            -grep("utctime|localtime",colnames(results)))
          iqr <- matrixStats::rowIQRs(
            as.matrix(results),na.rm=T)
          results <- data.frame(
            "n" = matrixStats::rowSums2(
              is.finite(as.matrix(results)),na.rm = T),
            setNames( as.data.frame( matrixStats::rowQuantiles(
              as.matrix(results),
                probs = c(0.025,0.05,0.25,0.5,0.75,0.95,0.975),na.rm = T)),
            c("p2.5","p5","p25","p50","p75","p95","p97.5")),
            "mean" = matrixStats::rowMeans2(as.matrix(results),
                                            na.rm = T),
            "uw" = matrixStats::rowMins(
              as.matrix(data.frame(
                matrixStats::rowQuantiles(
                  as.matrix(results),probs = 0.75,na.rm = T) + 1.5*iqr,
                matrixStats::rowMaxs(
                  as.matrix(results),na.rm = T)
              ))),
            "lw" =  matrixStats::rowMaxs(
              as.matrix(data.frame(
                matrixStats::rowQuantiles(
                  as.matrix(results),probs = 0.25,na.rm = T) - 1.5*iqr,
                matrixStats::rowMins(
                  as.matrix(results),na.rm = T)
              ))),
            "min" = matrixStats::rowMins(
              as.matrix(results),na.rm = T),
            "max" = matrixStats::rowMaxs(
              as.matrix(results),na.rm = T)
          )
          results <- cbind(results,results_time)
          objResults <- generate_cross_sectional_benchmarking_indicator(
            data = results, 
            isReal = !IOcase$SingleKPIInput$FromModel,
            indicator = IOcase$AggregatedKPIOutput$Name,
            frequency = frequency,
            groupSubject = group, 
            prefixUtcTimeColumns = "^utctime_",
            indicatorsUnitsSubjects = settings$IndicatorsUnitsSubjects,
            prevResults = objResults
          )
        }
      }
    }
    
    # Write the RDF file
    write_rdf(object = objResults$results_rdf,
              file = paste(settings$OutputDataDirectory,
                           sprintf("%s.ttl", digest(
                             paste0(group, unlist(settings$InputOutputDescription), 
                                    collapse="~"),
                             algo = "sha256", serialize = T)), sep="/"))
    
    # Write the JSON files (time series)
    write(jsonlite::toJSON(
      objResults$results_ts,
      dataframe = "rows",na = "null"),
      file=paste(settings$OutputDataDirectory,
                 sprintf("%s.json",digest(
                   paste0(group, unlist(names(objResults$results_ts)), 
                          collapse="~"),
                   algo = "sha256", serialize = T)),
                 sep="/") )
    
    gc(reset=T)
    
    }, 
    
    error=function(e){write(paste("###### Error ######", e),stderr())})  
}

