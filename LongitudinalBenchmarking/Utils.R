mongo_conn <- function(collection, settings){
  mongolite::mongo(collection=collection,db=settings$MongoConnection$DB,
          url=sprintf("mongodb://%s:%s@%s:%s",
                      settings$MongoConnection$User,settings$MongoConnection$Password,
                      settings$MongoConnection$Host,settings$MongoConnection$Port))
}

mongo_check <- function(collection, settings){
  return(!is.null(
    tryCatch(
      mongo_conn(collection,settings),
      error=function(e) NULL
    )))
}

save_results <- function(results, settings){
    
    suppressMessages(suppressWarnings(library(rdflib)))
    suppressMessages(suppressWarnings(library(jsonlite)))
    suppressMessages(suppressWarnings(library(mongolite)))
    
    # Write the RDF file
    write_rdf(
        object = results$results_rdf,
        file = paste(
            settings$OutputDataDirectory,
            sprintf(
                "%s.ttl",
                digest::digest(
                    object = results$key,
                    algo = "sha256",
                    serialize = T
                )
            ),
            sep="/"
        )
    )
    
    # Write the JSON files (time series)
    for(i in 1:length(results$results_ts)){
        write(
            jsonlite::toJSON(
                setNames(
                    list(results$results_ts[[i]]$basic),
                    names(results$results_ts)[i]),
                dataframe = "rows",na = "null"
            ),
            file=paste(
                settings$OutputDataDirectory,
                sprintf("%s.json", names(results$results_ts)[i]
                ),
                sep="/"
            )
        )
        
        columns_basic <- colnames(results$results_ts[[i]]$basic)
        
        # Upload the results for  cross-sectional benchmarking services, if they are available
        columns_for_collection_name <- c("keyPerformanceIndicator","frequency")
        mongo_collection <- paste0(
          "SingleKPI-Building-",
          paste(
            results$results_ts[[i]]$full[1,columns_for_collection_name],
            collapse="-"
          ))
        if(mongo_check(mongo_collection, settings) && 
           ("full" %in% names(results$results_ts[[i]])) &&
           nrow(results$results_ts[[i]]$full)>0){
            results_aux <- results$results_ts[[i]]$full[,!(colnames(results$results_ts[[i]]$full) %in% columns_for_collection_name)]
            columns_unique_values <- colnames(results_aux)[!(colnames(results_aux) %in% c(
                columns_basic,"modelSubject"))]
            # ensure index for variables that can be filtered
            if(mongo_conn(mongo_collection, settings)$info()$stats$count==0L){
                mongo_conn(mongo_collection, settings)$index(
                    add=jsonlite::toJSON(
                        as.list(
                            setNames(
                                rep(1,length(columns_unique_values)),
                                nm=columns_unique_values
                            )
                        ),
                        auto_unbox = T
                    )
                )
                for(feat in columns_unique_values){
                    mongo_conn(mongo_collection, settings)$index(
                        add=jsonlite::toJSON(as.list(setNames(1, nm=feat)),auto_unbox = T))
                }
            }
            # update and upsert each timestep of results
            for(r in 1:nrow(results_aux)){
                mongo_conn(mongo_collection, settings)$replace(
                    query=jsonlite::toJSON(jsonlite::unbox(as.data.frame(as.list(results_aux[r,columns_unique_values]))),
                                           POSIXt = "mongo",auto_unbox = T),
                    update=jsonlite::toJSON(jsonlite::unbox(as.data.frame(as.list(results_aux[r,]))),
                                            POSIXt = "mongo",auto_unbox = T),
                    upsert=T
                )
            }
        }
    }
}

run_benchmarking <- function(buildingsRdf, timeseriesObject, settings){
    suppressMessages(suppressWarnings(library(rentrack)))
    allBuildingSubjects <- get_all_buildings_list(buildingsRdf)
    for (buildingSubject in allBuildingSubjects){
      tryCatch(
        {
          building_results <- longitudinal_benchmarking(buildingSubject, buildingsRdf, timeseriesObject, settings,
                                                        libraryPath = ".",updateHadoopStatus = F)
          for(results in building_results){
              results$key <- buildingSubject
              if(typeof(results)=="list"){
                  save_results(results, settings)
              } else {
                  write(results, stderr())
              }
          }
        }, 
        error=function(e){NULL}
      )
    }
}
