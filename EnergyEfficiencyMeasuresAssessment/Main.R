source("Utils.R")
source("EEMsAssessment.R")

# Load configuration paths and settings for the algorithm
settings <- jsonlite::fromJSON("Settings.json")
settings <- append(settings,jsonlite::fromJSON("config.json"))

# Unzip input data if is needed
if(!dir.exists(settings$InputDataDirectory)){
  unzip(paste0(settings$InputDataDirectory,".zip"),
        exdir = settings$InputDataDirectory,junkpaths = T)
}

# Read buildings TTL file and time series
buildingsRdf <- suppressMessages(
  rdflib::rdf_parse(
    list.files(settings$InputDataDirectory,".ttl", full.names = T)[[1]],
    format = "turtle"))
timeseriesObject <- settings$InputDataDirectory
# buildingsRdf <- rdflib::rdf_parse("~/Downloads/00048/rdf.ttl", format = "turtle")
# timeseriesObject <- jsonlite::fromJSON("~/Downloads/00048/timeseries.json")

# Run the Energy Efficiency Measures assessment
run_EEMs_assessment(buildingsRdf, timeseriesObject, settings)
