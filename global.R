
library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(dplyr)
library(sf)
library(terra)
#library(raster)
library(leaflet)

get_crs <- function(crs_name){
  
  dplyr::case_when(
    crs_name == "EPSG:4326" ~ 4326,
    crs_name == "EPSG:3005" ~ 3005,
    crs_name == "BC Albers" ~ 3005,
    crs_name == "UTM Zone 7N" ~ 3154,
    crs_name == "UTM Zone 8N" ~ 3155,
    crs_name == "UTM Zone 9N" ~ 3156,
    crs_name == "UTM Zone 10N" ~ 3157,
    crs_name == "UTM Zone 11N" ~ 2955,
    TRUE ~ 3005
  )
  
}