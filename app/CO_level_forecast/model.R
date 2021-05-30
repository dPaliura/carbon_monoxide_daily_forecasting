# Packages used in model
library(dplyr)
library(fpp2)
library(ggplot2)


# Some constants
data.dir <- "data/"
obsvs.fname <- "observations.csv"
sites.fname <- "sites.csv"
meths.fname <- "methods.csv"


# Load data
observarions <- read.csv(
    file = paste0(data.dir, obsvs.fname),
    colClasses = c(
        date_local = "Date",
        site_id = "factor",
        method_id = "factor"
    ))#[,-1]
cat("Observations data set loaded\n")

sites <- read.csv(
    file = paste0(data.dir, sites.fname),
    colClasses = c(
        state_code = "factor",
        state_name = "factor",
        county_code = "factor",
        county_name = "factor",
        cbsa_name = "factor",
        city_name = "factor",
        datum = "factor"
    )
)[,-1]
cat("Sites data set loaded\n")

methods <- read.csv(
    file = paste0(data.dir, meths.fname),
    stringsAsFactors = TRUE
)[,-1]
cat("Methods data set loaded\n")


getObservationsBySiteID <- function(id, obs = observarions){
    obs %>% filter(site_id==id) %>% arrange(date_local)
}


getModelBySiteId <- function(id, obs = observarions, ...){
    obs <- getObservationsBySiteID
}


classify <- function(latitude, longitude){
    if (!between(latitude, 24, 50)) stop("latitude must be between 24 and 50")
    if (!between(longitude, -125, -68)) stop("latitude must be between -126 and -60")
    dists <- sqrt((sites$latitude-latitude)^2 + (sites$longitude-longitude)^2)
    indx <- which.min(dists)
    sites$site_id[indx]
}
