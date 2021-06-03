##
#   Packages used in app model
##
library(dplyr)
library(forecast)
library(ggplot2)


##
#   Some constants and useful functions
##
data.dir <- "data/"
obsvs.fname <- "observations.csv"
sites.fname <- "sites.csv"
opt.TW.pars.fname <- "optimal_TW_parameters.csv"


getObservationsBySiteID <- function(id, obs = observations){
    obs %>% filter(site_id==id) %>% arrange(date_local)
}


getModelBySiteId <- function(id, obs = observations, ...){
    obs <- getObservationsBySiteID
}


# Method which returns id of nearest site to given coordinates
classify <- function(latitude, longitude){
    if (!between(latitude, 24, 50)) stop("latitude must be between 24 and 50")
    if (!between(longitude, -125, -68)) stop("latitude must be between -126 and -60")
    dists <- sqrt((sites$latitude-latitude)^2 + (sites$longitude-longitude)^2)
    indx <- which.min(dists)
    sites$site_id[indx]
}


##
#   Forecasting method - Theil-Wage model
##
theil.wage <- function(ts, period, lev.par, seas.par, trend.par, trend=TRUE){
    n <- length(ts)
    s <- period

    regr <- lm(vals~indx, data=data.frame(vals=ts[1:period], indx=1:s))
    a_prev <- regr$coefficients[1]
    b_prev <- if (trend) regr$coefficients[2] else 0

    par1 <- lev.par; par2 <- seas.par; par3 <- if (!trend) 0 else trend.par


    regr2 <- lm(vals~indx, data=data.frame(vals=ts[(s+1):(2*s)], indx=(s+1):(2*s)))
    theta <- ((ts[1:s]-predict(regr)) + (ts[(s+1):(2*s)]-predict(regr2)))/2

    control <- NULL
    for (i in 1:n){
        a_nxt <- par1*(ts[i]-theta[i]) + (1-par1)*(a_prev-b_prev)
        b_nxt <- par3*(a_nxt-a_prev) + (1-par3)*b_prev
        theta_nxt <- par2*(ts[i]-a_nxt) + (1-par2)*theta[i]

        control <- c(control, a_prev + b_prev + theta_nxt)

        a_prev <- a_nxt
        b_prev <- b_nxt
        theta <- c(theta, theta_nxt)
    }

    return(list(
        a = a_nxt,
        b = b_nxt,
        theta = theta[(n+1):(n+s)],
        period = s,
        control = control
    ))
}

# Function to predict Theil-Wage model
theil.wage.predict <- function(tw.mod, horizon=1){
    tw.mod$a + horizon*tw.mod$b + tw.mod$theta[(horizon-1)%%tw.mod$period+1]
}



##
#   Load data
##
observations <- read.csv(
    file = paste0(data.dir, obsvs.fname),
    colClasses = c(
        date_local = "Date",
        site_id = "factor",
        method_id = "factor"
    ))
cat("Observations data set loaded\n")

sites <- read.csv(
    file = paste0(data.dir, sites.fname))[,-1]
cat("Sites data set loaded\n")

opt.TW.pars <- read.csv(
    file = paste0(data.dir, opt.TW.pars.fname),
    stringsAsFactors = TRUE
)
cat("Theil-Wage optimal parameters data set loaded\n")

