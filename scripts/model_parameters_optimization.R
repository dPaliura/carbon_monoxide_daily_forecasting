library(dplyr)


##
#   Some constants and functions
##
data.dir <- "app/CO_level_forecast/data/"
obsvs.fname <- "observations.csv"
saved.file.name <- "optimal_TW_parameters.csv"


getObservationsBySiteID <- function(id, obs = observations){
    obs %>% filter(site_id==id) %>% arrange(date_local)
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


##
#   Read data to work with
##
observations <- read.csv(
    file = paste0(data.dir, obsvs.fname))
cat("Observations data set loaded\n")


##
#   Optimize parameters for all sites available
##
#
# Here is the resulting table to be written
result <- data.frame(site_id = numeric(),
                     arithmean_level_par  = numeric(0),
                     arithmean_season_par = numeric(0),
                     arithmean_trend_par  = numeric(0),
                     firstmax_level_par  = numeric(0),
                     firstmax_season_par = numeric(0),
                     firstmax_trend_par  = numeric(0))
res.names <- names(result)


all_ids <- sort(unique(observations$site_id))
counter <- 1
whole <- length(all_ids)
par0 <- c(.3, .3, 0)
for (id in all_ids){
    cat(counter, "/", whole, "\n", sep="")
    obs <- getObservationsBySiteID(id)

    ts1 <- obs$arithmetic_mean
    f1 <- function(x) sd(ts1 - theil.wage(ts1, 365, x[1], x[2], x[3])$control)

    ts2 <- obs$arithmetic_mean
    f2 <- function(x) sd(ts2 - theil.wage(ts2, 365, x[1], x[2], x[3])$control)

    opt1 <- optim(par = par0, fn = f1,
                  method = "L-BFGS-B", lower = 0, upper = 1)
    cat("ID", id, "arithmetic_mean optimised\n")
    par0 <- opt1$par

    opt2 <- optim(par = opt1$par, fn = f2,
                  method = "L-BFGS-B", lower = 0, upper = 1)
    cat("ID", id, "first_max_value optimised\n")

    result <- rbind(result, c(id, opt1$par, opt2$par))

    counter <- counter + 1
}
names(result) <- res.names


##
#   Write result into the file which will be used by app
##
write.csv(result, paste0(data.dir, saved.file.name), row.names = FALSE)


Sys.time()
cat("Script executed at 2021-06-06 14:32:01")