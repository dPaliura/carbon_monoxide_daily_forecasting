library(dplyr)
library(MLmetrics)


##
#   Read data to work with
##
data.dir <- "app/CO_level_forecast/data/"
obsvs.fname <- "observations.csv"
optpars.fname <- "optimal_TW_parameters.csv"

observations <- read.csv(
    file = paste0(data.dir, obsvs.fname))
cat("Observations data set loaded\n")
TWpars <- read.csv(
    file = paste0(data.dir, optpars.fname))
cat("Optimal parameters table loaded\n")

##
#   Function to select all observations by specified site_id
##
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

# Function to predict Theil-Wage model
theil.wage.predict <- function(tw.mod, horizon=1){
    tw.mod$a + horizon*tw.mod$b + tw.mod$theta[(horizon-1)%%tw.mod$period+1]
}


##
#   Evaluate accuracy
##
all_ids <- sort(unique(observations$site_id))
counter <- 1
whole <- length(all_ids)
res <- NULL
colnames <- c("rmse_am_train", "rmse_fmv_train",
              "rmse_am_test", "rmse_fmv_test",
              "rmse_am_test_nointerp", "rmse_fmv_test_nointerp")
for (id in all_ids){
    cat(counter, "/", whole, "\n", sep="")
    obs <- getObservationsBySiteID(id)
    optpars <- TWpars[TWpars$site_id == id , ]

    train <- head(obs, nrow(obs)-365)
    test <- tail(obs, 365)

    test_nointerp_indx <- which(!test$interpolated)

    train1 <- train$arithmetic_mean
    test1 <- test$arithmetic_mean
    TWmod1 <- theil.wage(train1, 365,
                        lev.par = optpars$arithmean_level_par,
                        seas.par = optpars$arithmean_season_par,
                        trend.par = optpars$arithmean_trend_par)
    pred1 <- theil.wage.predict(TWmod1, 1:365)
    err1 <- test1 - pred1

    train2 <- train$first_max_value
    test2 <- test$first_max_value
    TWmod2 <- theil.wage(train2, 365,
                         lev.par = optpars$firstmax_level_par,
                         seas.par = optpars$firstmax_season_par,
                         trend.par = optpars$firstmax_trend_par)
    pred2 <- theil.wage.predict(TWmod2, 1:365)
    err2 <- test2 - pred2

    res <- rbind(res,
                 c(RMSE(TWmod1$control, train1), RMSE(TWmod2$control, train2),
                   RMSE(pred1, test1), RMSE(pred2, test2),
                   RMSE(pred1[test_nointerp_indx], test1[test_nointerp_indx]),
                   RMSE(pred2[test_nointerp_indx], test2[test_nointerp_indx])
                   ))

    counter <- counter + 1
}
colnames(res) <- colnames


##
#   Summarize result
##
summary(res)

Sys.time()
#Script executed at 2021-06-06 14:32:01
