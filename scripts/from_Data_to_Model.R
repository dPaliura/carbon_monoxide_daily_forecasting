# Libraries I use are here:
library(dplyr)
library(lubridate)
library(forecast)


# Some constants
data.dir <- "data/parted/with_ids/"
obsvs.fname <- "observations.csv"
sites.fname <- "sites.csv"
meths.fname <- "methods.csv"
save.dir <- "app/CO_level_forecast/data/"


# Load data
obsvs.df <- read.csv(
    file = paste0(data.dir, obsvs.fname),
    colClasses = c(
        date_local = "Date",
        poc = "factor",
        date_of_last_change = "Date",
        site_id = "factor",
        method_id = "factor"
    ),
    stringsAsFactors = TRUE
    )[,-1]

sites.df <- read.csv(
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


meths.df <- read.csv(
    file = paste0(data.dir, meths.fname),
    colClasses = c(
        method_code = "factor",
        parameter_code = "factor"
    ),
    stringsAsFactors = TRUE
)[,-1]


# Some special function
getObservationsBySiteID <- function(id, obs = obsvs.df){
    obs %>% filter(site_id==id) %>% arrange(date_local)
}


# Remove some sites placed far far away from continent (Hawaii, Puerto Rico and
# some site with broken coordinates - 0, 0)
sites <- filter (latitude > 22)

remained_ids <- NULL
new_obs <- NULL
NA_row <- rep(NA_character_, ncol(obsvs.df)); names(NA_row) <- names(obsvs.df)
for (id in sites.df$site_id){
    obs <- getObservationsBySiteID(id)

    # 1 check if site has observations during period longer than 3 years
    dates_range <- range(obs$date_local)
    ndays <- diff(dates_range)
    if (ndays < 365*3){
        cat(id, "less than 3 years period - removed\n")
        next
    }

    # 2 check if any negative arithmetic_mean present
    if (any(obs$arithmetic_mean < 0)){
        cat(id, "negative arithmetic_mean present - removed\n")
        next
    }

    # 3 check if dates gap greater than quarter of whole period
    unique_dates <- unique(obs$date_local)
    unique_dates_num <- length(unique_dates)
    if (unique_dates_num < ndays*3/4){
        cat(id, "gap greater than period quarter - removed\n")
        next
    }

    # 4 remove duplicates and fill gaps
    days_sequence <- seq.Date(dates_range[1], dates_range[2],"day")

    obs <- obs %>%
        group_by(date_local) %>%
        group_modify( function(x, y) x[1,] ) %>%
        ungroup

    gap_days <- setdiff(as.character(days_sequence),
                        as.character(unique_dates))
    NA_row["site_id"] <- id
    for (d in gap_days){
        NA_row["date_local"] <- d
        obs <- rbind(obs, NA_row)
    }

    new_obs <- rbind(new_obs, obs)
    remained_ids <- c(remained_ids, id)
    cat(id, "remained\n")
}


# Arrange filtered observations
new_obs <- new_obs %>% arrange(site_id, date_local)
# Select only valuable columns
new_obs <- new_obs %>% select(date_local,
                              arithmetic_mean, first_max_value,
                              site_id, method_id)
# Cast class to numeric at according columns
new_obs$arithmetic_mean <- as.numeric(new_obs$arithmetic_mean)
new_obs$first_max_value <- as.numeric(new_obs$first_max_value)

# Interpolate observations
for (id in remained_ids){
    index <- which(new_obs$site_id == id)

    v1 <- new_obs$arithmetic_mean[index]
    v2 <- new_obs$first_max_value[index]

    v1_interp <- na.interp(v1)
    v2_interp <- na.interp(v2)

    new_obs$arithmetic_mean[index] <- v1_interp
    new_obs$first_max_value[index] <- v2_interp
    cat(id, 'interpolated\n')
}
new_obs <- new_obs %>% mutate(interpolated = ifelse(is.na(method_id), T, F))

# Set only remained sites
new_sites <- sites.df[remained_ids,]

# Select only valuable columns in methods table
new_meths <- meths.df %>% select(method_id, method_name,
                                 units_of_measure, pollutant_standard)

# Write got tables
write.csv(new_obs, paste0(save.dir, obsvs.fname), row.names = FALSE)
write.csv(new_sites, paste0(save.dir, sites.fname), row.names = FALSE)
write.csv(new_meths, paste0(save.dir, meths.fname), row.names = FALSE)


##
# Clear environment
##
rm(data.dir, save.dir, obsvs.fname, sites.fname, meths.fname,
   obsvs.df, sites.df, meths.df,
   remained_ids, NA_row,
   obs,
   dates_range, ndays, unique_dates, unique_dates_num, days_sequence, gap_days,
   v1, v2, v1_interp,v2_interp, index,
   new_obs, new_sites, new_meths,
   id, d,
   getObservationsBySiteID)

detach("package:dplyr", unload = TRUE)
detach("package:fpp2", unload = TRUE)
detach("package:lubridate", unload = TRUE)
detach("package:forecast", unload = TRUE)
