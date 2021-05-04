library(data.table)
library(dplyr)

# Directories where new tables will be stored
dir0 <- "data/parted/"
dir1 <- paste0(dir0, "with_ids")
dir2 <- paste0(dir0, "by_codes")

# If they are not exist then create them
if (!dir.exists(dir0)) dir.create(dir0)
if (!dir.exists(dir1)) dir.create(dir1)
if (!dir.exists(dir2)) dir.create(dir2)


# Read target table
cols_classes <- c(
    "state_code" = "factor",
    "county_code" = "factor",
    "site_num" = "factor",
    "parameter_code" = "factor",
    "date_local" = "Date",
    "method_code" = "factor",
    "date_of_last_change" = "Date"
)
DT <- data.table(read.csv('data/epa_co_daily_summary.csv',
                          stringsAsFactors = TRUE,
                          colClasses = cols_classes))

##
#  First division
##

sites <- DT %>%
    select(
        state_code, state_name,
        county_code, county_name,
        site_num,
        city_name, address,
        cbsa_name, local_site_name,
        latitude, longitude, datum
    ) %>% unique

observations <- DT %>%
    select(
        date_local,
        poc, event_type,
        observation_count, observation_percent,
        arithmetic_mean, first_max_value, first_max_hour,
        aqi,
        date_of_last_change)

methods <- DT %>%
    select(
        method_code, method_name,
        parameter_code, parameter_name, units_of_measure,
        sample_duration, pollutant_standard
    ) %>% unique

# Assign site_id in tables sites and observations
site_id <- 1:nrow(sites)
sites$site_id <- site_id
observations$site_id <- numeric(length = nrow(DT))
for (id in site_id){
    site <- sites[id,]
    index <- which(
        DT$state_code==site$state_code & DT$county_code==site$county_code &
        DT$site_num==site$site_num & DT$datum==site$datum &
        DT$latitude==site$latitude & DT$longitude==site$longitude &
        DT$local_site_name==site$local_site_name & DT$address==site$address
    )
    observations[index,]$site_id <- id
    cat(id, '  ')
}

# Assign method_id in tables methods and observations
method_id <- 1:nrow(methods)
methods$method_id <- method_id
observations$method_id <- numeric(length = nrow(DT))
for (id in method_id){
    method <- methods[id,]
    index <- which(DT$method_code == method$method_code)
    observations[index,]$method_id <- id
    cat(id, '  ')
}

# Write got tables into files in subdir parted/with_ids
write.csv(sites, file = paste0(dir1,'/sites.csv'))
write.csv(methods, file = paste0(dir1,'/methods.csv'))
write.csv(observations, file = paste0(dir1,'/observations.csv'))


##
# Second division
##

# First confuse fix
cond1 <- DT$state_code==44 & DT$county_code==7 &
    DT$site_num==1010 & DT$datum=="NAD83"
new_latitude <- DT$latitude
new_latitude[cond1] <- 41.841573
new_longitude <- DT$longitude
new_longitude[cond1] <- -71.360770

# Second confuse fix
cond2 <- DT$state_code==47 & DT$county_code==157 &
    DT$site_num==24 & DT$datum=="WGS84"
new_local_site_name <- DT$local_site_name
new_local_site_name[cond2] <- "Alabama Ave. Station"

# Third confuse fix
cond3 <- DT$state_code==12 & DT$county_code==57 &
    DT$site_num==3002 & DT$datum=="WGS84"
new_address <- DT$address
new_address[cond3] <- "1167 N Dover Road Valrico FL 33527"

# fourth confuse fix
cond4 <- DT$state_code==29 & DT$county_code==510 &
    DT$site_num==85 & DT$datum=="NAD83"
new_latitude[cond4] <- 38.656498
new_longitude[cond4] <- -90.198646

# fifth confuse fix
cond5 <- DT$state_code==12 & DT$county_code==86 &
    DT$site_num==34 & DT$datum=="NAD83"
new_longitude[cond5] <- -80.399722
new_local_site_name[cond5] <- "KENDALL"
new_address[cond5] <- "9015 SW 127th Ave Miami FL 33186"

# Set up fixes in loaded data-set
DT$latitude <- new_latitude
DT$longitude <- new_longitude
DT$local_site_name <- new_local_site_name
DT$address <- new_address

# Write sub-tables into R environment
sites <- DT %>%
    select(state_code, state_name,
           county_code, county_name,
           site_num,
           city_name, address,
           cbsa_name, local_site_name,
           latitude, longitude, datum
           ) %>%
    unique

observations <- DT %>%
    select(date_local,
           poc, event_type,
           observation_count, observation_percent,
           arithmetic_mean, first_max_value, first_max_hour,
           aqi,
           date_of_last_change,
           state_code, county_code, site_num, datum,
           method_code)

methods <- DT %>%
    select(method_code, method_name,
           parameter_code, parameter_name, units_of_measure,
           sample_duration, pollutant_standard) %>%
    unique


# Write got tables into files in subdir parted/by_codes
write.csv(sites, file = paste0(dir2,'/sites.csv'))
write.csv(methods, file = paste0(dir2,'/methods.csv'))
write.csv(observations, file = paste0(dir2,'/observations.csv'))


#Sys.time()
cat("Script was run at",
    "2021-05-04 08:23:06 EEST")

rm(DT,
   method, methods,
   observations,
   site, sites,
   cols_classes,
   dir0, dir1, dir2,
   id, index,
   method_id, site_id,
   cond1, cond2, cond3, cond4, cond5,
   new_latitude, new_longitude, new_local_site_name, new_address)

detach("package:data.table", unload=TRUE)
detach("package:dplyr", unload=TRUE)

