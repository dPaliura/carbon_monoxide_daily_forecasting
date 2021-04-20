# install.packages('devtools')
# install.packages('httr')
# install.packages('zip')
library(devtools)
library(httr)
library(zip)

## install kaggler package from GitHub
## This package is not official Kaggle development
# When asked for updates, chosen '1 - All'
devtools::install_github("mkearney/kaggler")

# Sourcing package for Kaggle API usage
# Reference: https://github.com/mkearney/kaggler
library(kaggler)

# Authorizing
is.authorized <- FALSE
while (!is.authorized) {
    tryCatch(
        {
            cat('Authentication to Kaggle API. Use values from file',
            'kaggle.json obtained by Create New API Token.\n')

            kgl_auth(
                username = readline('Enter username: '),
                key = readline('Enter key: ')
            )

            is.authorized <- TRUE
        },
        error = function(e){
            cat('Something went wrong. Error message is:\n')
            print(e)
            cat('Try again.\n')
        }
    )
}


# Making GET request using Kaggle API URL to obtain file URL
r <- httr::GET('https://www.kaggle.com/api/v1/datasets/download/epa/carbon-monoxide/epa_co_daily_summary.csv', kgl_auth())

# Now downloading the file using obtained response (expected status 200)
download.file(r$url, 'data/kaggle.zip', method = 'wget')
# Yeah, it's kinda crutch, but main purpose - to obtain file with data through
# the code if this code exists

# Unzipping downloaded file
unzip(zipfile = 'data/kaggle.zip', exdir = 'data')

# Sys.time()
cat("Script was run at",
    "2021-04-20 13:47:17 EEST")


# Clearing environment variables
rm(is.authorized, r)
# Disconnecting packages
detach('package:devtools', unload = TRUE)
detach('package:kaggler', unload = TRUE)
detach('package:zip', unload = TRUE)
# Done
