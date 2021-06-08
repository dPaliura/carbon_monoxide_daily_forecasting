#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("model.R", echo = FALSE)

shinyServer(function(input, output) {
    siteID <- reactive(classify(input$latitude, input$longitude))

    obs <- reactive(getObservationsBySiteID( siteID() ))

    output$site_info <- renderText({
        site_info <- sites %>% filter(site_id == siteID() ) %>%
            select(-county_code, -site_num, -datum, -site_id)

        paste(
            gsub(pattern = "_", " ", names(site_info)),
            as.character(site_info[1,]),
            collapse = ", \n",
            sep = " - "
        )
    })

    output$tsPreview <- renderPlot({
        obs <- obs()
        ylim <- range(obs$arithmetic_mean, obs$first_max_value)

        plot(obs$date_local, obs$arithmetic_mean,
             type = "l", ylim = ylim,
             bg = "lightgrey",
             main = "Preview of CO summary levels from nearest monitoring site",
             xlab = "Date", ylab = "parts per million")
        lines(obs$date_local, obs$first_max_value, col="red")

        legend("top",
               legend = c("Average through a day", "Maximum through a day"),
               col = c("black", "red"),
               pch = NA, lwd = 1)
    })

    tw.models <- eventReactive(
        eventExpr = input$tsSubmit,
        valueExpr = {
            obs <- obs()
            id <- siteID()

            pars <- opt.TW.pars[opt.TW.pars$site_id == id, -1]

            arithmetic_mean.TW <- theil.wage(
                                    ts = obs$arithmetic_mean, period = 365,
                                    lev.par = pars$arithmean_level_par,
                                    seas.par = pars$arithmean_season_par,
                                    trend.par = pars$arithmean_trend_par)
            first_max_value.TW <- theil.wage(
                                    ts = obs$first_max_value, period = 365,
                                    lev.par = pars$firstmax_level_par,
                                    seas.par = pars$firstmax_season_par,
                                    trend.par = pars$firstmax_trend_par)

            list(
                 arithmetic_mean = arithmetic_mean.TW,
                 first_max_value = first_max_value.TW,
                 data = data.frame(
                     date_local = obs$date_local,
                     arithmetic_mean = obs$arithmetic_mean,
                     first_max_value = obs$first_max_value
                 ))
        }
    )

    output$forecastPlot <- renderPlot({
        models <- tw.models()
        tw.AM <- models$arithmetic_mean
        tw.FMV <- models$first_max_value
        obs <- models$data
        hor <- input$horizon

        hor.seq <- 1:hor
        pred.AM  <- theil.wage.predict(tw.AM, hor.seq)
        pred.FMV <- theil.wage.predict(tw.FMV, hor.seq)

        sde.AM  <- sd(obs$arithmetic_mean - tw.AM$control)
        sde.FMV <- sd(obs$first_max_value - tw.FMV$control)

        subset.size <- 730
        obs.subset <- tail(obs, subset.size)

        dates <- obs.subset$date_local
        dates.hor <- tail(dates, 1) + hor.seq

        xlim <- c(dates[1], hor + tail(obs.subset$date_local, 1))
        ylim <- range(obs.subset$arithmetic_mean, obs.subset$first_max_value,
                      pred.AM, pred.FMV)


        plot(x = c(dates, dates.hor), y = rep(NA, subset.size + hor),
             xlim = xlim, ylim = ylim,
             bg = "lightgrey",
             main = "Forecasts of CO daily average and max values",
             xlab = "Date",
             ylab = "parts per million")

        lines(dates, obs.subset$arithmetic_mean)
        lines(dates.hor, pred.AM, col="blue")

        lines(dates, obs.subset$first_max_value, col="red")
        lines(dates.hor, pred.FMV, col="purple")

        legend("top",
               legend = c(
                   "Arithmetic mean known", "Arithmetic mean predicted",
                   "First max value known", "First max value predicted",
                   paste("Arithmetic mean RMSE is", sde.AM),
                   paste("First max value RMSE is", sde.FMV)
               ),
               col = c("black", "blue", "red", "purple", NA, NA),
               pch = NA, lwd = c(1, 1, 1, 1, NA, NA),
               ncol = 3)
    })
})
