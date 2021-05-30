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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$tsPlot <- renderPlot({
        site_id <- classify(input$latitude, input$longitude)
        obs <- getObservationsBySiteID(site_id)

        modl1 <- ets(obs$arithmetic_mean)
        modl2 <- ets(obs$first_max_value)

        autoplot(forecast(modl1, h=400, PI = F))
        autoplot(forecast(modl2, h=400, PI = F))
    })

})
