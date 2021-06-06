library(shiny)


shinyUI(fluidPage(
    titlePanel("EPA CO Forecasting"),
    fluidRow(
        column(
            width = 6,
            sliderInput("latitude",
                        "Latitude:",
                        min = 24,
                        max = 50,
                        value = 31,
                        step = 0.0001)
            ),
        column(
            width = 6,
            sliderInput("longitude",
                        "Longitude:",
                        min = -125,
                        max = -68,
                        value = -105,
                        step = 0.0001)
        )
    ),

    fluidRow(
        column(width = 4),
        column(
            width = 6,
            sliderInput("horizon",
                        "Forecast horizon:",
                        min = 1,
                        max = 730,
                        value = 365,
                        step = 1),
        )
    ),

    fluidRow(
        column(width = 5),
        column(
            width = 4,
            actionButton("tsSubmit",
                         "Forecast")
        )
    ),

    textOutput(outputId = "site_info"),

    plotOutput("tsPreview"),

    plotOutput("forecastPlot")
))
