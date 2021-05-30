#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("EPA CO Forecasting"),

    sliderInput("latitude",
                "Latitude:",
                min = 24,
                max = 50,
                value = 37,
                step = 0.0001),
    sliderInput("longitude",
                "Longitude:",
                min = -125,
                max = -68,
                value = -95,
                step = 0.0001),



    # Show a plot of the generated distribution
    plotOutput("tsPlot")
))
