#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(wesanderson)
library(deSolve)
source("fx.R")

shinyServer(function(input, output, session) {

    scenarioA <- reactive({
         runScenario(input$N.tot, input$prop.high, input$prop.low, input$c.mean, input$c.low, input$rho.high, input$rho.low,
                     input$brate, input$muS.high, input$muS.low, input$muI.high, input$muI.low, input$Q)
    })
     
     output$a_Plot <- renderPlot({
        par(mfrow = c(1,1), mgp = c(2,1,0))
        plotScenario(datA = scenarioA())
    })
})
        #     output$table <- renderTable({
#         tabScenario(datA = scenarioA(), datB = scenarioB(), quantile = input$quantile)
#     })
#     
#     output$tablecaption <- renderUI({
#         helpText("Epidemiological outcomes after", input$nyears,
#                  "years of simulations. Reported values are simulation means with a",
#                  paste0(input$quantile*100, "%"), "credible interval. Editing",
#                  "inputs in the left panel and/or plot options will update the",
#                  "table values.")
#     })

