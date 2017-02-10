library(shiny)
library(shinydashboard)
library(deSolve)
suppressMessages(library("EpiModel"))
source("fx.R")

shinyServer(function(input, output) {

    ## Main reactive functions
    param <- reactive({
        param.dcm(c.mean = input$c.mean,
                  c.low = input$c.low,
                  rho.high = input$rho.high,
                  rho.low = input$rho.low,
                  b.rate = input$b.rate,
                  muS.high = input$muS.high,
                  muS.low = input$muS.low,
                  muI.high = input$muI.high,
                  muI.low = input$muI.low,
                  Q = input$Q
        )
    })

    init <- reactive({
        init.dcm(S.high = input$S.high,
                 I.high = input$I.high,
                 S.low = input$S.low,
                 I.low = input$I.low)
    })

    control <- reactive({
        control.dcm(nsteps = 100,
                    dt = 0.02,
                    verbose = FALSE,
                    odemethod = "rk4",
                    new.mod = Qmod)
    })

    mod <- reactive({
      isolate(dcm(param(), init(), control()))
    })

    # Output plot
    output$a_Plot <- renderPlot({
        par(mfrow = c(1,1))
        plot(mod(), y = "prev", xlab = "Time",
             ylab = "Prevalence")
        })
    })

