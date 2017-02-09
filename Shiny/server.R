library(shiny)
library(shinydashboard)
library(deSolve)
suppressMessages(library("EpiModel"))
source("fx.R")

shinyServer(function(input, output) {
    
    ## Main reactive functions
    param <- reactive({
        param.dcm(c.mean = input$c.mean / 50,
                  c.low = input$c.low / 50,
                  rho.high = input$rho.high,
                  rho.low = input$rho.low,
                  b.rate = input$b.rate / 50,
                  muS.high = input$muS.high / 50,
                  muS.low = input$muS.low / 50,
                  muI.high = input$muI.high / 50,
                  muI.low = input$muI.low / 50,
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
        control.dcm(type = "SI",
                    nsteps = 100 / 0.02,
                    dt = 0.02,
                    verbose = FALSE,
                    odemethod = "rk4",
                    new.mod = Qmod)
    })
    
    mod <- #reactive({
        #input$runMod
        isolate(dcm(param(), init(), control()))
    #})
    ## ------------------------------------------------------------------------
    # Output plot

    output$a_Plot <- renderPlot({
        df <- as.data.frame(mod())
        par(mfrow = c(1,1))
        plot(df, y = "prev", type = 'l', xlab = "Time",
             ylab = "Prevalence", lwd = 2, ylim = c(0, 1), main = "HIV Prevalence")
        })
    })

