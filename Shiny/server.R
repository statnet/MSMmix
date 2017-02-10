library(shiny)
library(shinydashboard)
library(deSolve)
suppressMessages(library("EpiModel"))
source("fx.R")

shinyServer(function(input, output) {

    ## Main reactive functions
    param <- reactive({
        param.dcm(c.mean = input$c.mean,
                  c.g1 = input$c.g1,
                  rho.g1 = input$rho.g1,
                  rho.g2 = input$rho.g2,
                  b.rate = input$b.rate,
                  muS.g1 = input$muS.g1,
                  muS.g2 = input$muS.g2,
                  muI.g1 = input$muI.g1,
                  muI.g2 = input$muI.g2,
                  Q = input$Q)
    })

    init <- reactive({
        init.dcm(S.g1 = input$S.g1,
                 I.g1 = input$I.g1,
                 S.g2 = input$S.g2,
                 I.g2 = input$I.g2,
                 incid.g1 = 0,
                 incid.g2 = 0)
    })

    control <- reactive({
        control.dcm(nsteps = input$nsteps,
                    dt = input$dt,
                    verbose = FALSE,
                    odemethod = "rk4",
                    new.mod = Qmod)
    })

    mod <- reactive({
      dcm(param(), init(), control())
    })

    # Output plot
    output$a_Plot <- renderPlot({
        par(mfrow = c(1,2), mar = c(3,3,1,0), mgp = c(2,1,0), 
            cex = 1.5)
        plot(mod(), y = c("prev.g1", "prev.g2"), xlab = "Time", 
             lwd = 3, lty = c(1,2),
             ylab = "Prevalence", leg = "full", main = "Prevalence Plot")
        plot(mod(), y = c("incid.g1", "incid.g2"), xlab = "Time", 
             lwd = 3, lty = c(1,2),
             ylab = "Prevalence", leg = "full", main = "Incidence Plot")
        })
    })

