library(shiny)
library(shinydashboard)
library(deSolve)
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
                  Q = input$Q)
    })
    init <- reactive({
            init.dcm(prop.high = input$prop.high,
                     prop.low = 1 - input$prop.high,
                     N.tot = input$N.tot,
                     S.high = prop.high*N.tot - 1,
                     I.high = 1,
                     S.low = prop.low*N.tot - 1,
                     I.low = 1
                      )
    })
    control <- reactive({
        control.dcm(type = "SI",
                    nsteps = 100 / 0.02,
                    dt = 0.02,
                    verbose = FALSE,
                    odemethod = "rk4",
                    new.mod = Qmod)
    })
    mod <- reactive({
        input$runMod
        isolate(dcm(param(), init(), control()))
    })
    ## ------------------------------------------------------------------------
    # Output plot
    
    df <- as.data.frame(mod())

    output$a_Plot <- renderPlot({
        par(mfrow = c(1,1), mgp = c(2,1,0))
        plot(df, y = df$prev, type = 'l', xlab = "Time",
             ylab = "Prevalence", lwd = 2, ylim = c(0, 1), main = "Prevalence of HIV")
        })
    })

