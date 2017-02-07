library(shiny)
library(shinydashboard)
library(deSolve)

shinyServer(function(input, output, session) {

    # Params values
    params <- reactive({
        list(
        # Input parameters
        c.mean = input$c.mean / 50,
        c.low = input$c.low / 50,
        rho.high = input$rho.high,
        rho.low = input$rho.low,
        brate = input$brate / 50,
        muS.high = input$muS.high / 50,
        muS.low = input$muS.low / 50,
        muI.high = input$muI.high / 50,
        muI.low = input$muI.low / 50,
        Q = input$Q
        )
    })
    
    t0 <- as.numeric(
        S.high = (input$prop.high)*(input$N.tot) - 1,
        I.high = 1,
        S.low = (input$prop.low)*(input$N.tot) - 1,
        I.low = 1
    )
    
    dt <- seq(1, 100, 0.02)
    
    
    
    ## New module
    Qmod <- function(t, t0, parms) {
        with(as.list(c(t0, parms)), {
            
            ## Dynamic Calculations ##
            
            # Popsize
            N.high <- S.high + I.high
            N.low <- S.low + I.low
            N <- N.high + N.low
            prev <- (I.high + I.low)/N
            
            # Contact rates
            c.high <- (c.mean*N - c.low*N.low)/N.high
            
            # mixing matrix calculations based on Q
            g.hh <- ((c.high*N.high) + (Q*c.low*N.low)) / ((c.high*N.high) + (c.low*N.low))
            g.lh <- 1 - g.hh
            g.hl <- (1 - g.hh) * ((c.high*N.high) / (c.low*N.low))
            g.ll <- 1 - g.hl
            
            # prob that p is infected
            p.high <- (g.hh*I.high/N.high) + (g.lh*I.low/N.low)
            p.low <- (g.ll*I.low/N.low) + (g.hl*I.high/N.high)
            
            # lambda - force of infection
            lambda.high <- rho.high*c.high*p.high
            lambda.low <- rho.low*c.low*p.low
            
            ## Differential Equations ##
            dS.high <- brate*prop.high*N.tot - lambda.high*S.high - muS.high*S.high 
            dI.high <- lambda.high*S.high - muI.high*I.high
            
            dS.low <- brate*prop.low*N.tot - lambda.low*S.low - muS.low*S.low
            dI.low <- lambda.low*S.low - muI.low*I.low
            
            
            ## Output ##
            list(c(dS.high, dI.high,
                   dS.low, dI.low),
                 N = N,
                 prev = prev)
            
        })
    }
    
    # Reactive parameters
    ## ------------------------------------------------------------------------
    
    scenarioA <- data.frame(ode(y = t0, times = dt, func = Qmod, parms = params, method = "rk4"))

    output$a_Plot <- renderPlot({
        par(mfrow = c(1,1), mgp = c(2,1,0))
        plot(datA = scenarioA, y = scenarioA$prev, type = 'l', xlab = "Time",
             ylab = "Prevalence", lwd = 2, ylim = c(0, 1))
         title("Prevalence of HIV")
        })
    })

