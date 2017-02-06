library(shiny)
library(shinydashboard)
library(EpiModel)
library(deSolve)
source("fx.R")

# https://github.com/statnet/EpiModel/blob/master/R/dcm.R
# https://github.com/statnet/EpiModel/blob/master/R/dcm.mods.R
# https://github.com/statnet/EpiModelResearch/blob/master/shiny/epidcmDG/ui.R
# *******https://github.com/statnet/EpiModelResearch/blob/master/shiny/epidcmDG/server.R
# http://desolve.r-forge.r-project.org/
# https://www.r-bloggers.com/sir-model-of-epidemics/
# http://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html




shinyServer(function(input, output, session) {

    # Reactive parameters    
    parms <- reactive({list(
        
        # Input parameters
        N.tot <- input$N.tot, 
        prop.high <- input$prop.high,
        prop.low <- 1 - input$prop.high,
        c.mean = input$c.mean,
        c.low = input$c.low,
        rho.high = input$rho.high,
        rho.low = input$rho.low,
        brate = input$brate,
        muS.high = input$muS.high,
        muS.low = input$muS.low,
        muI.high = input$muI.high,
        muI.low = input$muI.low,
        Q = input$Q,
  
        # Initial starting values
        S.high = N.tot*prop.high - 1,
        I.high = 1,
        S.low = N.tot*prop.low - 1,
        I.low = 1
    )

    dt <- seq(1, 100, 0.02)
    
    scenarioA <- Qmod(t = dt, parms = parms, t0 = t0)
    #df1 <- data.frame(ode(y=t0, times=dt, func=Qmod, parms=params, method='rk4'))
    output$a_Plot <- renderPlot({
        par(mfrow = c(1,1), mgp = c(2,1,0))
        plot(datA = scenarioA, y = "prev", xlab = "Time",
             ylab = "Prevalence")
         title("Prevalence of HIV")
        })
    })
})