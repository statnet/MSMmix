library(shiny)
library(shinydashboard)
library(deSolve)
suppressMessages(library("EpiModel"))
source("fx.R")
library(plotly)

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

    output$plot1 <- renderPlotly({
        df <- as.data.frame(mod())
        p1 <- plot_ly(df, x = ~time, y = ~prev.g1, name = "Group 1", type = 'scatter', mode = 'lines', 
                     line = list(width = 2)) %>%
            add_trace(y = ~prev.g2, name = 'Group 2', mode = 'lines', line = list(dash = "dash", width = 2)) %>%
            layout(xaxis = list(title = "Time"),
                   yaxis = list (title = "Proportion"))
        p1
    })
    output$plot2 <- renderPlotly({
        df <- as.data.frame(mod())
        p2 <- plot_ly(df, x = ~time, y = ~incid.g1, name = "Group 1", type = 'scatter', mode = 'lines', 
                      line = list(width = 2)) %>%
            add_trace(y = ~incid.g2, name = 'Group 2', mode = 'lines', line = list(dash = "dash", width = 2)) %>%
            layout(xaxis = list(title = "Time"),
                   yaxis = list (title = "Proportion"))
        p2
    })

})

