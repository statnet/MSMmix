library(shiny)
library(shinydashboard)
library(deSolve)
suppressMessages(library("EpiModel"))

Qmod <- function(t, t0, parms) {
    with(as.list(c(t0, parms)), {
        
        ## Dynamic Calculations ##
        
        # Popsize
        N.high <- S.high + I.high
        N.low <- S.low + I.low
        N <- N.high + N.low
        prev <- (I.high + I.low)/N
        
        # Contact rates
        c.high <- abs(c.mean*N - c.low*N.low)/N.high
        
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
        
        # Birth rates
        
        ## Differential Equations ##
        dS.high <- b.rate*N.high - lambda.high*S.high - muS.high*S.high 
        dI.high <- lambda.high*S.high - muI.high*I.high
        
        dS.low <- b.rate*N.low - lambda.low*S.low - muS.low*S.low
        dI.low <- lambda.low*S.low - muI.low*I.low
        
        
        ## Output ##
        list(c(dS.high, dI.high,
               dS.low, dI.low),
             N = N,
             prev = prev)
        
    })
}

param <- param.dcm(c.mean = 10 / 50,
              c.low = 10 / 50,
              rho.high = 0.2,
              rho.low = 0.2,
              b.rate = 0.02 / 50,
              muS.high = 0.01 / 50,
              muS.low = 0.01 / 50,
              muI.high = 0.02 / 50,
              muI.low = 0.02 / 50,
              Q = 0.50
    )
init <- init.dcm(S.high = 999,
             I.high = 1,
             S.low = 999,
             I.low = 1)

control <- control.dcm(type = "SI",
                nsteps = 100 / 0.02,
                dt = 0.02,
                verbose = FALSE,
                odemethod = "rk4",
                new.mod = Qmod)

df <- as.data.frame(dcm(param, init, control))

plot(df, y = "prev", type = 'l', xlab = "Time",
     ylab = "Prevalence", lwd = 2, ylim = c(0, 1), main = "HIV Prevalence")