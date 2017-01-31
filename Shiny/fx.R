library(deSolve)

Qmod <- function(dt, t0, parms) {
    with(as.list(c(dt, t0, parms)), {
        
        ## Dynamic Calculations ##
        
        N.high <- c(0)
        N.low <- c(0)
        N <- c(0)
        prev <- c(0)
        p.high <- c(0)
        p.low <- c(0)
        lambda.high <- c(0)
        lambda.low <- c(0)
        dS.high <- c(0)
        dS.low <- c(0)
        dI.high <- c(0)
        dI.low <- c(0)
        
        # Popsize
        N.high[dt] <- S.high[dt] + I.high[dt]
        N.low[dt] <- S.low[dt] + I.low[dt]
        N[dt] <- N.high[dt] + N.low[dt]
        prev[dt] <- (I.high[dt] + I.low[dt])/N[dt]
        
        # Contact rates
        c.high <- (c.mean*N.tot - c.low*N.low[1])/N.high[1]
        
        # mixing matrix calculations based on Q
        g.hh <- ((c.high*N.high[1]) + (Q*c.low*N.low[1])) / ((c.high*N.high[1]) + (c.low*N.low[1]))
        g.lh <- 1 - g.hh
        g.hl <- (1 - g.hh) * ((c.high*N.high[1]) / (c.low*N.low[1]))
        g.ll <- 1 - g.hl
        
        # prob that p is infected
        p.high[dt] <- (g.hh*I.high/N.high[dt]) + (g.lh*I.low/N.low[dt])
        p.low[dt] <- (g.ll*I.low/N.low[dt]) + (g.hl*I.high/N.high[dt])
        
        # lambda - force of infection
        lambda.high[dt] <- rho.high * c.high * p.high
        lambda.low[dt] <- rho.low * c.low * p.low
        
        # Birth rate - beta
        beta.high <- brate*prop.high*N.tot
        beta.low <- brate*prop.low*N.tot
        
        ## Differential Equations ##
        dS.high[dt] <- beta.high - lambda.high[dt]*S.high[dt] - muS.high*S.high[dt] 
        dI.high[dt] <- lambda.high*S.high[dt] - muI.high*I.high[dt]
        
        dS.low[dt] <- beta.low - lambda.low*S.low[dt] - muS.low*S.low[dt]
        dI.low[dt] <- lambda.low*S.low[dt] - muI.low*I.low[dt]
        
        
        ## Output ##
        list(c(dS.high, dI.high,
               dS.low, dI.low),
             N = N,
             prev = prev)
        
    })
}

parms <- list(
    N.tot <- 20000000, 
    prop.high <- 0.02,
    prop.low <- 0.98,
    c.mean = 2,
    c.low = 1.4,
    rho.high = 0.75,
    rho.low = 0.50,
    brate = .02,
    muS.high = 0.01,
    muS.low = 0.01,
    muI.high = 0.05,
    muI.low = 0.05,
    Q = 0
)

t0 <- c(
    S.high = N.tot*prop.high - 1,
    I.high = 1,
    S.low = N.tot*prop.low - 1,
    I.low = 1
)
dt <- seq(1, 100, 0.02)
Qmod(parms = parms, dt = dt, t0 = t0)