Qmod <- function(t, parms) {
    with(as.list(c(t, parms)), {
        
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
        N[dt] <- N.high + N.low
        prev[dt] <- (I.high + I.low)/N
        
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
        
        ## Differential Equations ##
        dS.high <- brate*prop.high*N.tot - lambda.high*S.high - muS.high*S.high 
        dI.high <- lambda.high*S.high - muI.high*I.high
        
        dS.low <- brate*prop.low*N.tot - lambda.low*S.low - muS.low*S.low
        dI.low <- lambda.low*S.low[dt] - muI.low*I.low
        
        
        ## Output ##
        list(c(dS.high, dI.high,
               dS.low, dI.low),
             N = N,
             prev = prev)
        
    })
}
