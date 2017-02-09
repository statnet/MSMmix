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
        
        # Birth rates
        bratehigh <- brate*prop.high
        bratelow <- brate*prop.low
        
        ## Differential Equations ##
        dS.high <- bratehigh*N.tot - lambda.high*S.high - muS.high*S.high 
        dI.high <- lambda.high*S.high - muI.high*I.high
        
        dS.low <- bratelow*N.tot - lambda.low*S.low - muS.low*S.low
        dI.low <- lambda.low*S.low - muI.low*I.low
        
        
        ## Output ##
        list(c(dS.high, dI.high,
               dS.low, dI.low),
             N = N,
             prev = prev)
        
    })
}

s