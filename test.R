# https://github.com/statnet/EpiModel/blob/master/R/dcm.R
# https://github.com/statnet/EpiModel/blob/master/R/dcm.mods.R
# https://github.com/statnet/EpiModelResearch/blob/master/shiny/epidcmDG/ui.R
# *******https://github.com/statnet/EpiModelResearch/blob/master/shiny/epidcmDG/server.R
# http://desolve.r-forge.r-project.org/
# https://www.r-bloggers.com/sir-model-of-epidemics/
# http://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html

library(deSolve)
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


params <- list(

        # Input parameters
        N.tot <- 20000,
        prop.high <- 0.02,
        prop.low <- 1 - prop.high,
        c.mean <- 20 / 50,
        c.low <- 10 / 50,
        rho.high <- 0.5,
        rho.low <- 0.5,
        brate <- 0.02 / 50,
        muS.high <- 0.01 / 50,
        muS.low <- 0.01 / 50,
        muI.high <- 0.02 / 50,
        muI.low <- 0.02 / 50,
        Q = 0.75
        )

t0 <- c(
    S.high = (prop.high)*(N.tot) - 1,
    I.high = 1,
    S.low = (prop.low)*(N.tot) - 1,
    I.low = 1
)

dt <- seq(1, 100, 0.02)

df1 <- data.frame(ode(y=t0, times=dt, func=Qmod, parms=params, method='rk4'))
df2 <- data.frame(ode(y=t0, times=dt, func=Qmod, parms=params, method='rk4'))
df3 <- data.frame(ode(y=t0, times=dt, func=Qmod, parms=params, method='rk4'))

head(df1)
plot(df1$time, df1$prev, type='l', col = "red", lwd = 3)
lines(df2$time, df2$prev, type = 'l', col = "blue", lwd = 3)
lines(df3$time, df3$prev, type = 'l', col = "green", lwd = 3)
