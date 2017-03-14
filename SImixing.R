

Qmod <- function(t, t0, parms) {
    with(as.list(c(t0, parms)), {
        
        # Dynamic calcs
        N.g1 <- S.g1 + I.g1
        N.g2 <- S.g2 + I.g2
        N <- N.g1 + N.g2
        prev <- (I.g1 + I.g2)/N
        prev.g1 <- I.g1 / N.g1
        prev.g2 <- I.g2 / N.g2
        
        # mixing matrix calculations based on Q
        g.11 <- ((c.g1*N.g1) + (Q*c.g2*N.g2)) / ((c.g1*N.g1) + (c.g2*N.g2))
        g.21 <- 1 - g.11
        g.12 <- (1 - g.11) * ((c.g1*N.g1) / (c.g2*N.g2))
        g.22 <- 1 - g.12
        
        # lambda - force of infection
        lambda.g1 <-  rho.g1 * c.g1 * (g.11*I.g1/N.g1) + rho.g2 * c.g1 * (g.21*I.g2/N.g2)
        lambda.g2 <-  rho.g2 * c.g2 * (g.22*I.g2/N.g2) + rho.g1 * c.g2 * (g.12*I.g1/N.g1)
        
        
        ## Differential Equations ##
        dS.g1 <- b.rate*N.g1 - lambda.g1*S.g1 - muS.g1*S.g1
        dI.g1 <- lambda.g1*S.g1 - muI.g1*I.g1
        
        dS.g2 <- b.rate*N.g2 - lambda.g2*S.g2 - muS.g2*S.g2
        dI.g2 <- lambda.g2*S.g2 - muI.g2*I.g2
        
        
        ## Output ##
        list(c(dS.g1, dI.g1,
               dS.g2, dI.g2,
               incid.g1.flow = lambda.g1*S.g1,
               incid.g2.flow = lambda.g2*S.g2),
             N = N,
             prev = prev,
             prev.g1 = prev.g1,
             prev.g2 = prev.g2)
        
    })
}

param <- param.dcm(c.g1 = 3,
          c.g2 = 3,
          rho.g1 = 0.05,
          rho.g2 = 0.05,
          b.rate = 0.01,
          muS.g1 = 0.01,
          muS.g2 = 0.01,
          muI.g1 = 0.01,
          muI.g2 = 0.01,
          Q = 0)


Num.g1 <- 1000
Num.g2 <- 1000
prevalence.g1 <- 0.01
prevalence.g2 <- 0.01
S.g1 <- Num.g1 * (1 - (prevalence.g1))
I.g1 <- Num.g1 * (prevalence.g1)
S.g2 <- Num.g2 * (1 - (prevalence.g2))
I.g2 <- Num.g1 * (prevalence.g2)

init <- init.dcm(S.g1 = S.g1,
         I.g1 = I.g1,
         S.g2 = S.g2,
         I.g2 = I.g2,
         incid.g1.flow = 0,
         incid.g2.flow = 0)

control <- control.dcm(nsteps = 500,
            dt = 0.1,
            verbose = FALSE,
            odemethod = "rk4",
            new.mod = Qmod)

mod <- dcm(param, init, control)

plot(mod, y = "prev")
plot(mod, y = "N")
plot(mod, y = c("incid.g1.flow", "incid.g2.flow"), lty = 1:2)

df <- as.data.frame(mod)
head(df)
plot(df$prev)
