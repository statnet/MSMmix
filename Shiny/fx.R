
Qmod <- function(t, t0, parms) {
    with(as.list(c(t0, parms)), {

        # Dynamic calcs
        N.g1 <- S.g1 + I.g1
        N.g2 <- S.g2 + I.g2
        N <- N.g1 + N.g2
        prev <- (I.g1 + I.g2)/N
        prev.g1 <- I.g1 / N.g1
        prev.g2 <- I.g2 / N.g2

        # Contact rates, balanced
        c.g2 <- abs(c.mean*N - c.g1*N.g1)/N.g2

        # mixing matrix calculations based on Q
        g.hh <- ((c.g1*N.g1) + (Q*c.g2*N.g2)) / ((c.g1*N.g1) + (c.g2*N.g2))
        g.lh <- 1 - g.hh
        g.hl <- (1 - g.hh) * ((c.g1*N.g1) / (c.g2*N.g2))
        g.ll <- 1 - g.hl

        # prob that p is infected
        p.g1 <- (g.hh*I.g1/N.g1) + (g.lh*I.g2/N.g2)
        p.g2 <- (g.ll*I.g2/N.g2) + (g.hl*I.g1/N.g1)

        # lambda - force of infection
        lambda.g1 <- rho.g1 * c.g1 * p.g1
        lambda.g2 <- rho.g2 * c.g2 * p.g2

        ## Differential Equations ##
        dS.g1 <- 0.5*b.rate*N.g1 - lambda.g1*S.g1 - muS.g1*S.g1
        dI.g1 <- lambda.g1*S.g1 - muI.g1*I.g1

        dS.g2 <- 0.5*b.rate*N.g2 - lambda.g2*S.g2 - muS.g2*S.g2
        dI.g2 <- lambda.g2*S.g2 - muI.g2*I.g2


        ## Output ##
        list(c(dS.g1, dI.g1,
               dS.g2, dI.g2,
               incid.g1 = lambda.g1*S.g1,
               incid.g2 = lambda.g2*S.g2),
             N = N,
             prev = prev,
             prev.g1 = prev.g1,
             prev.g2 = prev.g2)

    })
}
