
##
## EPI 554 Lab 6
##

N <- 20000000
prop.high <- 0.02
prop.low <- 0.98

S.high <- N*prop.high
S.low <- N*prop.low

I.high <- 1
I.low <- 0

N.high <- S.high+I.high
N.low <- S.low+I.low

c.mean <- 2
c.low <- 1.4

c.high <- (c.mean*N - c.low*N.low)/N.high

g.h <- function(c.high, N.high, c.low, N.low) {
  (c.high*N.high)/(c.high*N.high + c.low*N.low)
}
g.h(c.high, N.high, c.low, N.low)

g.hl <- function(g.hh, c.h, N.h, c.l, N.l) {
  (1-g.hh)*(c.high*N.high)/(c.low*N.low)
}
g.ll <- function(g.hh, c.high, N.high, c.low, N.low) {
  1-(1-g.hh)*(c.high*N.high)/(c.low*N.low)
}
g.lh <- function(g.hh) {
  1-g.hh
}
Q <- function(g.hh, c.high, N.high, c.low, N.low) {
  (g.hh + g.ll(g.hh, c.high, N.high, c.low, N.low)-1)
}

Q(g.hh=1, c.high, N.high, c.low, N.low)
Q(g.hh=0, c.high, N.high, c.low, N.low)
Q(g.hh=0.314, c.high, N.high, c.low, N.low)



#########

N.tot <- 20000000
prop.high <- 0.02
prop.low <- 0.98

library(deSolve)

Qmod <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {

    ## Dynamic Calculations ##

    # Popsize
    N.high <- S.high + I.high
    N.low <- S.low + I.low
    N <- N.high + N.low
    prev <- (I.high+I.low)/N

    # Contact rates
    c.high <- (c.mean*N - c.low*N.low)/N.high

    # mixing matrix calculations based on Q
    g.hh <- ((c.high*N.high) + (Q*c.low*N.low)) / ((c.high*N.high) + (c.low*N.low))
    g.lh <- 1 - g.hh
    g.hl <- (1 - g.hh) * ((c.high*N.high) / (c.low*N.low))
    g.ll <- 1 - g.hl

    # prob that p is infected
    p.high <- (g.hh*I.high/N.high)+(g.lh*I.low/N.low)
    p.low <- (g.ll*I.low/N.low)+(g.hl*I.high/N.high)

    # lambda
    lambda.high <- rho * c.high * p.high
    lambda.low <- rho * c.low * p.low


    ## Differential Equations ##
    dS.high <- -lambda.high*S.high + nu*I.high
    dI.high <- lambda.high*S.high - nu*I.high

    dS.low <- -lambda.low*S.low + nu*I.low
    dI.low <- lambda.low*S.low - nu*I.low


    ## Output ##
    list(c(dS.high, dI.high,
           dS.low, dI.low),
           N = N,
           prev = prev)

  })
}

params <- list(
  c.mean = 2,
  c.low = 1.4,
  rho = 0.75,
  nu = 6,
  Q = 0
)

t0 <- c(
  S.high = N.tot*prop.high - 1,
  I.high = 1,
  S.low = N.tot*prop.low - 1,
  I.low = 1
)
dt <- seq(1, 100, 0.02)


df1 <- data.frame(ode(y=t0, times=dt, func=Qmod, parms=params, method='rk4'))

head(df1)

library(RColorBrewer)
pal <- brewer.pal(5, 'Set1')

par(mar=c(3,3,1,1))
plot(df1$time, df1$prev, type='l', ylim=c(0,0.04), col=pal[1], lwd=3)
lines(df2$time, df2$prev, col=pal[2], lwd=3)
lines(df3$time, df3$prev, col=pal[3], lwd=3)
lines(df4$time, df4$prev, col=pal[4], lwd=3)
lines(df5$time, df5$prev, col=pal[5], lwd=3)
legend('topright', legend=paste('Q =', Qseq), lty=1, lwd=3, col=pal, cex=0.75)
