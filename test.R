library(shiny)
library(shinydashboard)
library(deSolve)
suppressMessages(library("EpiModel"))
source("shiny/fx.R")
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