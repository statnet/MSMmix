library("shiny")
library("shinydashboard")
library("deSolve")
suppressMessages(library("EpiModel"))
source("fx.R")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = "Mixing DCM"),
    dashboardSidebar(
        width = 200,
        sidebarMenu(
            menuItem("Model Scenarios", tabName = "Model", icon = icon("line-chart"))
        )
    ),
    dashboardBody(
        tabItem(
            tabName = "Model",
                fluidRow(
                  # Prevalence Plot
                  column(width = 12,
                          box(width = NULL,
                                 title = "Results", status = "primary", solidHeader = TRUE,
                                 plotOutput("a_Plot", height = 500)))
                ),
                fluidRow(
                        # Input parameters
                        column(width = 4,
                           box(width = NULL,
                                    title = "Initial Conditions", status = "warning", solidHeader = TRUE,

                                    numericInput(inputId = "S.g1", label = "Susceptible, Group 1",
                                                 min = 0, value = 1000),
                                    numericInput(inputId = "I.g1", label = "Infected, Group 1 ",
                                                 min = 0, value = 1),
                                    numericInput(inputId = "S.g2", label = "Susceptible, Group 2",
                                                 min = 0, value = 1000),
                                    numericInput(inputId = "I.g2", label = "Infected, Group 2",
                                                 min = 0, value = 1)
                           ),
                           box(width = NULL,
                               title = "Control Settings", status = "warning", solidHeader = TRUE,
                               numericInput(inputId = "nsteps", label = "Time Steps",
                                            min = 0, value = 1000),
                               numericInput(inputId = "dt", label = "dt",
                                            min = 0, value = 1)
                           )
                           ),
                        column(width = 4,
                           box(width = NULL,
                                  title = "Contact and Transmission", status = "success", solidHeader = TRUE,
                                  sliderInput(inputId = "Q",
                                           label = "Q Mixing Statistic (0 = proportional, 1 = assortative)",
                                           min = 0, max = 1, value = 0, step = 0.01),
                                  sliderInput(inputId = "c.mean",
                                           label = "Mean Contact Rate Overall",
                                           min = 0, max = 30, value = 4, step = 1),
                                  sliderInput(inputId = "c.g1", label = "Contact Rate, Group 1",
                                                 min = 0, max = 30, value = 4, step = 1),
                                  sliderInput(inputId = "rho.g1", label = "Transmission Rate, Group 1",
                                                 min = 0, max = 1, value = 0.20, step = 0.01),
                                  sliderInput(inputId = "rho.g2", label = "Transmission Rate, Group 2",
                                                 min = 0, max = 1, value = 0.20, step = 0.01)
                           )),
                        column(width = 4,
                          box(width = NULL,
                                  title = "Demographics", status = "success", solidHeader = TRUE,

                                  sliderInput(inputId = "b.rate", label = "Birth Rate",
                                          min = 0, max = 3, value = 0.5, step = 0.1),
                                  sliderInput(inputId = "muS.g1",
                                              label = "Death Rate, Susceptible, Group 1 ",
                                             min = 0, max = 3, value = 0.5, step = 0.1),
                                  sliderInput(inputId = "muI.g1",
                                              label = "Death Rate, Infected, Group 1",
                                             min = 0, max = 3, value = 0.5, step = 0.1),
                                  sliderInput(inputId = "muS.g2",
                                              label = "Death Rate, Susceptible, Group 2",
                                             min = 0, max = 3, value = 0.5, step = 0.1),
                                  sliderInput(inputId = "muI.g2",
                                              label = "Death Rate, Infected, Group 2",
                                               min = 0, max = 3, value = 0.5, step = 0.1)
                         ))
                      ) # end fluidRow
                ) # end tabItem
            ) # end dashboardBody
)
)


