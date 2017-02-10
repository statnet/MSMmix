library("shiny")
library("shinydashboard")
library("deSolve")
suppressMessages(library("EpiModel"))
source("fx.R")
library("plotly")
library("DiagrammeR")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = "Mixing DCM"),
    dashboardSidebar(
        width = 200,
        sidebarMenu(
            menuItem("Introduction", tabName = "Introduction", icon = icon("book")),
            menuItem("Model Scenarios", tabName = "Model", icon = icon("line-chart"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Introduction",
                     column(10, offset = 1,
                            h3("Impact of Assortative Mixing on HIV Prevalence and Incidence among MSM",
                               style = "color: #193556;"),
                            h4("A Web-Based Modeling Tool for Public Health Practice", style = "color: #2E619E;"),
                            hr(),
                            p("This software tool provides additional opportunities to explore a simple deterministic 
                              compartment model of assortative mixing for a SI disease epidemic. To get started, enter the following:"),
                            tags$ul(
                                tags$li(strong("Initial Conditions:"), "a starting number of individuals for each of the four compartments, 
                              susceptible and infected individuals in both group 1 and group 2"),
                                tags$li(strong("Control Settings:"), "number of time steps and the length of each time step."),
                                tags$li(strong("Contact and Transmission:"), "the Q mixing statistic, as well as contact rates and transmission rates for both groups."),
                                tags$li(strong("Demographics:"), "underlying birth and death rates for the model.")
                            ),
                            p("After selecting the parameters set in each model, the model will
                             automatically update the plots .")
                     )
                     
            ),
            
            tabItem(
                tabName = "Model",
                    fluidRow(
                      # Prevalence Plot
                      column(width = 6,
                              box(width = NULL,
                                     title = "Prevalence", status = "primary", solidHeader = TRUE,
                                     plotlyOutput("plot1"))),
                      column(width = 6,
                             box(width = NULL,
                                 title = "Incidence", status = "primary", solidHeader = TRUE,
                                 plotlyOutput("plot2")))
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
                                                min = 0, value = 500),
                                   numericInput(inputId = "dt", label = "dt",
                                                min = 0, value = 0.1)
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
                                               min = 0, max = 10, value = 2, step = 0.1),
                                      sliderInput(inputId = "c.g1", label = "Contact Rate, Group 1",
                                                     min = 0, max = 10, value = 2, step = 0.1),
                                      sliderInput(inputId = "rho.g1", label = "Transmission Rate, Group 1",
                                                     min = 0, max = 1, value = 0.05, step = 0.005),
                                      sliderInput(inputId = "rho.g2", label = "Transmission Rate, Group 2",
                                                     min = 0, max = 1, value = 0.05, step = 0.005)
                               )),
                            column(width = 4,
                              box(width = NULL,
                                      title = "Demographics", status = "success", solidHeader = TRUE,
    
                                      sliderInput(inputId = "b.rate", label = "Birth Rate",
                                              min = 0, max = 1, value = 0.1, step = 0.01),
                                      sliderInput(inputId = "muS.g1",
                                                  label = "Death Rate, Susceptible, Group 1 ",
                                                 min = 0, max = 1, value = 0.1, step = 0.01),
                                      sliderInput(inputId = "muI.g1",
                                                  label = "Death Rate, Infected, Group 1",
                                                 min = 0, max = 1, value = 0.1, step = 0.01),
                                      sliderInput(inputId = "muS.g2",
                                                  label = "Death Rate, Susceptible, Group 2",
                                                 min = 0, max = 1, value = 0.1, step = 0.01),
                                      sliderInput(inputId = "muI.g2",
                                                  label = "Death Rate, Infected, Group 2",
                                                   min = 0, max = 1, value = 0.1, step = 0.01)
                             ))
                          ) # end fluidRow
                    ) # end tabItem
            ) # end tabItems
    )# end dashboardBody
)
)


