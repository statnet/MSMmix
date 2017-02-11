library("shiny")
library("shinydashboard")
library("deSolve")
suppressMessages(library("EpiModel"))
source("fx.R")
library("plotly")

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


# Intro Page --------------------------------------------------------------

tabItem(tabName = "Introduction",
column(10, offset = 1,

h3("Impact of Assortative Mixing on HIV Prevalence and Incidence among MSM",
style = "color: #193556;"),
h4("A Web-Based Modeling Tool for Public Health Practice", 
   style = "color: #2E619E;"),
hr(),
p("This application simulates a deterministic compartment model of assortative 
  mixing for a SI disease. To get started, enter the following:"),
tags$ul(
tags$li(strong("Initial Conditions:"), "Starting number of individuals for each 
        of the four compartments, susceptible and infected individuals in both 
        group 1 and group 2"),
tags$li(strong("Control Settings:"), "Number of time steps and the length of each 
        time step."),
tags$li(strong("Contact and Transmission:"), "Q mixing statistic, as well as 
        contact rates and transmission rates for both groups."),
tags$li(strong("Demographics:"), "Birth and death rates for the model.")
),
p("After selecting the parameters set in each model, the model will 
  automatically update the plots ."),

p("These are the differential equations for the model:"),
withMathJax(),
tags$div(HTML("
")),
helpText('$$\\frac{dS_{1}}{dt} = \\nu N_{1} - \\lambda_{1} S_{1} - \\mu_{S_{1}} S_{1}$$'),
helpText('$$\\frac{dI_{1}}{dt} = \\lambda_{1} S_{1} - \\mu_{I_{1}} I_{1}$$'),
helpText('$$\\frac{dS_{2}}{dt} = \\nu N_{2} - \\lambda_{2} S_{2} - \\mu_{S_{2}} S_{2}$$'),
helpText('$$\\frac{dI_{2}}{dt} = \\lambda_{2} S_{2} - \\mu_{I_{2}} I_{2}$$'),
helpText('$$N_{1} = S_{1} + I_{1}$$'),
helpText('$$N_{2} = S_{2} + I_{2}$$'),
helpText('\\(I_{j}\\) is number of people of state \\(I\\) from group \\(j\\)'), 
helpText('\\(\\nu\\) is the birth rate'), 
helpText('\\(\\lambda_{j}\\) is the force of infection for people from group \\(j\\)'),
helpText('\\(\\mu_{ij}\\) is the death rate for people of state \\(I\\) from group \\(j\\)'), 
helpText('\\(N_{i}\\) is the number of people in group \\(j\\)')
)
),


# Results Page ------------------------------------------------------------

tabItem(
tabName = "Model",

## Plot windows
fluidRow(
column(width = 6,
box(width = NULL,
title = "Prevalence", status = "primary", solidHeader = TRUE,
plotlyOutput("plot1"))),

column(width = 6,
box(width = NULL,
title = "Incidence", status = "primary", solidHeader = TRUE,
plotlyOutput("plot2")))
),

# Inputs
fluidRow(
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
)
),

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
)
)

) # end fluidRow
) # end tabItem
) # end tabItems
) # end dashboardBody
)
)


