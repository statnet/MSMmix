library("shiny")
library("shinydashboard")
library("deSolve")
suppressMessages(library("EpiModel"))
source("fx.R")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = "Assortative Mixing"),
    dashboardSidebar(
        width = 200,
        sidebarMenu(
            menuItem("Model Scenarios", tabName = "Model", icon = icon("line-chart"))
            #box(width = NULL,
            #    actionButton(inputId = "runMod", "Run Model"))
        )


    ),
    dashboardBody(
        tabItem(
            
            ## Model Scenarios Tab ##
            tabName = "Model",
            #                 
                fluidRow(
                    
                        # Prevalence Plot
                        column(width = 12,
                                box(width = NULL,
                                       title = "Assortative Mixing Plots", status = "primary", solidHeader = TRUE,
                                       plotOutput("a_Plot", height = 500)
                                        )

                             )
                ),
                fluidRow(       
                        # Input parameters
                        column(width = 4,
                               box(width = NULL,
                                        title = "Mixing and State Sizes", status = "success", solidHeader = TRUE,
                                           
                                        sliderInput(inputId = "Q", label = "Assortative mixing parameter: 0 = random, 1 = completely assortative",
                                               min = 0, max = 1, value = 0.02, step = 0.01),
                                        sliderInput(inputId = "S.high", label = "Initial Group 1 Susceptibles",
                                                       min = 999, max = 9999, step = 500, value = 999), 
                                        sliderInput(inputId = "I.high", label = "Initial Group 1 Infected",
                                                       min = 1, max = 101, step = 10, value = 1), 
                                        sliderInput(inputId = "S.low", label = "Initial Group 2 Susceptibles",
                                                       min = 999, max = 9999, step = 500, value = 999), 
                                        sliderInput(inputId = "I.low", label = "Initial Group 2 Infected",
                                                       min = 1, max = 101, step = 10, value = 1)
                               )),
                        column(width = 4,
                               box(width = NULL,
                                        title = "Contact and Transmission", status = "success", solidHeader = TRUE,
                                   
                                        sliderInput(inputId = "c.low", label = "Contact rate for Group 1 (partners / year)",
                                                       min = 0, max = 30, value = 15, step = 1),
                                        sliderInput(inputId = "c.mean", label = "Mean contact rate among both groups (partners / year)",
                                                       min = 0, max = 30, value = 15, step = 1),
                                        sliderInput(inputId = "rho.high", label = "Transmission parameter for Group 1",
                                                       min = 0, max = 1, value = 0.20, step = 0.05),
                                        sliderInput(inputId = "rho.low", label = "Transmission parameter for Group 2",
                                                       min = 0, max = 1, value = 0.20, step = 0.05)
                                       )
                            ),
                        
                        column(width = 4,
                                box(width = NULL,
                                        title = "Birth and Death Rates", status = "success", solidHeader = TRUE,
                                    
                                        sliderInput(inputId = "b.rate", label = "Birth Rate (%/year)",
                                                min = 0, max = 3, value = 0.5, step = 0.5),
                                        sliderInput(inputId = "muS.low", label = "Death rate for low-risk group w/o Disease (% / year)",
                                                   min = 0, max = 3, value = 0.5, step = 0.5),
                                        sliderInput(inputId = "muI.low", label = "Death rate for low-risk group w Disease",
                                                   min = 0, max = 3, value = 0.5, step = 0.5),
                                        sliderInput(inputId = "muS.high", label = "Death rate for high-risk group w/o Disease",
                                                   min = 0, max = 3, value = 0.5, step = 0.5),
                                        sliderInput(inputId = "muI.high", label = "Death rate for high-risk group w Disease",
                                                   min = 0, max = 3, value = 0.5, step = 0.5)
                               )
                        )
                        )
                )
            )
)
)


