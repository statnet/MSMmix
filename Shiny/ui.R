library(shiny)
library(shinydashboard)
library(deSolve)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = " HIV Racial Disparities "),
    dashboardSidebar(
        width = 200,
        sidebarMenu(
            menuItem("Model Scenarios", tabName = "Model", icon = icon("line-chart"))
        )


    ),
    dashboardBody(
        tabItem(
            
            ## Model Scenarios Tab ##
            tabName = "Model",
            #                 
                fluidRow(
                    
                        # Prevalence Plot
                        column(width = 8,
                                box(width = NULL,
                                       title = "Model Plots", status = "primary", solidHeader = TRUE,
                                       plotOutput("a_Plot", height = 792)
                                        )

                             ),
                        
                        # Input parameters
                        column(width = 2,
                                       box(width = NULL,
                                           title = "Model Parameters" , status = "success", solidHeader = TRUE,
                                           sliderInput(inputId = "N.tot", label = "Initial Population Size",
                                                       min = 1000000, max = 20000000, step = 1000000, value = 2000000), 
                                           sliderInput(inputId = "brate", label = "Birth Rate (%/year)",
                                                       min = 0, max = 3, value = 0.5, step = 0.5),
                                           sliderInput(inputId = "c.low", label = "Contact rate for Group 1 (partners / year)",
                                                       min = 0, max = 30, value = 15, step = 1),
                                           sliderInput(inputId = "c.mean", label = "Mean contact rate (partners / year)",
                                                       min = 0, max = 30, value = 15, step = 1),
                                           sliderInput(inputId = "rho.high", label = "Transmission parameter for Group 1",
                                                       min = 0, max = 1, value = 0.20, step = 0.05),
                                           sliderInput(inputId = "rho.low", label = "Transmission parameter for Group 2",
                                                       min = 0, max = 1, value = 0.20, step = 0.05)
                                       )
                            ),
                        
                        column(width = 2,
                               box(width = NULL,
                                   title = "Model Parameters" , status = "success", solidHeader = TRUE,
                                   sliderInput(inputId = "muS.low", label = "Death rate for low-risk group w/o Disease (% / year)",
                                               min = 0, max = 3, value = 0.5, step = 0.5),
                                   sliderInput(inputId = "muI.low", label = "Death rate for low-risk group w Disease",
                                               min = 0, max = 3, value = 0.5, step = 0.5),
                                   sliderInput(inputId = "muS.high", label = "Death rate for high-risk group w/o Disease",
                                               min = 0, max = 3, value = 0.5, step = 0.5),
                                   sliderInput(inputId = "muI.high", label = "Death rate for high-risk group w Disease",
                                               min = 0, max = 3, value = 0.5, step = 0.5),
                                   sliderInput(inputId = "Q", label = "Assortative mixing parameter",
                                               min = 0, max = 1, value = 0.02, step = 0.01),
                                   sliderInput(inputId = "prop.high", label = "Proportion in high-risk group",
                                               min = 0, max = 1, value = 0.02, step = 0.01)
                               )
                        )
                        )
                )
            )
)
)


