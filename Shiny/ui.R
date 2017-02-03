library(shiny)
library(shinydashboard)
library(wesanderson)
library(deSolve)
source("fx.R")

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
                        column(width = 4,
                                       box(width = NULL,
                                           title = "Model Parameters" , status = "success", solidHeader = TRUE,
                                           selectInput(inputId = "N.tot", label = "Initial Population Size",
                                                       choices = c("1000000", "5000000", "10000000", "20000000")),
                                           selectInput(inputId = "brate", label = "Birth Rate",
                                                       choices = c("0.5%", "1.0%", "1.5%", "2.0")),
                                           selectInput(inputId = "c.low", label = "Contact rate for low-risk group",
                                                       choices = c("26% (Paper Model)",
                                                                   "20%", "15%", "10%")),
                                           selectInput(inputId = "c.mean", label = "Mean contact rate",
                                                       choices = c("26% (Paper Model)",
                                                                   "20%", "15%", "10%")),
                                           selectInput(inputId = "rho.high", label = "Transmission parameter for high-risk group",
                                                       choices = c("26% (Paper Model)",
                                                                   "20%", "15%", "10%")),
                                           selectInput(inputId = "rho.low", label = "Transmission parameter for high-risk group",
                                                       choices = c("26% (Paper Model)",
                                                                   "20%", "15%", "10%")),
                                           selectInput(inputId = "muS.low", label = "Death rate for low-risk group w/o Disease",
                                                       choices = c("26% (Paper Model)",
                                                                   "20%", "15%", "10%")),
                                           selectInput(inputId = "muI.low", label = "Death rate for low-risk group w Disease",
                                                       choices = c("26% (Paper Model)",
                                                                   "20%", "15%", "10%")),
                                           selectInput(inputId = "muS.high", label = "Death rate for high-risk group w/o Disease",
                                                       choices = c("26% (Paper Model)",
                                                       "20%", "15%", "10%")),
                                           selectInput(inputId = "muI.high", label = "Death rate for high-risk group w Disease",
                                                       choices = c("26% (Paper Model)",
                                                                   "20%", "15%", "10%")),
                                           selectInput(inputId = "Q", label = "Assortative mixing parameter",
                                                       choices = c("26% (Paper Model)",
                                                                   "20%", "15%", "10%")),
                                           sliderInput(inputId = "prop.high", label = "Proportion in high-risk group",
                                                       min = 0, max = 1, value = 0.02)
                                       )
                            )
            
                        )
                )
            )
)
)


