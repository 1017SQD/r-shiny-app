library(shiny)
library(shinydashboard)
library(plotly)

# Importation des données
spotify <- read.csv("~/RShinyApplication/app/data/Spotify_Youtube.csv")

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Spotify & Youtube"),
    ## Sidebar content
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Widgets", tabName = "widgets", icon = icon("th")),
        
        selectInput(inputId="artist", label="Select input",
                    choices = c(unique(spotify$Artist)), 
                    selected=c(unique(spotify$Artist))[0])
      )
    ),
    ## Body content
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                fluidRow(
                  # Dynamic valueBoxes
                  valueBoxOutput("Stream", width = 3),
                  
                  valueBoxOutput("Views", width = 3),
                  
                  valueBoxOutput("Likes", width = 3),
                  
                  valueBoxOutput("Comments", width = 3)
                  ),
                
                fluidRow(
                  box(plotlyOutput("histogram", height = 250)),
                  
                  box(plotlyOutput("scatterplot", height = 250))
                ),
                
                fluidRow(
                  box(plotlyOutput("barplot", height = 250)),
                      
                  box(plotOutput("polarplot", height = 250))

                )
        ),
        
        # Second tab content
        tabItem(tabName = "widgets",
                h2("Widgets tab content")
        )
      )
    )
  )
)
