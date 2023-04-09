library(shiny)
library(shinydashboard)
library(plotly)
library(tidyr)

# Importation des données
spotify <- read.csv("~/RShinyApplication/app/data/Spotify_Youtube.csv")
spotify[is.na(spotify)] <- 0

round_number <- function(x) {
  if (x >= 1e6) {
    paste0(round(x / 1e5) / 10, "M")
  } else if (x >= 1e3) {
    paste0(round(x / 1e2) / 10, "K")
  } else {
    x
  }
}

# MinMaxScaler 
minmax <- function(x, na.rm = TRUE) {
  scaler = (x - min(x)) / (max(x) - min(x))
  return(scaler)
}

# Configuration du serveur
shinyServer(function(input, output){
  
  filtered_data <- reactive({
    spotify %>%
    filter(Artist == input$artist)
    })
  
  output$Stream <- renderValueBox({
    valueBox(
      round_number(sum(filtered_data()$Stream)), "Number of Stream", icon = icon("stream", lib = "font-awesome"),
      color = "purple"
    )
  })
  
  output$Views <- renderValueBox({
    valueBox(
      round_number(sum(filtered_data()$Views)), "Number of Views", icon = icon("eye", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  output$Likes <- renderValueBox({
    valueBox(
      round_number(sum(filtered_data()$Likes)), "Number of Likes", icon = icon("thumbs-up", lib = "font-awesome"),
      color = "red"
    )
  })
  
  output$Comments <- renderValueBox({
    valueBox(
      round_number(sum(filtered_data()$Comments)), "Number of Comments", icon = icon("comments", lib = "font-awesome"),
      color = "green"
    )
  })
  
  # Création de l'histogramme de la durée des chansons
  output$histogram <- renderPlotly({
      p <- ggplot(data = filtered_data(), aes(x = Danceability)) +
        geom_histogram(bins=10)
      
      ggplotly(p) %>%
        config(displayModeBar = FALSE)
  })
  
  # Création du nuage de points
  output$scatterplot <- renderPlotly({
    p <- ggplot(data = filtered_data(), aes(x = Views, y = Comments, color = Album_type)) +
      geom_point(size=3, alpha=0.8)
    
    ggplotly(p) %>%
      config(displayModeBar = FALSE)
  })
  
  # Création d'un graphique en barre
  output$barplot <- renderPlotly({
    top5 <- filtered_data() %>%
      group_by(Track) %>%
      summarise(Record = sum(Comments)) %>%
      arrange(desc(Record))
    
    p <- ggplot(data = top5[1:5,], aes(x=reorder(Track, Record), y=Record)) +
      geom_bar(stat="identity") +
      coord_flip()
    
    ggplotly(p) %>%
      config(displayModeBar = FALSE)
    
  })
  
  # Création d'un graphique en coordonnées polaires
  output$polarplot <- renderPlot({
    
    df_polar <- filtered_data() %>%
      gather(key = "colname", value = "metric", Danceability:Tempo) %>%
      mutate(metric = minmax(metric))
    
    p <- ggplot(df_polar, aes(x=colname, y=metric)) +
      geom_point() +
      coord_polar()
    
    p
    
  })
  
  
  # Filtrage croisé pour les graphiques
  # observeEvent(event_data("plotly_selected", source = "scatterplot"), {
  #   selected_points <- event_data("plotly_selected", source = "scatterplot")
  #   if (!is.null(selected_points)) {
  #     spotify_filtered <- spotify %>%
  #       filter(Danceability >= selected_points$x[1] &
  #                Danceability <= selected_points$x[2] &
  #                Energy >= selected_points$y[1] &
  #                Energy <= selected_points$y[2])
  #     plot_ly(spotify_filtered, x = ~Duration_ms, type = "histogram")
  #   } else {
  #     plot_ly(spotify, x = ~Duration_ms, type = "histogram")
  #   }
  # })
  
})
