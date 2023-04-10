server <- function(input, output) {
  
  # Display user ID on screen
  output$ID_sortie <- renderText({
    paste("Bienvenue ", input$user,".")
  })
  output$listened_to <- renderUI({
    recommended_artists <- artists$name[artists$charid %in% user_row[[input$user]]]
    
    output_text <- tagList()
    
    for (i in 1:length(recommended_artists)) {
      artist_name <- recommended_artists[i]
      ligne_artist <- subset(artists, name == artist_name)
      
      if (nrow(ligne_artist) == 0) {
        artist_link <- artist_name
      } else {
        artist_link <- tags$a(artist_name, href = ligne_artist$url, target = "_blank")
      }
      
      output_text <- tagList(output_text, artist_link, br())
    }
    
    output_text
  })
  
  output$recommendations_Popular <- renderUI({
    recommended_artists <- artists$name[artists$charid %in% Myrecommendations_pop[[input$user]]]
    
    output_text <- tagList()
    
    for (i in 1:length(recommended_artists)) {
      artist_name <- recommended_artists[i]
      ligne_artist <- subset(artists, name == artist_name)
      
      if (nrow(ligne_artist) == 0) {
        artist_link <- artist_name
      } else {
        artist_link <- tags$a(artist_name, href = ligne_artist$url, target = "_blank")
      }
      
      output_text <- tagList(output_text, artist_link, br())
    }
    
    output_text
  })
  output$recommendations_UBCF <- renderUI({
    #user_artists_selected <- user_artists_wide[user_artists_wide$USER == substring(input$user, 2), ] # liste qui contient l'utilisateur selectionn?
    recommended_artists <- artists$name[artists$charid %in% Myrecommendations_UBCF[[input$user]]]
    
    output_text <- tagList()
    
    for (i in 1:length(recommended_artists)) {
      artist_name <- recommended_artists[i]
      ligne_artist <- subset(artists, name == artist_name)
      
      if (nrow(ligne_artist) == 0) {
        artist_link <- artist_name
      } else {
        artist_link <- tags$a(artist_name, href = ligne_artist$url, target = "_blank", )
      }
      
      output_text <- tagList(output_text, artist_link, br())
    }
    
    output_text
  })
  output$recommendations_als <- renderUI({
    #user_artists_selected <- user_artists_wide[user_artists_wide$USER == substring(input$user, 2), ] # liste qui contient l'utilisateur selectionn?
    recommended_artists <- artists$name[artists$charid %in% Myrecommendations_als[[input$user]]]
    
    output_text <- tagList()
    
    for (i in 1:length(recommended_artists)) {
      artist_name <- recommended_artists[i]
      ligne_artist <- subset(artists, name == artist_name)
      
      if (nrow(ligne_artist) == 0) {
        artist_link <- artist_name
      } else {
        artist_link <- tags$a(artist_name, href = ligne_artist$url, target = "_blank")
      }
      
      output_text <- tagList(output_text, artist_link, br())
    }
    
    output_text
  })
  output$Favorite <- renderUI({
    user_artists_selected <- user_artists_wide[user_artists_wide$user == substring(input$user, 2), ]
    user_artists_sorted <- user_artists_selected[order(user_artists_selected, decreasing = TRUE), ]
    
    output_text <- tagList()
    
    for (i in 1:5) {
      artist_name <- user_artists_sorted[i]
      ligne_artist <- subset(artists, name == artist_name)
      
      if (nrow(ligne_artist) == 0) {
        artist_link <- artist_name
      } else {
        artist_link <- tags$a(artist_name, href = ligne_artist$url, target = "_blank")
      }
      
      output_text <- tagList(output_text, artist_link, br())
    }
    
    output_text
  })
  output$recommendations_Random <- renderUI({
    #user_artists_selected <- user_artists_wide[user_artists_wide$USER == substring(input$user, 2), ] # liste qui contient l'utilisateur selectionn?
    recommended_artists <- artists$name[artists$charid %in% Myrecommendations_random[[input$user]]]
    
    output_text <- tagList()
    
    for (i in 1:length(recommended_artists)) {
      artist_name <- recommended_artists[i]
      ligne_artist <- subset(artists, name == artist_name)
      
      if (nrow(ligne_artist) == 0) {
        artist_link <- artist_name
      } else {
        artist_link <- tags$a(artist_name, href = ligne_artist$url, target = "_blank")
      }
      
      output_text <- tagList(output_text, artist_link, br())
    }
    
    output_text
  })
}