

ui <- fluidPage(
  # theme of the app
  source('theme.R'),
  # App title
  titlePanel("Music Recommender System"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      selectInput("user", "Find your user ID :", choices = list_id)
    ),
    
    # Main panel
    mainPanel(
      tabsetPanel(
        #tabPanel("Favorite", uiOutput("Favorite"))#,
        tabPanel("Listened to artists", uiOutput("listened_to")),
        tabPanel("Recommendations Popular", uiOutput("recommendations_Popular")), # transforme en html
        tabPanel("Recommendations UBCF", uiOutput("recommendations_UBCF")),
        tabPanel("Recommendations ALS", uiOutput("recommendations_als")),
        tabPanel("Recommendations Random", uiOutput("recommendations_Random"))
      )
    )
  )
)