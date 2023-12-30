#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(plotly)
library(httr)
library(xml2)

# Import overall BGG data
bgg_data <- read_rds("bgg_data.rds")
  

# data for sliders
players_min <- min(bgg_data$min_players_rec, na.rm = TRUE)
players_max <- max(bgg_data$min_players_rec, na.rm = TRUE)
time_min    <- min(bgg_data$min_time, na.rm = TRUE)
time_max    <- max(bgg_data$min_time, na.rm = TRUE)

# Function to get a user's collection
get_user_collection <- function(username, retries = 10, retryDelay = 5) {
  
  url <- paste0("https://www.boardgamegeek.com/xmlapi2/collection?username=",
                username,
                "&own=1") # BGG API endpoint for fetching a user's collection
  
  response <- GET(url) # Make the GET request
  
  while (retries > 0) {
    # Make the GET request
    response <- GET(url)
    
    # Check the response status code
    if (status_code(response) == 200) { # Check the response status code
      content <- content(response, as = "text") # Parse the XML content
      xml_content <- read_xml(content)
      items <- xml_find_all(xml_content, ".//item") # Extract item nodes
      
      # Extract relevant data from each item node using sapply and xml2 functions
      collection <- tibble(
        objectid = sapply(items, function(x) xml_attr(x, "objectid")),
        name = sapply(items, function(x) xml_text(xml_find_first(x, ".//name")))
      )
      
      collection_tibble <- as_tibble(collection) # Convert the data frame to a tibble
      
      # Clean up 
      collection_tibble <- collection_tibble %>%
        mutate(objectid = as.numeric(objectid))
      
      
      return(collection_tibble)
      
    } else if (status_code(response) == 202) {
      
      # Accepted but processing, need to retry after some delay
      Sys.sleep(retryDelay)
      retries <- retries - 1
      
    } else {
      
      # Some other error occurred
      stop("Failed to retrieve collection. Status code: ", status_code(response))
    }
  }
  
  # If all retries exhausted
  stop("After ", retries, " retries, failed to retrieve collection. Status code: 202")
}


# Define UI 
ui <- fluidPage(
  
    titlePanel("Choose your next board game!"),

    sidebarLayout(
        sidebarPanel(
          
          textInput("username", "Filter by collection:", value = ""),
          
          checkboxInput("filterPlayers", "Filter by number of players:", 
                        value = FALSE),
          sliderInput("players", "Players:", 
                      min = players_min, max = players_max, value = 2, step = 1),
          
          checkboxInput("filterPlaytime", "Filter by playtime", 
                        value = FALSE),
          sliderInput("playtime", "Playtime (minutes):",
                      min = time_min, max = time_max, value = 120, step = 20)
        ),

        mainPanel(
          plotlyOutput("plot"), # Output for the interactive plot
          htmlOutput("explanatoryText")
        )
    )
)


# Server logic
server <- function(input, output, session) {
  
  # Reactive expression to hold the modified data
  reactive_data <- reactive({
    
    # Start with the original data
    data <- bgg_data
    
    # Apply player number filter
    if(input$filterPlayers) {
      data <- data[data$min_players_rec <= input$players & 
                     data$max_players_rec >= input$players, ]
    }
    
    # Apply playtime filter
    if(input$filterPlaytime) {
      data <- data[data$min_time <= input$playtime & 
                     data$max_time >= input$playtime, ]
    }
    
    # If a username is entered, modify the data
    if (input$username != "") {
      user_collection <- tryCatch({
        get_user_collection(input$username)
      }, error = function(e) NULL)
      
      # Check if user_collection is not NULL
      if (!is.null(user_collection)) {
        user_collection$objectid <- as.numeric(user_collection$objectid)
        user_collection_flag <- data$bgg_id %in% user_collection$objectid
        data$user_collection <- user_collection_flag
        data$alpha <- ifelse(user_collection_flag, 
                             1, 
                             0.3)
        data$hover_text <- ifelse(user_collection_flag, 
                                  as.character(data$name), 
                                  "")
      }
    } else {
      # Default values when no username is provided
      data$user_collection <- FALSE
      data$alpha <- 1
      data$hover_text <- data$name
    }
    data
  })
  
  # Rendering the plot
  output$plot <- renderPlotly({
    
    # Access the reactive dataset
    data <- reactive_data()
    
    p <- ggplot(data, 
                aes(x = bayes_rating, y = complexity, color = user_collection, 
                    alpha = alpha, text = hover_text)) +
      geom_point() + 
      scale_y_reverse() + 
      scale_color_manual(values = c("FALSE" = "black", "TRUE" = "darkblue")) +
      labs(x = "Geek Rating", y = "Complexity", title = "") +
      theme_classic() +
      theme(legend.position = "none") 
    
    # Convert to a plotly object
    ggplotly(p, tooltip = "text") %>% 
      layout(
        annotations = list(
          list(
            x = 1.0, y = 1.05, xref = 'paper', yref = 'paper',
            text = "Fun and easy party game", showarrow = FALSE, 
            xanchor = 'right', yanchor = 'top', font = list(color = 'darkgreen')
          ),
          list(
            x = 0, y = 1.05, xref = 'paper', yref = 'paper',
            text = "Boring but easy hangover game", showarrow = FALSE, 
            xanchor = 'left', yanchor = 'top', font = list(color = 'purple')
          ),
          list(
            x = 0, y = -0.12, xref = 'paper', yref = 'paper',
            text = "Hard and dull masochism", showarrow = FALSE, 
            xanchor = 'left', yanchor = 'bottom', font = list(color = 'darkred')
          ),
          list(
            x = 1, y = -0.12, xref = 'paper', yref = 'paper',
            text = "Games night workout", showarrow = FALSE, 
            xanchor = 'right', yanchor = 'bottom', font = list(color = 'orange')
          )
        )
      )
  })
  
  # Render explanatory text
  output$explanatoryText <- renderText({
    HTML("<p>This plot visualizes various board games using the <a href=\"https://boardgamegeek.com/wiki/page/BoardGameGeek_FAQ#toc4\">\"Geek Rating\" (a Bayesian average of scores)</a> and the \"Complexity Score\" from <a href=\"https://boardgamegeek.com\">BoardGameGeek</a> to help you choose either a new game to buy, or the next game to play from your collection.</p> <p>Use the filters to only show games for a specific number of players (recommended by BGG) or playtime (games with a playtime >8h are not included in this dataset!)</p><p>To restrict the graph to showing games only from a specific user's collection, enter their BGG username.</p>")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)



