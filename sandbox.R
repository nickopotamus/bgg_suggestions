# Load necessary libraries
library(tidyverse)
library(httr)
library(xml2)

# Read Kaggle CSV file and select required columns
file_path <- "data/bgg_GameItem.csv" 

bgg_data <- read_csv(file_path) %>%
  as_tibble() %>% 
  select(bgg_id, name, min_players_rec, max_players_rec, min_time, max_time, bayes_rating, complexity) %>% 
  filter(!is.na(bayes_rating) & !is.na(complexity)) %>% 
  filter(min_time <= 480)

write_rds(bgg_data, "plot_app/bgg_data.rds") # Save this for the shiny app

# Simple plot
ggplot(bgg_data) +
  aes(x = bayes_rating, y = complexity) +
  geom_point() +
  labs(x = "Geek Rating", y = "Complexity", title = "Geek rating against game complexity") +
  theme_classic() 

# Get user's collection
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

# Replace 'your_username' with the actual BGG username
user_collection_tibble <- get_user_collection("your_username")
#print(user_collection_tibble)

# Plot highlighting collection

# Tweak the table for plotting
bgg_data$user_collection <- bgg_data$bgg_id %in% user_collection_tibble$objectid
bgg_data$alpha <- ifelse(bgg_data$user_collection, 1, 0.3)
bgg_data$hover_text <- ifelse(bgg_data$user_collection, as.character(bgg_data$name), "")


# Create the ggplot
ggplot(bgg_data) +
  aes(x = bayes_rating, y = complexity, color = user_collection, alpha = alpha) +
  geom_point() +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) + # Set color: red for user's collection, black for others
  labs(x = "Bayes Rating", y = "Complexity", title = "Bayes Rating vs Complexity") +
  theme_minimal() +
  theme(legend.position = "none") # Hide the legend

