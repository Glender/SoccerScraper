library(rvest)
library(stringr)
library(tibble)


# scrape a table with the numbers of players/staff for a team 
scrape_nr_players_season <- function(url) {
  
  html_doc <- rvest::read_html(url)
  
  # extract table
  data <- html_doc %>%
    rvest::html_table(header = FALSE) %>% 
    .[[1]] 
  
  # extract players
  players <- data$X3 %>%
    stringr::str_remove_all("[:alpha:]|[:punct:]| ") %>%
    as.numeric()
  
  # extract the start and end date of
  # the season. data are like 'Seizoen 2021/2022'
  # so we remove the chars, keep the numbers
  season_start_end_date <- data$X2 %>% 
    stringr::str_remove_all("[:alpha:]| ") %>%
    stringr::str_split(pattern = "/")
  
  # season start_start_end_date is a list of char vectors
  # we extract the first and second element to get the desired data
  season_end <- as.numeric(sapply(season_start_end_date, "[[", 2))
  season_start <- as.numeric(sapply(season_start_end_date, "[[", 1))
  
  # collect data in tibble dataframe
  data <- tibble::tibble(
    club = data$X1,
    season_start = season_start,
    season_end = season_end,
    nr_of_players = players
  )
  return(data)
}

# example for Feyenoord:

# state url from which to scrape
# data on number of players per season
url <- 'https://www.voetbal.com/teams/feyenoord/20/'

data <- scrape_nr_players_season(url)
print(data)