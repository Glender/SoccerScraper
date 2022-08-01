library(rvest)
library(stringr)
library(tibble)
library(readr)


# this function extracts the clubname from a url
# e.g. its get 'feyenoord' from 'https://www.voetbal.com/teams/feyenoord/2016/6/'
find_clubname_in_url <- function(url) {
  
  # find all  '/' chars in your url
  # it is assumed that the club name
  # is located between the 4th and 5th '/'
  lookup <- stringr::str_locate_all(url, "/")[[1]]
  start_end <- lookup[c(4,5), 1]
  
  # extract clubname
  club_name <- stringr::str_sub(
    url, 
    start = start_end[1] + 1, 
    end = start_end[2] - 1
  )
  
  return(club_name)
}


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
    nr_of_players = players,
    tag = find_clubname_in_url(url)
  )
  
  cat("Scraped nr. of players per season from:", url, "\n")
  
  return(data)
}


# wrapper around scrape_nr_players_season
scrape_nr_player_season_urls <- function(urls) {
  data <- lapply(urls, scrape_nr_players_season)
  return(do.call(rbind, data))
}


# example for Feyenoord:

# state url from which to scrape
# data on number of players per season
url <- 'https://www.voetbal.com/teams/feyenoord/20/'

data <- scrape_nr_players_season(url)
print(data)


# list with team names that need to be scaped
team_names <- c(
  "sc-heerenveen",
  "fortuna-sittard",
  "sc-cambuur",
  "psv-eindhoven",
  "rkc-waalwijk",
  "fc-groningen",
  "vitesse",
  "nec-nijmegen",
  "az-alkmaar",
  "sparta-rotterdam",
  "afc-ajax",
  "sbv-excelsior",
  "fc-emmen",
  "fc-utrecht",
  "fc-volendam",
  "feyenoord",
  "fc-twente",
  "go-ahead-eagles"
)

# create url with team names
urls <- sprintf("https://www.voetbal.com/teams/%s/20/", team_names)
print(urls)

# scrape all urls
total_data <- scrape_player_count_per_season_urls(urls)

#write_csv2(total_data, "player_count_per_season.csv")
#saveRDS(total_data, "player_count_per_season.rds")