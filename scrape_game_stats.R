library(rvest)
library(stringr)
library(tibble)
library(readr)

# extract start season from url
get_year_from_url <- function(url) {
  # https://regexland.com/all-between-specified-characters/
  # find all text between the '-'s
  start_season <- 
    stringr::str_extract(
      url, "(?<=ned-eredivisie-).{4}(?=-)"
    )
  return(as.numeric(start_season))
}

# get characters before the colon and convert to numeric
# e.g. get double 32 from '32:18'
get_before_colon <- function(string) {
  return(
    as.numeric(stringr::str_extract(string, "(\\d*)"))
  )
}


# this function takes a url from voetbal.com as input
# like https://www.voetbal.com/wedstrijdgegevens/ned-eredivisie-2021-2022-spieltag/34/
# and it scrapes the table with game statistics for a season, wins, goals losses etc.
scrape_gamestats <- function(url){
  
  cat("Scraping data from:", url, "\n")
  
  # extract the 4th table from the website
  tables <- rvest::read_html(url) %>%
    rvest::html_table()
  
  # from the multiple tables collected on the website
  # extract the table with the most rows
  idx <- which.max(sapply(tables, nrow))
  table <- tables[[idx]]
  
  # remove cols that only have Na's
  table <- table[, 
      colSums(is.na(table)) < nrow(table)
  ]
  
  # one table column is like
  # 88:23, extract those pair of numbers
  # separately (88 and 23) using regexes
  own_goals <- get_before_colon(table$Dp.)
  goals_against <- as.numeric(
    stringr::str_extract(table$Dp., "(?<=:).*")
  )
  
  # from the url we extract the start of season
  season_start <- get_year_from_url(url)
  
  # because of the changing pointing system
  # sometimes points are reported in the format 
  # 54:14, if so, get the first two numbers
  if( typeof(table$P.) == "character" ){
    made_points <- get_before_colon(table$P.)
  } else {
    made_points <- table$P.
  }
  
  # gather all data in tibble dataframe
  data <- tibble::tibble(
    rank = table$`#`,
    season_start = season_start,
    season_end = season_start + 1,
    team = table$Team,
    nr_games = table$GS.,
    wins = table$W.,
    stale = table$G.,
    loss = table$V.,
    goals_made = own_goals,
    goals_against = goals_against,
    saldo = table$Ds.,
    points = made_points
  )
  return(data)
}


# create vector of urls
start_years <- 1956:2021
urls <- sprintf(
  'https://www.voetbal.com/wedstrijdgegevens/ned-eredivisie-%s-%s-spieltag/34/', 
  start_years, 
  start_years + 1
)

# example
urls[20:40]


# get the gamestats from one url
scrape_gamestats(urls[66])


# fun extracts all the game stat data from the 
# input urls and collect the gamestats
# and merges them
scrape_all_gamestats <- function(urls) {
  
  data <- lapply(urls, scrape_gamestats)
  df <- do.call(rbind, data)
  
  return(df)
}

# save and load data
#saveRDS(df, file = "gamestats_eredivisie.rds")
#df <- readRDS("gamestats_eredivisie.rds")
#write_csv2(df, "gamestats_eredivisie.csv")
