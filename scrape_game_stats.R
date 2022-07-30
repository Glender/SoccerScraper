library(rvest)
library(stringr)
library(tibble)


# extract start season from url
get_year_from_url <- function(url) {
  
  # https://regexland.com/all-between-specified-characters/
  # find all text between the '-'s
  # 
  start_season <- stringr::str_extract(
    url, "(?<=ned-eredivisie-).{4}(?=-)"
  )
  
  return(as.numeric(start_season))
}


# create vector of urls
start_years <- 1956:2023
urls <- sprintf(
  'https://www.voetbal.com/wedstrijdgegevens/ned-eredivisie-%s-%s-spieltag/34/', 
  start_years, 
  start_years + 1
)

urls[60]
url <- urls[60]

urls[1]

#
scrape_gamestats <- function(url){
  
  # extract the 4th table from the website
  tables <- rvest::read_html(url) %>%
    rvest::html_table()
  
  # from the multiple tables collected on the website
  # extract the table with the most rows
  idx <- which.max(sapply(tables, nrow))
  table <- tables[[idx]]
  
  # remove cols that only have Na's
  table <- table[, colSums(is.na(table))<nrow(table)]
  
  # one table column is like
  # 88:23, extract those pair of numbers
  # separately (88 and 23) using regexes
  own_goals <- as.numeric(
    stringr::str_extract(table$Dp., "(\\d*)")
  )
  
  goals_against <- as.numeric(
    stringr::str_extract(table$Dp., "(?<=:).*")
  )
  
  # from the url we extract the start of season
  season_start <- get_year_from_url(url)
  
  # because of the changing pointing system
  # sometimes points are reported in the format 
  # 54:14, if so, get the first two numbers
  if(typeof(table$P.)=="character"){
    made_points <- as.numeric(stringr::str_extract(table$P., "(\\d*)"))
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

scrape_gamestats(urls[1])
