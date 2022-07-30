library(rvest)
library(stringr)


# fun to find scrapeable years for a website like
# "https://www.voetbal.com/teams/feyenoord/2011/6/"
find_scrapable_years <- function(html_doc) {
  
  # find the html form that gives the option to
  # select multiple years of soccer data
  # e.g. 2013-2014
  string_years <- rvest::html_element(html_doc, "#site select") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("[:alpha:]|[:punct:]")
  
  # above delivers one lengthy string with years '202320222021' etc.
  # convert to numeric by extracting each 4 chars
  years_numeric <- stringr::str_extract_all(
    string_years, pattern = "(.{4})", simplify = FALSE) %>%
    .[[1]] %>% as.numeric()
  
  return(unique(sort(years_numeric, decreasing = FALSE)))
}


# function that scraped the transfer table from a url 
get_transfers <- function(url) {
  
  # parse the scraped html document
  html_doc <- rvest::read_html(url)
  
  # extract all tables from the html file
  data <- rvest::html_elements(html_doc, "table") %>%
    rvest::html_table() %>%
    .[[2]] # we are only interested by the 2nd table
  
  cat("Scraped the website", url, "\n")
  
  return(data)
}


# function below finds all scrapable years from the 
# voetbal.com/teams/feynoord website
# and returns all the made transfers 
scrape_all_transfer_years <- function(
    url = "https://www.voetbal.com/teams/feyenoord/2016/6/",
    club_name = "feyenoord"
  ){
  
  html_doc <- rvest::read_html(url)

  # from the website collect all 
  range_years <- find_scrapable_years(html_doc)
  
  # make vector of all urls
  urls <- sprintf(
    "http://www.voetbal.com/teams/%s/%s/6/",
    club_name,
    range_years
  )
  
  # extract all transfer data from each url
  # the result is a list of dataframes
  data_list <- lapply(urls, get_transfers)

  return(data_list)
}


# use the main function to scrape all data from the url:
# clubname must be in line with the name in url
# will automate that soon
data_feynoord <- scrape_all_transfer_years(
  url = "https://www.voetbal.com/teams/feyenoord/2016/6/",
  club_name = "feynoord"
)

# the data is a list of dataset
# e.g. to get the latest data (2024)
data_feynoord[110]

# to get data from 2023, etc.
data_feynoord[109]

# or from another team:
data_ajax <- scrape_all_transfer_years(
  url = "https://www.voetbal.com/teams/afc-ajax/2023/6/",
  club_name = "afc-ajax"
)

# latest data
data_ajax[119]

# noteice that there arent transfers in 2024
data_ajax[120]

# other data must be scrapable in similar fashion