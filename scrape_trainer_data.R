library(rvest)
library(lubridate)


# get all trainer data from a voetbal.com url
# assure the data is in the correct format
scrape_trainer_data <- function(url){
  
  # we assume that the first table on the website
  # is the table of interest
  # get the first table from the website
  table_trainer_data <- rvest::read_html(url) %>%
    rvest::html_table(header = TRUE) %>%
    .[[1]] # 1st table
  
  # convert character to date
  birth_years <- 
    lubridate::dmy(table_trainer_data$geb.)
  
  # outputs a list of start and end dates
  start_end_dates <- table_trainer_data$Periode %>%
    stringr::str_split(pattern = " - ") 
  
  # for convenience, create new columns
  # sapply gets the first of second elements of a list
  # '1' to get start dates, '2' for end dates
  start_date <- lubridate::dmy(sapply(start_end_dates, '[[', 1))
  end_date <- lubridate::dmy(sapply(start_end_dates, '[[', 2))
  
  # combine all data in a tibble df
  # to assure informative printing
  trainer_data <- tibble::tibble(
    start_date = start_date,
    end_date = end_date,
    days_in_service = end_date - start_date,
    trainer = table_trainer_data$Trainer,
    birthyear = birth_years,
    age_in_years = as.numeric(
      lubridate::today() - birthyear
    ) / 365.25
  )
  return(trainer_data)
}


# give a url that links to trainer data for a specific team
url <- "https://www.voetbal.com/teams/feyenoord/9/"
data_feynoord <- scrape_trainer_data(url)

# examine data
print(data_feynoord)

# or from another team:
url <- "https://www.voetbal.com/teams/afc-ajax/9/"
data_ajax <- scrape_trainer_data(url)
print(data_ajax)

# longest serving trainer for ajax?
data_ajax %>% 
  dplyr::arrange(desc(days_in_service))