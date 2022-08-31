complete <- function(directory, id =1:332) {
  sapply(c('data.table', 'readr', 'tidyverse'), require, character.only = TRUE)
  ##loads required packages
   names <- list.files(paste(directory,"/", sep = ""), 
                      full.names = TRUE) [id]
  ##list the files in the directory
  readc <- lapply(names, read_csv, col_types = list(
    col_date(),
    col_double(),
    col_double(),
    col_integer()
  )) 
  ##makes a vector of all the files
  ##read.csv: reads csv files
   df_readc <-data.table::rbindlist(readc, fill = TRUE)
  ##convert readc (list) to data.table (different of rows, so can't use
  ##data.frame)
  new_tibble <-as_tibble(df_readc)
  ##convert readc (list) to a tibble
  final_tibble <- new_tibble %>%
    na.omit () %>%
    group_by(ID) %>%
    ##perform operations based on ID
    summarise(nobs = n())
  ##give sum of observations that have a value
  return(final_tibble)
}

