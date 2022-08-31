corr <- function(directory, threshold = 0) {
  sapply(c('dplyr', 'purrr', 'readr', 'tidyr'), require, character.only = TRUE)
  ##loads required packages
  files <- list.files(directory, full.names = TRUE)
  ##list files of interest
  readc <- map_df(files, read_csv,
                  col_types = list(
                    col_date(),
                    col_double(),
                    col_double(),
                    col_integer()
                  )) %>% na.omit()
  ##read files into readc and creates a data.frame and omits NA values
  readc_counts <- (readc) %>%
      group_by(ID) %>%
      count() %>%
      filter(n > threshold)
  ##performs operation based on ID, counts number of occurences based on ID,
  ##filters results based on threshold set in function
  if (nrow(readc_counts) < 1) {
    return(numeric(0))
    ##if the results are less than 1, then we will get 0 back
     }
  correlation <- readc_counts %>%
    inner_join(readc, by = "ID") %>%
    ##return rows when matching condition is met, 
    ## what is common between datasets
    nest (data = -ID) %>%
    ##forms list of data frames with all the nested variables (ID)
    mutate(correlation = map(data, ~cor(.x$sulfate, .x$nitrate))) %>%
    ##adds the correlation variable to the dataframe
    unnest(correlation)
    ##creates tibble of results
    options(digits = 10)
    ##display results with 10 digits to reduce rounding error
    return(correlation)
    ##return the results
  }

