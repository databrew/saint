#' @import dplyr
#' @import readr
#' @import lubridate
get_data <- function(data_file = 'data.csv',
                     user = '',
                     password = '') {
  # Get list of current data
  if(file.exists(data_file)){
    data <- read_csv(data_file)
    data$end_time <- as.POSIXct(data$end_time, tz = 'Europe/Paris')
    data$end_time <- as.character(data$end_time)
  } else {
    data <- tibble(instanceID = '')
  }
  # Define which uuids to exclude (because they've already been retrieved)
  exclude_these <- data$instanceID
  exclude_these <- exclude_these[!is.na(exclude_these)]
  
  # Retrieve ODK data
  df <- odk_get_data(url = 'https://bohemia.systems', 
                     id = 'saint', 
                     user = user, 
                     password = password,
                     exclude_uuids = exclude_these)
  if(!is.null(df)){
    df <- df$non_repeats
    message('---Data updated. ', nrow(df), ' new rows. Will combine with the ', nrow(data), ' already existing rows.')
    # Combine the old data with the new data
    # combined <- bind_rows(data, df)
    combined <- bind_rows(mutate_all(data, as.character), mutate_all(df, as.character))

    # Fix the end_time field
    combined$end_time <- substr(gsub('T', ' ', as.character(combined$end_time)), 1, 19)
    
    message('---Writing csv with updated data to ', data_file)
    # save(combined, file = '/tmp/tmp.RData')
    write_csv(combined, data_file)
  } else {
    combined <- data
  }
  # combined$end_time <- as.POSIXct(combined$end_time) - lubridate::hours(2)
  attr(combined$end_time, 'tzone') <- 'Europe/Paris'
  
  return(combined)
}
