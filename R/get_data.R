#' Get data
#' 
#' Retrieve data
#' @param data_file The data file
#' @param user The user
#' @param password The password
#' @param form_id The form id
#' @import dplyr
#' @import readr
#' @import lubridate
#' @return Data is read
#' @export
get_data <- function(data_file = 'data.csv',
                     user = '',
                     password = '',
                     form_id = 'saint') {
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
  
  if(form_id == 'saintperu'){
    id2 = 'data'
  } else {
    id2 = NULL
  }
  # Retrieve ODK data
  df <- odk_get_data(url = 'https://bohemia.systems', 
                     id = form_id, 
                     id2 = id2,
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
  
  # Manual correction
  if('uuid:9d2dd549-74c3-4088-8051-8767a7cc1bb4' %in% combined$instanceID){
    combined$fecha[combined$instanceID == 'uuid:9d2dd549-74c3-4088-8051-8767a7cc1bb4'] <- '2020-08-17'
  }
  
  return(combined)
}
