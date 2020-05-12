#' @import dplyr
#' @import readr
get_data <- function(data_file = 'data.csv',
                     user = '',
                     password = '') {
  # Get list of current data
  if(file.exists(data_file)){
    data <- read_csv(data_file)
  } else {
    data <- tibble(instanceID = '')
  }
  # Define which uuids to exclude (because they've already been retrieved)
  exclude_these <- data$instanceID
  
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

    message('---Writing csv with updated data to ', data_file)
    write_csv(combined, data_file)
  } else {
    combined <- data
  }
  return(combined)
}