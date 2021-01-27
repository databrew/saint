#' Update data AWS
#' 
#' Write data to AWS
#' @param s3_csv_path Path to an S3 creds csv
#' @param df A data frame
#' @param creds A credentials yaml path
#' @import aws.s3
#' @import readr
#' @import yaml
#' @return Data is read
#' @export

update_data_aws <- function(s3_csv_path,
                         df, creds) {
  
  # Read in credentials
  s3_creds <- read_csv(s3_csv_path)
  creds <- yaml.load_file(creds)
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = s3_creds$`Access key ID`,
    "AWS_SECRET_ACCESS_KEY" = s3_creds$`Secret access key`,
    "AWS_DEFAULT_REGION" = "eu-west-3"
  )
  
  if(!is.null(df)){
    already_done <- df$instanceID
  } else {
    already_done <- NULL
  }

  saint1 <- 
    odk_get_data(url = 'https://bohemia.systems',
                 id = 'saintperu',
                 id2 = 'data',
                 unknown_id2 = FALSE,
                 uuids = NULL,
                 exclude_uuids = already_done,
                 user = creds$databrew_odk_user,
                 password = creds$databrew_odk_pass,
                 widen = TRUE,
                 pre_auth = FALSE,
                 use_data_id = FALSE, 
                 sleeper = 0.25,
                 use_progress_bar = TRUE,
                 chunk_size = 1000)
  
  saint2 <- 
    odk_get_data(url = 'https://bohemia.systems',
                 id = 'saintperu2',
                 unknown_id2 = TRUE,
                 uuids = NULL,
                 exclude_uuids = already_done,
                 user = creds$databrew_odk_user,
                 password = creds$databrew_odk_pass,
                 widen = TRUE,
                 pre_auth = FALSE,
                 use_data_id = FALSE, 
                 sleeper = 0.25,
                 use_progress_bar = TRUE,
                 chunk_size = 1000)
  
  new_data <-
    bind_rows(saint1$non_repeats,
              saint2$non_repeats)
  df <- bind_rows(
    df, new_data
  )
  df <- df %>% 
    dplyr::distinct(.keep_all = TRUE)
  # write_data_aws(s3_csv_path = s3_csv_path,
  #                df = df)
  return(df)
}
