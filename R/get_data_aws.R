#' Get data AWS
#' 
#' Retrieve data from AWS
#' @param s3_csv_path Path to an S3 creds csv
#' @import aws.s3
#' @import readr
#' @return Data is read
#' @export

get_data_aws <- function(s3_csv_path) {
  message('Loading data from AWS')
  
  # Read in credentials
  s3_creds <- read_csv(s3_csv_path)
  
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = s3_creds$`Access key ID`,
    "AWS_SECRET_ACCESS_KEY" = s3_creds$`Secret access key`,
    "AWS_DEFAULT_REGION" = "eu-west-3"
  )
  
  # Read
  tempfile <- tempfile()  
  save_object(object = "s3://saintperu/df.RData", file = tempfile)
  load(tempfile, envir = .GlobalEnv)
  message('---Successfully loaded data from AWS')
}
