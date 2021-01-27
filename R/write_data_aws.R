#' Write data AWS
#' 
#' Write data to AWS
#' @param s3_csv_path Path to an S3 creds csv
#' @param df A data frame
#' @import aws.s3
#' @import readr
#' @return Data is read
#' @export

write_data_aws <- function(s3_csv_path,
                         df) {
  
  message('Writing data to AWS')
  
  # Read in credentials
  s3_creds <- read_csv(s3_csv_path)
  
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = s3_creds$`Access key ID`,
    "AWS_SECRET_ACCESS_KEY" = s3_creds$`Secret access key`,
    "AWS_DEFAULT_REGION" = "eu-west-3"
  )
  
  # Save as an R object
  tempfile <- tempfile()
  save(df, file = tempfile)
  
  # Put in bucket
  put_object(
    file = tempfile,
    object = 'df.RData',
    bucket = 'saintperu'
  )
  
  # # Read
  # save_object(object = "s3://saintperu/df.RData", file = tempfile)
  # load(tempfile, envir = .GlobalEnv)
  message('---Successfully wrote data to AWS')
  
}
