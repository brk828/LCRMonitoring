# M&A useful functions

# Function to measure euclidean distance between UTMs (p1E, q1N) and (p2E, q2E)
euclid <- function(p1, q1, p2, q2) {sqrt((p2 - p1) ^2 + (q2 - q1) ^2)}

# function to install or load libraries depending on availability
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

split_hourly <- function(df, id_col, start_col, end_col) {
  
  # split_hourly takes records that include start and end date-time fields
  # and splits them into hourly records for each whole hour between the start and 
  # end date-times.
  packages(dplyr)
  packages(lubridate)
  
  id_col_string <- deparse(substitute(id_col))
  start_col <- deparse(substitute(start_col))
  end_col <- deparse(substitute(end_col))
  
  unique_ids <- unique(df[[id_col_string]])
  expanded_df <- data.frame()
  
  for (i in unique_ids) {
    sub_df <- df[df[[id_col_string]] == i,]
    
    start_hour <- floor_date(sub_df[[start_col]], unit = "hour") + hours(1)
    end_hour <- floor_date(sub_df[[end_col]], unit = "hour")- hours(1)
    
    
    intervals <- seq.POSIXt(from = start_hour,
                            to = end_hour,
                            by = "hour")
    
    # Create a data frame with one row for each hour interval
    temp_df <- data.frame(IntervalDate = as.Date(intervals),
                          IntervalStart = intervals,
                          IntervalEnd = intervals + minutes(59) + seconds(59),
                          IntervalHour = hour(intervals))
    
    temp_df <- cbind(sub_df, temp_df)
    
    expanded_df <- rbind(expanded_df, temp_df)
    
    
  }
  return(expanded_df)
}

# function to download Ncreased Basin scanning tables from website
download_basin <- function(sub_folder = ""){
  NcreasedUrl<-"https://nativefishlab.net/BasinScanningIndex.RData"
  dest_folder = ifelse(sub_folder == "", "", paste0(sub_folder, "/"))
  download.file(NcreasedUrl, destfile = paste0(dest_folder, "BasinScanningIndex.RData"),cacheOK=TRUE, 
                extra = options(timeout = 200)) 
}

# function to download Ncreased backwater tables from website
download_backwater <- function(sub_folder = ""){
  NcreasedUrl<-"https://nativefishlab.net/BWScanningIndex.RData"
  dest_folder = ifelse(sub_folder == "", "", paste0(sub_folder, "/"))
  download.file(NcreasedUrl, destfile = paste0(dest_folder, "BWScanningIndex.RData"),cacheOK=TRUE, 
                extra = options(timeout = 200)) 
}

# function to download NGWG database table from website
download_nfwg <- function(sub_folder = ""){
  NcreasedUrl<-"https://nativefishlab.net/NFWGTable.RData"
  dest_folder = ifelse(sub_folder == "", "", paste0(sub_folder, "/"))
  download.file(NcreasedUrl, destfile = paste0(dest_folder, "NFWGTable.RData"),cacheOK=TRUE, 
                extra = options(timeout = 200)) 
}

