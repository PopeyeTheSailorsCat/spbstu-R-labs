fix_column <- function(column){
  new_column <- as.numeric(step<-sub(' ', '', column))
  if (any(is.na(new_column))) {
    return(column)
  }else{
    return(new_column)
  }
}

fix_data <- function(data){
  fixed_df <-data.frame(lapply(data, fix_column))
  return(fixed_df)
}

file_name <- "C:/Users/Admin/Desktop/R-hw/first-lab/test_data_01_(1).csv" # For your file location
get_data <- read.csv(file_name)
fix_data(get_data)

