merge_dataframes <- function(data) {
  result = Reduce(function(x,y) merge(x,y, by='id'), data) # Using Reduce for merge all dfs by id
  return(result)
}


get_id <- function(data) {
  merged = merge_dataframes(data) # all dfs in one
  means_temp <- rowMeans(merged[-which(colnames(merged)=='id')]) # get mean
  result <- data.frame(id = merged$id, mean = means_temp)
  return(result)
}



file_name <- "C:\\Users\\Admin\\Desktop\\R-hw\\first\\data.RData"
load(file_name)
result <- get_id(data)
print(result)
