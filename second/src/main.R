# Title     : TODO
# Objective : TODO
# Created by: Admin
# Created on: 05.12.2021

get_max_avg_week <-function (data){
  max_temp <- -220
  max_week <- -1
  week_counter <- 0
  t_counter <-0
  for (row in seq_len(nrow(data))) {
    day_week <- weekdays(as.Date(data[row, "YYYYMMDD"]))
    t_counter <- t_counter + data[row, "T2M"]
    if (day_week == "понедельник" ){
      if (max_week == -1){
        max_week <- 1
      }else{
        if (max_temp < t_counter/7){
          max_week <- week_counter
          max_temp <- t_counter/7
        }
        t_counter <- 0
      }
      week_counter <- week_counter + 1
    }
  }
  return(max_week)
}

file <- "RH_T.csv"
data <- read.csv(file)
get_max_avg_week(data)