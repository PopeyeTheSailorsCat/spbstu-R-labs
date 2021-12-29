# Title     : TODO
# Objective : TODO
# Created by: Admin
# Created on: 27.12.2021

library(tidyverse)
library(ggplot2)

library(reshape2)


load("ExpImp.RData")
data <- as_tibble(ExpImp)
data <- data  %>% mutate_all(funs(gsub("-", "0", .)))
data <- data %>% rename(region = 'Регион' )
data <- data  %>% mutate_all(coalesce, "0") %>% filter(region != "в том числе:" )
data[,2:13] <- sapply(data[,2:13], as.double)

# print(data)
# data$sum <- rowSums(as.numeric(data[,2:3]))
export <- data %>% select(contains('Экспорт')) %>% # 'select' function to choose the columns you want
    mutate(row_sum = rowSums(.)) # 'mutate' function to create
import <- data %>% select(contains('Импорт')) %>% # 'select' function to choose the columns you want
    mutate(row_sum = rowSums(.)) # 'mutate' function to create

data$export <- export$row_sum
data$import <- import$row_sum


semi_data <-data[1:10,]
dfm <- melt(semi_data[,c("region",'export','import')], id.vars = 1)

ggplot(dfm, aes(x = region, y = value)) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  scale_y_log10()+xlab("Регион")+ylab("log(Значений)")

semi_data <-data[3:10,]
semi_data$export <- -1 * semi_data$export
dfm <- melt(semi_data[,c("region",'export','import')], id.vars = 1)
ggplot(dfm, aes(x = region, y = value)) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  xlab("Регион")+ylab("Значения")+
  geom_hline(aes(yintercept = 0))+
  geom_text(aes(label=value), vjust=1,hjust=1, color="black", size=3)