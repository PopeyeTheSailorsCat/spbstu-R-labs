# Title     : TODO
# Objective : TODO
# Created by: Admin
# Created on: 27.12.2021
library(tidyverse)
library(RColorBrewer)

file <- "data-forest.csv"
data <- as_tibble(read.csv(file,sep=";"))
data$naimenovanie_pokazatelya <- enc2utf8(data$naimenovanie_pokazatelya)
line  <-  as_tibble("Площадь земель, занятых лесными насаждениями (покрытых лесной растительностью), всего")
res <- filter(data, naimenovanie_pokazatelya %in% line)
print(res)



library(ggplot2)

# Create Data
newValues <- gsub(",", ".", res$znachenie_pokazatelya)
numericValues <- as.numeric(newValues)
data <- data.frame(
  name=res$preobladayushchie_drevesnye_i_kustarnikovye_porody,
  value = numericValues
)
print(data)

ggplot(data, aes(x="", y=value, fill=name)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() #



data <- data %>%
  arrange(desc(name)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(data, aes(x="", y=prop, fill=name)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none") +

  geom_text(aes(y = ypos, label = value), color = "white", size=6)


#
#
# # # create dummy data
# data <- data.frame(
#   name=data$group,
#   value=data
# )

# The most basic barplot you can do:
# Specific color for each bar? Use a well known palette
p<-ggplot(data=data, aes(x=name, y=value, fill = name )) +
  geom_bar(stat="identity")+theme_minimal()
p

# Horizontal bar plot
p + coord_flip()

colourCount <- length(unique(data$name))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
p+scale_fill_manual(values = getPalette(colourCount))
