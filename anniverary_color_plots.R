library(magrittr)
library(stringr)
library(data.table)
library(ggplot2)
library(betterviz)

sched <- GetColorSchedule()

words <- sched$desc %>%
  paste(collapse = " ") %>%
  str_to_lower() %>%
  str_split(" ") %>%
  unlist() %>%
  VectorSource() %>%
  Corpus() %>%
  tm_map(removePunctuation) %>%
  tm_map(function(x)removeWords(x,stopwords()))


tdm <- TermDocumentMatrix(words)
m <- as.matrix(tdm)
v <- sort(rowSums(m))
d <- data.table(word = names(v),freq=v)

colors <- c("blue","red","white","green","orange","purple","yellow","gold","pink","pastel","burgundy","magenta")

d.colors <- d[word %in% colors]

d.colors[, word := factor(word, levels = colors, ordered = T)]

d.ideas <- d[!word %in% c(colors,c("honor","day"))]

wordcloud(d.ideas$word, d.ideas$freq)

ggplot(d.colors, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", aes(fill = word)) +
  geom_text(aes(label = freq), nudge_y = 1) +
  coord_flip() +
  scale_fill_manual(values = c("blue","red","lightgrey","darkgreen","orange","purple","yellow","gold","pink","green","#8C001A","#FF00FF"), guide = F) +
  labs(x = "", y = "Days color appeared (May '15 - May '16)", title = "Empire State Building color frequency\nby @ESBcolor") +
  theme_betterment()
