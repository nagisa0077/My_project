library(ggplot2)
library(wordcloud)
library(tm)
library(jiebaR)
library(stringr)
library(dplyr)

# 數據匯入與清理
data <- read.csv("foodpanda.csv",F)
data <- data[,-c(1,3)]
data <- data[which(nchar(data)>10)] #刪除評論小於10的無效留言

# 文字雲
text <- c(data)
## 中文分詞
segmenter <- worker()
segmented_text <- segment(text, segmenter)
segmented_text <- unlist(segmented_text)

## 建立詞頻表
text_corpus <- Corpus(VectorSource(segmented_text))
text_dtm <- TermDocumentMatrix(text_corpus)
text_matrix <- as.matrix(text_dtm)
word_freqs <- sort(rowSums(text_matrix), decreasing = TRUE)
word_freqs <- data.frame(word = names(word_freqs), freq = word_freqs)

word_freqs <- word_freqs %>%
  filter(nchar(word) > 2 | word == "錢")
wordcloud(words = word_freqs$word, freq = word_freqs$freq, min.freq = 1, scale = c(3, 0.5), 
          colors = brewer.pal(8, "Dark2"), random.order = FALSE)

# 關鍵字計算
family <- c("家","孩","父母","爸","媽")
money <- c("薪水","萬","房","款","錢","收入","存")
self <- c("時間","健康","自由","夢想","生活","創業","成就感")
environment <- c("疫情","討生活","轉行","機會")


family_count <- sum(sapply(data, function(comment) {
  any(str_detect(comment, family))
}))
money_count <- sum(sapply(data, function(comment) {
  any(str_detect(comment, money))
}))
self_count <- sum(sapply(data, function(comment) {
  any(str_detect(comment, self))
}))
environment_count <- sum(sapply(data, function(comment) {
  any(str_detect(comment, environment))
}))

result  <- data.frame(
  reason = c( "Money", "Self-actualization","Family", "Macro environment"),
  count = c(money_count, self_count, family_count, environment_count))

ggplot(result, aes(x = reason, y = count,fill = reason)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  labs(title = "Comment_Count", x = "Reason", y = "Comment_Count") +
  theme_minimal()
