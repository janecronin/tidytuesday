library(appa)
library(tidytext)
library(dplyr)
library(ggplot2)

data <- appa::appa

# tidy and tokenize

tidy_appa <- data %>% unnest_tokens(word, full_text)
data("stop_words")
tidy_appa <- tidy_appa %>% anti_join(stop_words)

# tf-idf

total_words <- tidy_appa %>% count(book_num, word, sort = TRUE) %>% ungroup()
sum_words <- total_words %>% group_by(book_num) %>% summarize(total = sum(n))
chapter_words <- left_join(total_words, sum_words)

chapter_idf <- chapter_words %>% bind_tf_idf(word, book_num, n)
chapter_idf %>% select(-total) %>% arrange(desc(tf_idf))
chapter_idf %>% arrange(desc(tf_idf))%>% 
  mutate(word = factor(word, levels = rev(unique(word))))    %>%
  group_by(book_num) %>% 
  top_n(20) %>% 
  ungroup %>% 
  ggplot(aes(word, tf_idf, fill = book_num)) + 
  geom_col(show.legend = FALSE) + 
  labs(x = NULL, y = "tf-idf") + 
  facet_wrap(~book_num, ncol = 2, scales = "free_y") + 
  coord_flip()

#tf-idf - character

ch_words <- tidy_appa %>% count(character, word, sort = TRUE) %>% ungroup()
ch_sum_words <- ch_words %>% group_by(character) %>% summarize(total = sum(n))
ch_words <- left_join(ch_words, ch_sum_words) %>% filter(character == "Aang" | character == "Katara" | character == "Sokka" | character == "Zuko" | character == "Toph") 


ch_idf <- ch_words %>% bind_tf_idf(word, character, n)
ch_idf %>% select(-total) %>% arrange(desc(tf_idf))
ch_idf %>% arrange(desc(tf_idf))%>% 
  group_by(character) %>% 
  top_n(20) %>% 
  ungroup %>% 
  mutate(character = as.factor(character), word = reorder_within(word, tf_idf, character)) %>%
  ggplot(aes(word, tf_idf, fill = character)) + 
  geom_col(show.legend = FALSE) + 
  labs(x = NULL, y = "tf-idf") + 
  scale_x_reordered() +
  facet_wrap(~character, ncol = 2, scales = "free_y") + 
  coord_flip() + 
  ggtitle("The Last Air-Bender: Characters' Most Distinctive Words") +
  theme_light() +
  scale_fill_brewer(palette = "Pastel1", direction = -1, type = "qual")
