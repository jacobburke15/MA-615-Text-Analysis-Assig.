###### chapter 2 ######
## Peter Pan
## I used an additional novel titled  MARGARET OGILVY, and this is also written by Barrie.
library(knitr)
library(tidyverse)
library(textdata)
library(gutenbergr)

## ------------------------------------------------------------------------
## 2.1 The Sentiments dataset
library(tidytext)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

## ----tidy_books----------------------------------------------------------
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>% 
  ungroup() %>%
  unnest_tokens(word, text)
tidy_books
## --------------------------------
peterpan <- gutenberg_download(16)
peterpan <- peterpan[-(12:46),]

margaret <- gutenberg_download(342)

barrie <- rbind(peterpan, margaret)
## ------------------------------------------------------------------------
## 2.2 Sentiment analysis with inner join

# barrie is the author of Peter Pan.
# margaret is an another novel written by barrie.

### Create columns for linenumber and chapter
### peterpan
b1 <- peterpan %>% 
  group_by(gutenberg_id) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE))))

### Create columns for linenumber and chapter
### margaret
b2 <- margaret %>% group_by(gutenberg_id) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE))))

rbind(b1, b2)
b1_b2 <- rbind(b1, b2)
bb <- b1_b2 %>% ungroup() %>%
  unnest_tokens(word, text)

### sentiment analysis with 'nrc'
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# peterpan
bb %>%
  filter(gutenberg_id == "16") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# margaret
bb %>%
  filter(gutenberg_id == "342") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

### sentiment analysis with 'bing'
library(tidyr)
library(ggplot2)

barrie_sentiment <- bb %>%
  inner_join(get_sentiments("bing")) %>%
  count(gutenberg_id, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(barrie_sentiment, aes(index, sentiment, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free_x")
## ------------------------------------------------------------------------
## 2.3 Comparing the three sentiment dictionaries

peterpan2.3 <- bb %>% 
  filter(gutenberg_id == 16)

peterpan2.3

margaret2.3 <- bb %>% 
  filter(gutenberg_id == 342)

margaret2.3

### ----------------------
afinn <- peterpan2.3 %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(peterpan2.3 %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          peterpan2.3 %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Figure 2.3: Comparing three sentiment lexicons using Peter Pan.
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Let’s look briefly at how many positive and negative words are in these lexicons.
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% count(sentiment)

get_sentiments("bing") %>% count(sentiment)

## ------------------------------------------------------------------------
## 2.4 Most common positive and negative words

bing_word_counts <- bb %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

## -----------------------------------------
# Figure 2.4: Words that contribute to positive and negative sentiment in Barrie’s novels
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Figure 2.4 lets us spot an anomaly in the sentiment analysis; the word “pan” is coded as negative
# but it is used as a title for Peter Pan in Barrie’s works. 
# If it were appropriate for our purposes, we could easily add “pan” to a custom stop-words list using bind_rows().
# We could implement that with a strategy such as this.

custom_stop_words <- bind_rows(tibble(word = c("pan"), lexicon = c("custom")), stop_words)

custom_stop_words

## ------------------------------------------------------------------------
## 2.5 Wordclouds

# Figure 2.5: The most common words in Barrie’s novels
library(wordcloud)

bb %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Figure 2.6: Most common positive and negative words in Barrie’s novels
library(reshape2)

bb %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
## ------------------------------------------------------------------------
## 2.6 Looking at units beyond just words

PandP_sentences <- tibble(text = peterpan$text) %>% 
  unnest_tokens(sentence, text, token = "sentences")

PandP_sentences$sentence[4]

## ---------------------------------------
barrie_chapters <- barrie %>%
  group_by(gutenberg_id) %>%
  unnest_tokens(chapter, text, token = "regex", pattern = "Chapter|CHAPTER [\\dIVXLC]") %>% 
  ungroup()

barrie_chapters %>% 
  group_by(gutenberg_id) %>% 
  summarise(chapters = n())

## --------------------------------------- 
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- bb %>%
  group_by(gutenberg_id, chapter) %>%
  summarize(words = n())

bb %>%
  semi_join(bingnegative) %>%
  group_by(gutenberg_id, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("gutenberg_id", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()